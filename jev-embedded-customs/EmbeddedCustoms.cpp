#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassInstrumentation.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Linker/Linker.h"
#include "llvm/Object/Archive.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <filesystem>

#include <fmt/format.h>

#define rel_assert(__e)                                                        \
  ((__e) ? (void)0                                                             \
         : release_assert_fail(__FILE__, __LINE__, __PRETTY_FUNCTION__, #__e))

using namespace llvm;

static cl::opt<std::string>
    exported_syms_file("embcust-exported-syms",
                       cl::desc("file with list of exported symbols"),
                       cl::Required);

static cl::opt<std::string>
    libgcc("embcust-libgcc", cl::desc("libgcc/compiler-rt lto library to link"),
           cl::Required);

template <> struct fmt::formatter<StringRef> : formatter<string_view> {
  // parse is inherited from formatter<string_view>.
  template <typename FormatContext>
  auto format(StringRef StrRef, FormatContext &ctx) {
    return formatter<string_view>::format(StrRef.str(), ctx);
  }
};

namespace jev {

struct PassOpts {
  bool Pre;
  bool Post;
  bool ShouldRenameCtorsArray;
  std::string EntrypointWrapper;

  PassOpts() = default;
};

static const char *libcallRoutineNames[] = {
#define HANDLE_LIBCALL(code, name) name,
#include "llvm/IR/RuntimeLibcalls.def"
#undef HANDLE_LIBCALL
};

static std::set<std::string> bannedLibcallRoutineNames{
    "__gcc_personality_sj0", "__gcc_personality_v0", "__gcc_personality_imp",
    "readEncodedPointer", // for gcc perso
};

static std::set<std::string> usedNames{
    // "abort", // called by compiler-rt that's linked in later
};

static std::set<std::string> getLibcallRoutines() {
  std::set<std::string> res;
  for (const auto name : libcallRoutineNames) {
    if (name)
      res.insert(name);
  }
  std::erase_if(res, [](const auto &name) {
    return bannedLibcallRoutineNames.contains(name);
  });
  return res;
}

static const auto libcallRoutines{getLibcallRoutines()};

static void EoE(Error &&E) {
  std::string str;
  llvm::raw_string_ostream ostr(str);
  ostr << E;
  report_fatal_error(StringRef{str});
}

static void release_assert_fail(const char *file, int line, const char *func,
                                const char *expr) {
  std::filesystem::path f{file};

  report_fatal_error(StringRef{fmt::format(
      "{:s}:{:d}\n\n{:s}:{:d} {:s} condition failed ->\n\trel_assert({:s})\n",
      file, line, f.filename().string(), line, func, expr)});
}

static std::vector<std::string> getLines(StringRef path) {
  std::vector<std::string> ret;
  SmallVector<StringRef, 0> arr;
  auto mb = MemoryBuffer::getFile(path);
  if (!mb) {
    return ret;
  }
  (*mb)->getBuffer().split(arr, '\n');

  for (StringRef s : arr) {
    s = s.trim();
    if (!s.empty() && s[0] != '#')
      ret.push_back(s.str());
  }
  return ret;
}

static void dump_module(Module &M, StringRef path) {
  std::error_code EC;
  raw_fd_ostream f{path, EC};
  if (EC) {
    report_fatal_error("dump_module: failed to create file " + path);
  }
  WriteBitcodeToFile(M, f);
}

static std::unique_ptr<Module> load_bc_archive(StringRef ar_path,
                                               LLVMContext &Context) {
  fmt::print(stderr, "loading {:s}\n", ar_path.str());
  auto ar_bin = llvm::object::createBinary(ar_path);
  if (Error E = ar_bin.takeError())
    EoE(std::move(E));
  if (!ar_bin->getBinary()->isArchive()) {
    report_fatal_error("binary isn't an archive file");
  }
  const auto *ar = cast<const llvm::object::Archive>(ar_bin->getBinary());

  std::unique_ptr<Module> Result(new Module("ArchiveModule", Context));
  Linker L(*Result);

  Error Err = Error::success();
  for (const auto &child : ar->children(Err)) {
    if (Err)
      EoE(std::move(Err));
    auto name = child.getName();
    if (Error E = name.takeError())
      EoE(std::move(E));

    auto MemBuf = child.getMemoryBufferRef();
    if (Error E = MemBuf.takeError())
      EoE(std::move(E));

    if (identify_magic(MemBuf->getBuffer()) != llvm::file_magic::bitcode) {
      if (!name->startswith("outline_atomic_")) {
        errs() << "child " << *name << " isn't a bitcode file\n";
      }
      continue;
    }

    SMDiagnostic ParseErr;
    std::unique_ptr<Module> NewM{getLazyIRModule(
        MemoryBuffer::getMemBuffer(*MemBuf, false), ParseErr, Context)};
    if (!NewM) {
      report_fatal_error("failed to parse " + *name);
    }

    if (L.linkInModule(std::move(NewM))) {
      report_fatal_error("failed to link in " + *name);
    }
  }
  if (Err)
    EoE(std::move(Err));

  return Result;
}

static void link_bc_archive(Module &M, StringRef ar_path, bool only_needed) {
  std::unique_ptr<Module> NewM = load_bc_archive(ar_path, M.getContext());
  if (!NewM) {
    report_fatal_error("failed to load " + ar_path);
  }
  errs() << "dumping librt whole mod\n";
  dump_module(*NewM, "librt.bc");
  dump_module(M, "merd.bc");
  errs() << "begining librt ar whole link\n";
  Linker L(M);
  if (L.linkInModule(std::move(NewM),
                     only_needed ? Linker::Flags::LinkOnlyNeeded : 0)) {
    report_fatal_error("failed to link in " + ar_path);
  }
  dump_module(M, "merd-linked.bc");
}

static void add_undefs_for_syms(Module &M,
                                const std::set<std::string> &sym_names) {
  IRBuilder<> IRB(M.getContext());
  FunctionType *FnTy = FunctionType::get(IRB.getVoidTy(), false);
  for (const auto &sym_name : sym_names) {
    M.getOrInsertFunction(sym_name, FnTy);
  }
}

static void remove_undefs_syms(Module &M,
                               const std::set<std::string> &sym_names) {
  for (const auto &sym_name : sym_names) {
    auto GV = M.getNamedValue(sym_name);
    if (GV && GV->isDeclaration()) {
      GV->eraseFromParent();
    }
  }
}

static void link_libgcc(Module &M, StringRef libgcc_path) {
  add_undefs_for_syms(M, libcallRoutines);
  link_bc_archive(M, libgcc_path, true);
  // remove_undefs_syms(M, libcallRoutines);
  for (const auto &bannedName : bannedLibcallRoutineNames) {
    auto GV = M.getNamedValue(bannedName);
    errs() << "looking for gv " << bannedName << "\n";
    if (GV) {
      errs() << "got gv " << GV->getName() << "\n";
      GV->eraseFromParent();
    }
  }
}

struct Structor {
  int Priority = 0;
  Constant *Func = nullptr;
  GlobalValue *ComdatKey = nullptr;

  Structor() = default;
};

static void preprocessXXStructorList(const DataLayout &DL, const Constant *List,
                                     SmallVector<Structor, 8> &Structors) {
  // Should be an array of '{ i32, void ()*, i8* }' structs.  The first value is
  // the init priority.
  if (!isa<ConstantArray>(List))
    return;

  // Gather the structors in a form that's convenient for sorting by priority.
  for (Value *O : cast<ConstantArray>(List)->operands()) {
    auto *CS = cast<ConstantStruct>(O);
    if (CS->getOperand(1)->isNullValue())
      break; // Found a null terminator, skip the rest.
    ConstantInt *Priority = dyn_cast<ConstantInt>(CS->getOperand(0));
    if (!Priority)
      continue; // Malformed.
    Structors.push_back(Structor());
    Structor &S = Structors.back();
    S.Priority = Priority->getLimitedValue(65535);
    S.Func = CS->getOperand(1);
    if (!CS->getOperand(2)->isNullValue()) {
      S.ComdatKey =
          dyn_cast<GlobalValue>(CS->getOperand(2)->stripPointerCasts());
    }
  }

  // Emit the function pointers in the target-specific order
  llvm::stable_sort(Structors, [](const Structor &L, const Structor &R) {
    return L.Priority < R.Priority;
  });
}

static bool rename_structor_array(const char *Array, const char *NewArray,
                                  const char *Section, Module &M) {
  IRBuilder<> IRB(M.getContext());

  const auto DL = M.getDataLayout();
  const GlobalVariable *GVCtor = M.getNamedGlobal(Array);
  fmt::print(stderr, "GVCtor: {:p}\n", fmt::ptr(GVCtor));
  if (!GVCtor)
    return false;

  const Constant *OrigInit = GVCtor->getInitializer();
  rel_assert(OrigInit);
  SmallVector<Structor, 8> Structors;
  preprocessXXStructorList(DL, OrigInit, Structors);
  for (const auto &ctor : Structors) {
    fmt::print(stderr, "ctor name: {:s}\n", ctor.Func->getName().str());
  }

#if 0
  FunctionType *FnTy = FunctionType::get(IRB.getVoidTy(), false);

  // Get the current set of static global structors
  std::vector<Constant *> CurrentCtors;
  StructType *EltTy = StructType::get(
      IRB.getInt32Ty(), PointerType::getUnqual(FnTy), IRB.getInt8PtrTy());
  if (GlobalVariable *GVCtor = M.getNamedGlobal(Array)) {
    if (Constant *Init = GVCtor->getInitializer()) {
      unsigned n = Init->getNumOperands();
      CurrentCtors.reserve(n);
      for (unsigned i = 0; i != n; ++i)
        CurrentCtors.push_back(cast<Constant>(Init->getOperand(i)));
    }
    GVCtor->eraseFromParent();
  }

  // Build a 3 field global_ctor entry.  We don't take a comdat key.
  Constant *CSVals[3];
  CSVals[0] = IRB.getInt32(Priority);
  CSVals[1] = F;
  CSVals[2] = Data ? ConstantExpr::getPointerCast(Data, IRB.getInt8PtrTy())
                   : Constant::getNullValue(IRB.getInt8PtrTy());
  Constant *RuntimeCtorInit =
      ConstantStruct::get(EltTy, makeArrayRef(CSVals, EltTy->getNumElements()));

  CurrentCtors.push_back(RuntimeCtorInit);

  // Create a new initializer.
  ArrayType *AT = ArrayType::get(EltTy, CurrentCtors.size());
  Constant *NewInit = ConstantArray::get(AT, CurrentCtors);

  // Create the new global variable and replace all uses of
  // the old global variable with the new one.
  (void)new GlobalVariable(M, NewInit->getType(), false,
                           GlobalValue::AppendingLinkage, NewInit, Array);
#endif

  return true;
}

static void renameCtorsDtorsArrays(Module &M) {
  errs() << "renaming ctor/dtor array\n";
  rename_structor_array("llvm.global_ctors", "llvm.global_ctors.embcust",
                        ".init_array_embcust", M);
  rename_structor_array("llvm.global_dtors", "llvm.global_dtors.embcust",
                        ".fini_array_embcust", M);
}

static void wrapEntrypoints(const std::string &EntrypointWrapperName,
                            Module &M) {
  errs() << "wrapping entry points with a call to " << EntrypointWrapperName
         << "()\n";
}

static void usedHackApply(Module &M) {
  GlobalVariable *GV = M.getGlobalVariable("llvm.used");
  if (!GV)
    return;
  rel_assert(GV->hasInitializer());
  const auto *CA = cast<ConstantArray>(GV->getInitializer());
  GV = new llvm::GlobalVariable(M, CA->getType(), true,
                                GlobalValue::ExternalLinkage,
                                GV->getInitializer(), "__embcust_llvm_used");
  // appendToUsed(M, ArrayRef{cast<GlobalValue>(GV)});
}

static void usedHackRemove(Module &M) {
  GlobalVariable *GV = M.getGlobalVariable("__embcust_llvm_used");
  fmt::print(stderr, "GV: {:p} name: {:s}\n", fmt::ptr(GV),
             GV ? GV->getName() : "(null)");
  if (!GV)
    return;
  GV->eraseFromParent();
}

static bool runEmbeddedCustoms(Module &M, const PassOpts &opts) {
  errs() << "running embcust " << (opts.Pre ? "pre" : "post") << "\n";
  if (opts.Pre) {
    link_libgcc(M, libgcc);
  }

  // get exported syms from newline seperated file (# as first char on line is a
  // comment)
  const auto exported_sym_names = getLines(exported_syms_file);
  const std::set<std::string> exported_syms{exported_sym_names.cbegin(),
                                            exported_sym_names.cend()};

  // symbols to not change visibility of
  static const std::set<std::string> vis_mod_skiplist{// "llvm.used",
                                                      "__embcust_llvm_used"};

  for (auto &GV : M.global_values()) {
    // if we find a (builtin? not sure) libcall, add it to used or else it may
    // get optimized out but another pass later adds a call
    bool isLibcall = libcallRoutines.contains(GV.getName().str());
    if (isLibcall || usedNames.contains(GV.getName().str())) {
      appendToUsed(M, ArrayRef{&GV});
    }

    if (
        // don't touch decls
        !GV.isDeclaration() &&
        // don't change visibility on symbols we want exported
        // note we don't force external visibility for these *optionally*
        // exported symbols
        !exported_syms.contains(GV.getName().str()) &&
        // don't change visibility of symbols in skip list
        !vis_mod_skiplist.contains(GV.getName().str()) &&
        // don't mess with llvm reserved globals, e.g. making llvm.used private
        // drops .init_array
        !GV.getName().startswith("llvm.")) {
      using LT = GlobalValue::LinkageTypes;

      // don't seem to need to change visibility, probably since i'm only
      // statically linking
      // func.setVisibility(GlobalValue::VisibilityTypes::HiddenVisibility);

      // downgrade linkage
      const auto old_linkage = GV.getLinkage();
      auto new_linkage = old_linkage;
      switch (old_linkage) {
      case LT::LinkOnceAnyLinkage:
      case LT::CommonLinkage:
      case LT::ExternalWeakLinkage:
      case LT::WeakAnyLinkage:
      case LT::ExternalLinkage:
      case LT::AvailableExternallyLinkage:
      case LT::LinkOnceODRLinkage:
      case LT::WeakODRLinkage:
      case LT::AppendingLinkage:
        // internal linkage is the "weakest" that still shows up in symbol
        // table (debugging)
        new_linkage = LT::InternalLinkage;
        break;
      case LT::InternalLinkage:
      case LT::PrivateLinkage:
        // do nothing, already "weakest" linkage
        break;
      }
      GV.setLinkage(new_linkage);
    }
  }

  if (opts.Pre) {
    usedHackApply(M);
  } else {
    usedHackRemove(M);
  }

  if (opts.ShouldRenameCtorsArray) {
    renameCtorsDtorsArrays(M);
  }

  if (!opts.EntrypointWrapper.empty()) {
    wrapEntrypoints(opts.EntrypointWrapper, M);
  }

  return true;
}

class EmbeddedCustomsPass : public PassInfoMixin<EmbeddedCustomsPass> {
  const PassOpts opts;

public:
  EmbeddedCustomsPass() : opts() {}
  EmbeddedCustomsPass(const PassOpts &opts) : opts(opts) {}

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    if (!runEmbeddedCustoms(M, opts))
      return PreservedAnalyses::all();
    return PreservedAnalyses::none();
  }
};

} // namespace jev

static jev::PassOpts parsePassOpts(StringRef opts_str) {
  jev::PassOpts opts{};

  StringRef opt_list{opts_str};
  opt_list.consume_front("embcust<");
  opt_list.consume_back(">");
  SmallVector<StringRef> opts_strs;
  opt_list.split(opts_strs, "&");

  for (const auto &opt : opts_strs) {
    if (opt == "pre")
      opts.Pre = true;
    else if (opt == "post")
      opts.Post = true;
    else if (opt == "rename_ctors_array")
      opts.ShouldRenameCtorsArray = true;
    else if (opt.startswith("ep_wrapper=")) {
      StringRef ep_wrap_sym = opt;
      ep_wrap_sym.consume_front("ep_wrapper=");
      opts.EntrypointWrapper = ep_wrap_sym;
    }
  }

  if ((opts.Pre && opts.Post) || !(opts.Pre || opts.Post)) {
    report_fatal_error("Must specify pre or post but not both");
  }

  if (opts.Pre && libgcc.empty()) {
    report_fatal_error("Specify a libgcc path with -embcust-libgcc <PATH>");
  }

  return opts;
}

/* New PM Registration */
llvm::PassPluginLibraryInfo getEmbeddedCustomsPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "EmbeddedCustomsPass", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            // EmbeddedCustomsPass
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::ModulePassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "embcust") {
                    report_fatal_error("EmbeddedCustomsPass needs pre/post "
                                       "option e.g. embcust<{pre,post}>");
                    return false;
                  } else if (Name.startswith("embcust<") &&
                             Name.endswith(">")) {
                    const auto opts = parsePassOpts(Name);
                    PM.addPass(jev::EmbeddedCustomsPass(opts));
                    return true;
                  }
                  return false;
                });

            // debug callbacks
            PB.getPassInstrumentationCallbacks()->registerAfterPassCallback(
                [](StringRef P, Any IR, const PreservedAnalyses &) {
                  if (!any_isa<const Module *>(IR))
                    return;
                  const auto &M = *any_cast<const Module *>(IR);
                  const auto GV = M.getNamedValue("__getf2");
                  fmt::print(stderr, "after pass: {:s} GV: {:p}\n", P,
                             fmt::ptr(GV));
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getEmbeddedCustomsPluginInfo();
}
