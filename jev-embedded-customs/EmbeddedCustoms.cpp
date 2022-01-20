#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Function.h"
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

#include <fmt/format.h>

using namespace llvm;

static cl::opt<std::string>
    exported_syms_file("embcust-exported-syms",
                       cl::desc("file with list of exported symbols"),
                       cl::Required);

static cl::opt<std::string>
    libgcc("embcust-libgcc",
           cl::desc("libgcc/compiler-rt lto library to link"));

namespace jev {

static const char *libcallRoutineNames[] = {
#define HANDLE_LIBCALL(code, name) name,
#include "llvm/IR/RuntimeLibcalls.def"
#undef HANDLE_LIBCALL
};

static std::set<std::string> getLibcallRoutines() {
  std::set<std::string> res;
  for (const auto name : libcallRoutineNames) {
    if (name)
      res.insert(name);
  }
  return res;
}

static const auto libcallRoutines{getLibcallRoutines()};

static void EoE(Error &&E) {
  std::string str;
  llvm::raw_string_ostream ostr(str);
  ostr << E;
  report_fatal_error(StringRef{str});
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

static bool link_bc_archive(Module &M, StringRef ar_path) {
  std::unique_ptr<Module> NewM = load_bc_archive(ar_path, M.getContext());
  if (!NewM) {
    report_fatal_error("failed to load " + ar_path);
  }
  errs() << "dumping librt whole mod\n";
  dump_module(*NewM, "librt.bc");
  dump_module(M, "mod.bc");
  errs() << "begining librt ar whole link\n";
  Linker L(M);
  if (L.linkInModule(std::move(NewM), Linker::Flags::LinkOnlyNeeded)) {
    report_fatal_error("failed to link in " + ar_path);
  }
  dump_module(M, "mod-linked.bc");
  return true;
}

static bool runEmbeddedCustoms(Module &M, bool link_libgcc,
                               bool rename_ctors_array) {

  if (link_libgcc) {
    link_bc_archive(M, libgcc);
  }

  // get exported syms from newline seperated file (# as first char on line is a
  // comment)
  const auto exported_sym_names = getLines(exported_syms_file);
  const std::set<std::string> exported_syms{exported_sym_names.cbegin(),
                                            exported_sym_names.cend()};

  // symbols to not change visibility of
  static const std::set<std::string> vis_mod_skiplist{
      // "llvm.used",
  };

  for (auto &GV : M.global_values()) {
    // if we find a (builtin? not sure) libcall, add it to used or else it may
    // get optimized out but another pass later adds a call
    if (libcallRoutines.contains(GV.getName().str())) {
      appendToUsed(M, ArrayRef{&GV});
    }

    if ( // don't change visibility on symbols we want exported
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
      if (!GV.isDeclaration()) {
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
        case LT::PrivateLinkage:
          // do nothing, already "weakest" linkage
          break;
        default:
          llvm_unreachable("unhandled linkage type");
        }
      }
      GV.setLinkage(new_linkage);
    }
  }

  if (rename_ctors_array) {
    errs() << "renaming ctor/dtor array\n";
  }

  return true;
}

class EmbeddedCustomsPass : public PassInfoMixin<EmbeddedCustomsPass> {
  bool ShouldLinkLibgcc;
  bool ShouldRenameCtorsArray;

public:
  EmbeddedCustomsPass()
      : ShouldLinkLibgcc(false), ShouldRenameCtorsArray(false) {}
  EmbeddedCustomsPass(bool link_libgcc, bool rename_ctors_array)
      : ShouldLinkLibgcc(link_libgcc),
        ShouldRenameCtorsArray(rename_ctors_array) {
    if (ShouldLinkLibgcc && libgcc.empty()) {
      report_fatal_error("Specify a libgcc path with -embcust-libgcc <PATH>");
    }
  }

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    if (!runEmbeddedCustoms(M, ShouldLinkLibgcc, ShouldRenameCtorsArray))
      return PreservedAnalyses::all();
    return PreservedAnalyses::none();
  }
};

} // namespace jev

/* New PM Registration */
llvm::PassPluginLibraryInfo getEmbeddedCustomsPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "EmbeddedCustomsPass", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::ModulePassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "embcust") {
                    PM.addPass(jev::EmbeddedCustomsPass());
                    return true;
                  } else if (Name.startswith("embcust<") &&
                             Name.endswith(">")) {
                    bool link_libgcc = false;
                    bool rename_ctors_array = false;
                    StringRef opt_list{Name};
                    opt_list.consume_front("embcust<");
                    opt_list.consume_back(">");
                    SmallVector<StringRef> opts;
                    opt_list.split(opts, ",");
                    for (const auto &opt : opts) {
                      if (opt == "link_libgcc")
                        link_libgcc = true;
                      else if (opt == "rename_ctors_array")
                        rename_ctors_array = true;
                    }
                    PM.addPass(jev::EmbeddedCustomsPass(link_libgcc,
                                                        rename_ctors_array));
                    return true;
                  }
                  return false;
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getEmbeddedCustomsPluginInfo();
}
