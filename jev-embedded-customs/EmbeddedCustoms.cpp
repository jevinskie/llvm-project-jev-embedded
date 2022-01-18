#include "llvm/BinaryFormat/Magic.h"
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

__attribute__((noinline)) static bool link_bc_archive(Module &M,
                                                      StringRef ar_path) {
  fmt::print(stderr, "linking in {:s}\n", ar_path.str());
  auto ar_bin = llvm::object::createBinary(ar_path);
  if (Error E = ar_bin.takeError())
    EoE(std::move(E));
  if (!ar_bin->getBinary()->isArchive()) {
    report_fatal_error("binary isn't an archive file");
  }
  const auto *ar = cast<const llvm::object::Archive>(ar_bin->getBinary());

  // std::unique_ptr<Module> Result(new Module("ArchiveModule",
  // M.getContext())); Linker L(*Result);
  Linker L(M);

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
      errs() << "child " << *name << " isn't a bitcode file\n";
    }

    SMDiagnostic ParseErr;
    std::unique_ptr<Module> NewM{getLazyIRModule(
        MemoryBuffer::getMemBuffer(*MemBuf, false), ParseErr, M.getContext())};
    if (!NewM) {
      report_fatal_error("failed to parse " + *name);
    }

    if (L.linkInModule(std::move(NewM))) {
      report_fatal_error("failed to link in " + *name);
    }
  }
  if (Err)
    EoE(std::move(Err));

  return true;
}

static bool runEmbeddedCustoms(Module &M, bool ShouldLinkLibgcc) {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  errs() << "runEmbeddedCustoms: name: ";
  errs().write_escaped(M.getName()) << '\n';
  errs() << "runEmbeddedCustoms: link_libgcc: " << ShouldLinkLibgcc << '\n';

  const auto exported_sym_names = getLines(exported_syms_file);
  const std::set<std::string> exported_syms{exported_sym_names.cbegin(),
                                            exported_sym_names.cend()};
  fmt::print(stderr, "exp_sym: {}\n", fmt::join(exported_syms, ", "));
  if (ShouldLinkLibgcc) {
    fmt::print(stderr, "linking in libgcc\n", fmt::join(exported_syms, ", "));
    link_bc_archive(M, libgcc);
  }

  for (auto &GV : M.global_values()) {
    // errs() << "GV: ";
    // errs().write_escaped(GV.getName()) << '\n';
    if (!exported_syms.contains(GV.getName().str())) {
      using LT = GlobalValue::LinkageTypes;
      // func.setVisibility(GlobalValue::VisibilityTypes::HiddenVisibility);
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
          new_linkage = LT::InternalLinkage;
          break;
        case LT::PrivateLinkage:
          // do nothing
          break;
        default:
          llvm_unreachable("unhandled linkage type");
        }
      }

      if (new_linkage != old_linkage) {
        // errs() << "changing " << GV.getName() << " linkage from " <<
        // old_linkage
        // << " to " << new_linkage << '\n';
      }
      GV.setLinkage(new_linkage);
    }
  }
  return true;
}

class EmbeddedCustomsPass : public PassInfoMixin<EmbeddedCustomsPass> {
  bool ShouldLinkLibgcc;

public:
  EmbeddedCustomsPass() {}
  EmbeddedCustomsPass(bool link_libgcc) : ShouldLinkLibgcc(link_libgcc) {
    if (ShouldLinkLibgcc && libgcc.empty()) {
      report_fatal_error("Specify a libgcc path with -embcust-libgcc <PATH>");
    }
  }

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    if (!runEmbeddedCustoms(M, ShouldLinkLibgcc))
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
                  } else if (Name == "embcust<link_libgcc>") {
                    PM.addPass(jev::EmbeddedCustomsPass(true));
                    return true;
                  }
                  return false;
                });
            // PB.registerPipelineStartEPCallback(
            //     [&](ModulePassManager &PM, OptimizationLevel Level) {
            //       PM.addPass(EmbeddedCustomsPass());
            //     });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  assert(!"fuckabees!");
  return getEmbeddedCustomsPluginInfo();
}
