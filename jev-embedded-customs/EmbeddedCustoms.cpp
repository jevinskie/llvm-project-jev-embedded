#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
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

static bool runEmbeddedCustoms(Module &M, bool ShouldLinkLibgcc) {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  errs() << "runEmbeddedCustoms: name: ";
  errs().write_escaped(M.getName()) << '\n';
  errs() << "runEmbeddedCustoms: link_libgcc: " << ShouldLinkLibgcc << '\n';

  const auto exported_sym_names = getLines(exported_syms_file);
  const std::set<std::string> exported_syms{exported_sym_names.cbegin(),
                                            exported_sym_names.cend()};
  fmt::print(stderr, "exp_sym: {}\n", fmt::join(exported_syms, ", "));

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
  EmbeddedCustomsPass() { }
  EmbeddedCustomsPass(bool link_libgcc)
      : ShouldLinkLibgcc(link_libgcc) {
        assert(!ShouldLinkLibgcc || !libgcc.empty());
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
                  } else if (Name.startswith("embcust<") && Name.endswith(">")) {
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
