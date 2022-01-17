#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

namespace {

bool runEmbeddedCustoms(Module &M) {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  errs() << "runEmbeddedCustoms: name: ";
  errs().write_escaped(M.getName()) << '\n';
  for (const auto &global : M.globals()) {
    errs() << "global: ";
    errs().write_escaped(global.getName()) << '\n';
  }
  for (auto &func : M.functions()) {
    errs() << "func: ";
    errs().write_escaped(func.getName()) << '\n';
    if (func.getName() != "main") {
      func.setVisibility(GlobalValue::VisibilityTypes::HiddenVisibility);
      const auto old_linkage = func.getLinkage();
      auto new_linkage = old_linkage;
      switch (old_linkage) {
      case GlobalValue::LinkageTypes::ExternalLinkage:
        new_linkage = GlobalValue::LinkageTypes::InternalLinkage;
        break;
      default:
        // assert(!"unimplemented");
        break;
      }
      func.setLinkage(new_linkage);
    }
  }
  return true;
}

struct EmbeddedCustoms : PassInfoMixin<EmbeddedCustoms> {
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    if (!runEmbeddedCustoms(M))
      return PreservedAnalyses::all();
    return PreservedAnalyses::none();
  }
};

} // namespace

/* New PM Registration */
llvm::PassPluginLibraryInfo getEmbeddedCustomsPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "EmbeddedCustoms", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::ModulePassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "embcust") {
                    PM.addPass(EmbeddedCustoms());
                    return true;
                  }
                  return false;
                });
            // PB.registerPipelineStartEPCallback(
            //     [&](ModulePassManager &PM, OptimizationLevel Level) {
            //       PM.addPass(EmbeddedCustoms());
            //     });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getEmbeddedCustomsPluginInfo();
}
