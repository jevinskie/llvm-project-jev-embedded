#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

static cl::opt<bool> Wave("wave-goodbye", cl::init(true),
                          cl::desc("wave good bye"));

namespace {

bool runEmbeddedCustoms(Function &F) {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  if (Wave) {
    errs() << "Bye: ";
    errs().write_escaped(F.getName()) << '\n';
  }
  return false;
}

bool runEmbeddedCustoms(Module &M) {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  if (Wave) {
    errs() << "Bye: ";
    errs().write_escaped(M.getName()) << '\n';
  }
  return false;
}

struct LegacyEmbeddedCustoms : public FunctionPass {
  static char ID;
  LegacyEmbeddedCustoms() : FunctionPass(ID) {}
  bool runOnFunction(Function &F) override {
    errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
    return runEmbeddedCustoms(F);
  }
};

struct EmbeddedCustoms : PassInfoMixin<EmbeddedCustoms> {
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &) {
    errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
    if (!runEmbeddedCustoms(F))
      return PreservedAnalyses::all();
    return PreservedAnalyses::none();
  }
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
    if (!runEmbeddedCustoms(M))
      return PreservedAnalyses::all();
    return PreservedAnalyses::none();
  }
};

} // namespace

char LegacyEmbeddedCustoms::ID = 0;

static RegisterPass<LegacyEmbeddedCustoms> X("embcust", "Embedded Customs pass",
                                             false /* Only looks at CFG */,
                                             false /* Analysis Pass */);

/* Legacy PM Registration */
static llvm::RegisterStandardPasses
    RegisterEmbeddedCustoms(llvm::PassManagerBuilder::EP_VectorizerStart,
                            [](const llvm::PassManagerBuilder &Builder,
                               llvm::legacy::PassManagerBase &PM) {
                              errs()
                                  << "EmbeddedCustoms: " << __PRETTY_FUNCTION__
                                  << "\n";
                              PM.add(new LegacyEmbeddedCustoms());
                            });

/* New PM Registration */
llvm::PassPluginLibraryInfo getEmbeddedCustomsPluginInfo() {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  return {LLVM_PLUGIN_API_VERSION, "EmbeddedCustoms", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
            PB.registerVectorizerStartEPCallback(
                [](llvm::FunctionPassManager &PM, OptimizationLevel Level) {
                  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
                  PM.addPass(EmbeddedCustoms());
                });
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::FunctionPassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
                  if (Name == "embcust") {
                    PM.addPass(EmbeddedCustoms());
                    return true;
                  }
                  return false;
                });
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::ModulePassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
                  if (Name == "embcust") {
                    PM.addPass(EmbeddedCustoms());
                    return true;
                  }
                  return false;
                });
            PB.registerPipelineStartEPCallback(
                [&](ModulePassManager &PM, OptimizationLevel Level) {
                  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
                  PM.addPass(EmbeddedCustoms());
                });
            PB.printPassNames(errs());
          }};
}

#ifndef LLVM_BYE_LINK_INTO_TOOLS
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  errs() << "EmbeddedCustoms: " << __PRETTY_FUNCTION__ << "\n";
  return getEmbeddedCustomsPluginInfo();
}
#endif
