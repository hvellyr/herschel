/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "apt.h"
#include "codegen-apply.h"
#include "codegen-binnode.h"
#include "codegen-func.h"
#include "codegen-if.h"
#include "codegen-init.h"
#include "codegen-tools.h"
#include "codegen-types.h"
#include "codegen-vardef.h"
#include "codegen-while.h"
#include "codegen.h"
#include "log.h"
#include "predefined.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"

#include <vector>
#include <typeinfo>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/System/Host.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegistry.h"
//#include "llvm/DIBuilder.h"

using namespace herschel;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator(Compiler* compiler)
  : fCompiler(compiler),
    fContext(llvm::getGlobalContext()),
    fModule(NULL),
    // fDIBuilder(NULL),
    fBuilder(context()),
    fOptPassManager(NULL),
    fTargetData(NULL),
    fInitializer(new ModuleRuntimeInitializer(this)),
    fTypes(new CodegenTypeUtils(this)),
    fTools(new CodegenTools(this)),
    fHasMainFunc(false)
{
  hr_assert(fCompiler != NULL);

  llvm::InitializeNativeTarget();

  fModule = new llvm::Module("compile-unit", fContext);
  fModule->setTargetTriple(llvm::sys::getHostTriple());

  std::string error;
  const llvm::Target* target =
    llvm::TargetRegistry::lookupTarget(fModule->getTargetTriple(), error);
  if (target == NULL) {
    logf(kError, "Compile setup failure: %s", error.c_str());
    exit(1);
  }

  llvm::TargetMachine* machine = target->createTargetMachine(fModule->getTargetTriple(),
                                                             "");
  if (machine == NULL) {
    logf(kError, "Compile setup failure: no matching TargetMachine found");
    exit(1);
  }
  fTargetData = const_cast<llvm::TargetData*>(machine->getTargetData());

  // logf(kInfo, "Host triple: %s", fModule->getTargetTriple().c_str());
  // logf(kInfo, "PointerSize: %d", fTargetData->getPointerSize());

  fOptPassManager = new llvm::FunctionPassManager(fModule);

  setupOptPassManager();
}


void
CodeGenerator::setupOptPassManager()
{
  if (Properties::optimizeLevel() > kOptLevelNone) {
    // Set up the optimizer pipeline.  Start with registering info about how the
    // target lays out data structures.
    fOptPassManager->add(fTargetData);
    // Promote allocas to registers.
    fOptPassManager->add(llvm::createPromoteMemoryToRegisterPass());

    fOptPassManager->add(llvm::createScalarReplAggregatesPass());
    // fOptPassManager->add(llvm::createGlobalDCEPass());
    // fOptPassManager->add(llvm::createDeadArgEliminationPass());

    // fOptPassManager->add(llvm::createFunctionInliningPass());
    // fOptPassManager->add(llvm::createCondPropagationPass());
    fOptPassManager->add(llvm::createLoopRotatePass());

    // Hoist loop invariants
    fOptPassManager->add(llvm::createLICMPass());
    fOptPassManager->add(llvm::createLoopUnswitchPass());
    // Split loop index
    // fOptPassManager->add(llvm::createLoopIndexSplitPass());
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Canonicalize indvars
    fOptPassManager->add(llvm::createIndVarSimplifyPass());
    // Delete dead loops
    fOptPassManager->add(llvm::createLoopDeletionPass());
    // Unroll small loops
    fOptPassManager->add(llvm::createLoopUnrollPass());
    // Clean up after the unroller
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Remove redundancies
    fOptPassManager->add(llvm::createGVNPass());
    // Remove memcpy / form memset
    fOptPassManager->add(llvm::createMemCpyOptPass());
    // Constant prop with SCCP
    fOptPassManager->add(llvm::createSCCPPass());
    fOptPassManager->add(llvm::createTailCallEliminationPass());

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    fOptPassManager->add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    fOptPassManager->add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    fOptPassManager->add(llvm::createCFGSimplificationPass());

    fOptPassManager->doInitialization();
  }
}


CodeGenerator::~CodeGenerator()
{
  if (fOptPassManager != NULL) {
    delete fOptPassManager;
    fOptPassManager = NULL;
  }
  if (fModule != NULL) {
    delete fModule;
    fModule = NULL;
  }
}


bool
CodeGenerator::is64Bit() const
{
  return fTargetData->getPointerSize() == 8;
}


llvm::TargetData*
CodeGenerator::targetData() const
{
  return fTargetData;
}


llvm::LLVMContext&
CodeGenerator::context() const
{
  return fContext;
}


llvm::IRBuilder<>&
CodeGenerator::builder() const
{
  return const_cast<llvm::IRBuilder<>&>(fBuilder);
}


llvm::Module*
CodeGenerator::module() const
{
  return fModule;
}


llvm::FunctionPassManager*
CodeGenerator::optPassManager() const
{
  return fOptPassManager;
}


//------------------------------------------------------------------------------

bool
CodeGenerator::compileToCode(const CompileUnitNode* node,
                             const String& outputFile)
{
  node->codegen(this);

  fInitializer->finish();

  hr_assert(!outputFile.isEmpty());

  std::string errInfo;
  llvm::raw_fd_ostream outstream(StrHelper(outputFile), errInfo, 0);
  if (!errInfo.empty()) {
    logf(kError, "Failed to open output file '%s': %s",
         (const char*)StrHelper(outputFile), errInfo.c_str());
    return false;
  }

  switch (Properties::compileOutFormat()) {
  case kLLVM_IR:
    fModule->print(outstream, NULL);
    break;
  case kLLVM_BC:
    llvm::WriteBitcodeToFile(fModule, outstream);
    break;
  }
  return true;
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegenNode(const AptNode* node, bool autoloadAllocInst)
{
  llvm::Value* val = node->codegen(this);
  if (autoloadAllocInst && llvm::AllocaInst::classof(val)) {
    val = fBuilder.CreateLoad(val);
  }
  return val;
}


llvm::Value*
CodeGenerator::codegen(const CompileUnitNode* node)
{
  const NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++)
    codegenNode(nl[i].obj());

  if (fHasMainFunc) {
    createDefaultCMainFunc();
  }

  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const NodeList& nl)
{
  llvm::Value* lastVal = NULL;

  for (size_t bidx = 0; bidx < nl.size(); bidx++) {
    llvm::Value* val = codegenNode(nl[bidx]);
    if (val == NULL)
      return NULL;
    lastVal = val;
  }
  return lastVal;
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == String("lang|unspecified")) {
    // TODO
    return llvm::Constant::getNullValue(fTypes->getType(node->type()));
  }

  llvm::Value* val = NULL;

  switch (node->refersTo()) {
  case kLocalVar:
  case kParam:
    val = fNamedValues[node->name()];
    break;
  case kGlobalVar:
    val = fGlobalVariables[node->name()];
    break;
  default:
    hr_invalid("unexpected symbol reference");
  }

  if (val == NULL) {
    logf(kError, "Unknown symbol '%s'", (const char*)StrHelper(node->name()));
    return NULL;
  }

  return val;
}


llvm::Value*
CodeGenerator::codegen(const ArrayTypeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO

  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO

  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SlotdefNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  return CodegenVardef(this).emit(node, isLocal);
}


llvm::Value*
CodeGenerator::codegen(const AssignNode* node)
{
  const SymbolNode* lsym = dynamic_cast<const SymbolNode*>(node->lvalue());
  if (lsym != NULL) {
    llvm::Value* rvalue = codegenNode(node->rvalue());
    if (rvalue == NULL)
      return NULL;

    // Look up the name.
    llvm::AllocaInst* var = fNamedValues[lsym->name()];
    if (var == NULL) {
      logf(kError, "Unknown symbol '%s'", (const char*)StrHelper(lsym->name()));
      return NULL;
    }

    llvm::Value* val = fTools->emitPackCode(node->rvalue()->dstType(),
                                            node->rvalue()->typeConv(),
                                            fTools->wrapLoad(rvalue),
                                            node->rvalue()->type());
    fBuilder.CreateStore(val, var);

    return rvalue;
  }

  logf(kError, "Not supported yet: %s", typeid(node).name());
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DefNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode != NULL)
    return codegen(vardefNode, !K(isLocal));

  const FuncDefNode* func = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (func != NULL)
    return codegen(func, !K(isLocal));

  const TypeDefNode* type = dynamic_cast<const TypeDefNode*>(node->defNode());
  if (type != NULL)
    return codegen(type);

  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode != NULL)
    return codegen(vardefNode, K(isLocal));

  const FuncDefNode* funcDefNode = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (funcDefNode != NULL) {
    logf(kError, "Compiling local functions not supported yet: %s", typeid(node).name());
    return NULL;
  }

  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BlockNode* node)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock* bb = llvm::BasicBlock::Create(context(),
                                                  "inner", curFunction);
  llvm::BasicBlock* contBB = llvm::BasicBlock::Create(context(),
                                                      "next", curFunction);
  // Insert an explicit fall through from the current block to the loopBB.
  fBuilder.CreateBr(bb);
  fBuilder.SetInsertPoint(bb);

  llvm::Value* lastVal = codegen(node->children());

  fBuilder.CreateBr(contBB);
  fBuilder.SetInsertPoint(contBB);

  return lastVal;
}


llvm::Value*
CodeGenerator::codegen(const BoolNode* node)
{
// TODO plaintype?
  if (node->dstType().isPlainType()) {
    if (node->value())
      return fBuilder.getTrue();
    else
      return fBuilder.getFalse();
  }

  return fTools->makeBoolAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const CharNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const KeywordNode* node)
{
  return fTools->makeKeywordAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const IntNode* node)
{
  return fBuilder.CreateIntCast(fBuilder.getInt32(node->value()),
                                fTypes->getType(node->type()),
                                node->type().isSigned());
}


llvm::Value*
CodeGenerator::codegen(const RealNode* node)
{
  return llvm::ConstantFP::get(context(),
                               llvm::APFloat(node->value()));
}


llvm::Value*
CodeGenerator::codegen(const StringNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RationalNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ArrayNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DictNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VectorNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RangeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const FuncDefNode* node, bool isLocal)
{
  return CodegenFuncDef(this).emit(node, isLocal);
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  return CodegenApply(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const KeyargNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ParamNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BinaryNode* node)
{
  return CodegenBinaryNode(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const NegateNode* node)
{
  llvm::Value *base = fTools->wrapLoad(codegenNode(node->base()));
  if (base == NULL)
    return NULL;

  return fBuilder.CreateMul(base,
                            llvm::ConstantInt::get(context(),
                                                   llvm::APInt(32, (uint64_t)-1, true)),
                            "negtmp");
}


llvm::Value*
CodeGenerator::codegen(const IfNode* node)
{
  return CodegenIf(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const MatchNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const OnNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SelectNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeDefNode* node)
{
  fInitializer->addTypeDef(node);
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const UnitConstNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const WhileNode* node)
{
  return CodegenWhile(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const CastNode* node)
{
  llvm::Value *val = fTools->wrapLoad(codegenNode(node->base()));
  if (val == NULL)
    return NULL;

  return fTools->emitPackCode(node->base()->dstType(),
                              node->base()->typeConv(),
                              val,
                              node->base()->type());
}


llvm::Value*
CodeGenerator::codegen(const UndefNode* node)
{
  hr_invalid("You shouldn't be here");
  return NULL;
}
