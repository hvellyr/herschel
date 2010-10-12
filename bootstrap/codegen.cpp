/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"

#include <vector>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/raw_ostream.h"


//----------------------------------------------------------------------------

using namespace heather;


CodeGenerator::CodeGenerator()
  : fModule(NULL),
    fBuilder(llvm::getGlobalContext()),
    fOptPassManager(NULL),
    fCurrentValue(NULL)
{
  llvm::InitializeNativeTarget();

  static llvm::ExecutionEngine *theExecutionEngine = NULL;

  llvm::LLVMContext& context = llvm::getGlobalContext();
  fModule = new llvm::Module("compile-unit", context);

  // Create the JIT.  This takes ownership of the module.
  std::string errStr;
  theExecutionEngine = llvm::EngineBuilder(fModule).setErrorStr(&errStr).setEngineKind(llvm::EngineKind::JIT).create();
  if (!theExecutionEngine) {
    fprintf(stderr, "Could not create ExecutionEngine: %s\n", errStr.c_str());
    exit(1);
  }
  fOptPassManager = new llvm::FunctionPassManager(fModule);

  // Set up the optimizer pipeline.  Start with registering info about how the
  // target lays out data structures.
  fOptPassManager->add(new llvm::TargetData(*theExecutionEngine->getTargetData()));
  // Promote allocas to registers.
  fOptPassManager->add(llvm::createPromoteMemoryToRegisterPass());
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


CodeGenerator::~CodeGenerator()
{
  if (fOptPassManager != NULL)
    delete fOptPassManager;
  if (fModule != NULL)
    delete fModule;
}


bool
CodeGenerator::compileToCode(const CompileUnitNode* node,
                             const String& outputFile)
{
  node->codegen(this);

  assert(!outputFile.isEmpty());

  std::string errInfo;
  llvm::raw_fd_ostream outstream(StrHelper(outputFile), errInfo, 0);
  if (!errInfo.empty()) {
    logf(kError, "Failed to open output file '%s': %s",
         (const char*)StrHelper(outputFile), errInfo.c_str());
    return false;
  }

  switch (Properties::compileOutFormat()) {
  case kNativeObject:
    logf(kError, "Unsupported outputformat.");
    return false;
  case kLLVM_IR:
    fModule->print(outstream, NULL);
    break;

  case kLLVM_BC:
    llvm::WriteBitcodeToFile(fModule, outstream);
    break;
  }
  return true;
}


llvm::Value*
CodeGenerator::codegenNode(const AptNode* node)
{
  return node->codegen(this);
}


llvm::Value*
CodeGenerator::codegen(const IntNode* node)
{
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, node->fValue, true));
}


llvm::Value*
CodeGenerator::codegen(const RealNode* node)
{
  return llvm::ConstantFP::get(llvm::getGlobalContext(),
                               llvm::APFloat(node->fValue));
}


llvm::Value*
CodeGenerator::codegen(const StringNode* node)
{
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  // Look this variable up in the function.
  llvm::Value* val = fNamedValues[node->string()];
  if (val == NULL) {
    printf("Unknown variable name");
    return NULL;
  }

  // Load the value.
  return fBuilder.CreateLoad(val, node->string());
}


llvm::Value*
CodeGenerator::codegen(const KeywordNode* node)
{
  return NULL;
}




llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->fBase.obj());
  if (symNode != NULL) {
    // Look up the name in the global module table.
    calleeFunc = fModule->getFunction(symNode->string());
    if (calleeFunc == NULL) {
      errorf(node->srcpos(), 0, "Unknown function referenced");
      return NULL;
    }
  }
  else {
    // TODO
    assert(0);
  }

  // TODO: proper argument mismatch check
  if (calleeFunc->arg_size() != node->fChildren.size()) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  std::vector<llvm::Value*> argv;
  for (unsigned i = 0, e = node->fChildren.size(); i != e; ++i) {
    argv.push_back(codegenNode(node->fChildren[i]));
    if (argv.back() == NULL)
      return NULL;
  }

  return fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::codegen(const ArrayNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ArraySymbolNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const AssignNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BinaryNode* node)
{
  llvm::Value *left = codegenNode(node->fLeft);
  llvm::Value *right = codegenNode(node->fRight);
  if (left == NULL || right == NULL)
    return NULL;

  switch (node->fOp) {
  case kOpPlus:     return fBuilder.CreateAdd(left, right, "addtmp");
  case kOpMinus:    return fBuilder.CreateSub(left, right, "subtmp");
  case kOpMultiply: return fBuilder.CreateMul(left, right, "multmp");
  case kOpLess:
    return fBuilder.CreateICmpULT(left, right, "cmptmp");
  default:
    printf("invalid binary operator");
    return NULL;
  }
}


static llvm::AllocaInst*
createEntryBlockAlloca(llvm::Function *func, const String& name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                          0,
                          std::string(StrHelper(name)));
}


void
CodeGenerator::codegen(const NodeList& nl, llvm::BasicBlock* bb)
{
  assert(fCurrentValue != NULL);

  for (size_t bidx = 0; bidx < nl.size(); bidx++) {
    llvm::Value* val = codegenNode(nl[bidx]);
    if (val == NULL)
      return;
    fBuilder.CreateStore(val, fCurrentValue);
  }
}


llvm::Value*
CodeGenerator::codegen(const BlockNode* node)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock* bb = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                  "inner", curFunction);
  llvm::BasicBlock* contBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "next", curFunction);
  fBuilder.SetInsertPoint(bb);

  codegen(node->fChildren, bb);

  fBuilder.CreateBr(contBB);
  fBuilder.SetInsertPoint(contBB);

  return contBB;
}


llvm::Value*
CodeGenerator::codegen(const BoolNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const CharNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const CompileUnitNode* node)
{
  for (size_t i = 0; i < node->fChildren.size(); i++)
    codegenNode(node->fChildren[i].obj());
  return NULL;
//   using namespace llvm;

//   std::vector<const llvm::Type*> sign2;
//   sign2.push_back(llvm::Type::getDoubleTy(getGlobalContext()));
//   sign2.push_back(llvm::Type::getInt32Ty(getGlobalContext()));

//   FunctionType *ft2 = FunctionType::get(llvm::Type::getInt32Ty(getGlobalContext()), sign2, true);
//   Function *f2 = llvm::Function::Create(ft2, Function::PrivateLinkage, "", fModule);

//   BasicBlock *bb2 = BasicBlock::Create(getGlobalContext(), "entry2", f2);

//   fBuilder.SetInsertPoint(bb2);
//   fBuilder.CreateRet(fBuilder.CreateAdd(llvm::ConstantInt::get(getGlobalContext(),
//                                                                APInt(32, 42, true)),
//                                         llvm::ConstantInt::get(getGlobalContext(),
//                                                                APInt(32, 11, true))));
//   verifyFunction(*f2);

//   printf("------------------------------------------\n");

// // Make the function type:  double(double,double) etc.
//   std::vector<const llvm::Type*> sign(0, llvm::Type::getDoubleTy(getGlobalContext()));
//   FunctionType *ft = FunctionType::get(ft2->getPointerTo(), //llvm::Type::getInt32Ty(getGlobalContext()),
//                                        sign, false);
//   Function *f = llvm::Function::Create(ft, Function::ExternalLinkage, std::string("x"), fModule);


// // Create a new basic block to start insertion into.
//   BasicBlock *bb = BasicBlock::Create(getGlobalContext(), "entry", f);
//   fBuilder.SetInsertPoint(bb);
//   fBuilder.CreateRet(f2);

//   verifyFunction(*f);

//   return f;
}


llvm::Value*
CodeGenerator::codegen(const DefNode* node)
{
  // TODO
  const FuncDefNode* func = dynamic_cast<const FuncDefNode*>(node->fDefined.obj());
  if (func != NULL) {
    return codegenNode(func);
  }
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DictNode* node)
{
  // TODO
  return NULL;
}


llvm::FunctionType*
CodeGenerator::createFunctionSignature(const FunctionNode* node)
{
  std::vector<const llvm::Type*> sign;

  bool isVarArgs = false;
  for (size_t pidx = 0; pidx < node->fParams.size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->fParams[pidx].obj());
    // TODO
    if (param->isRestArg())
      isVarArgs = true;
    else
      sign.push_back(llvm::Type::getInt32Ty(llvm::getGlobalContext()));
  }

  llvm::FunctionType *ft =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                            sign,
                            isVarArgs);

  return ft;
}


llvm::Value*
CodeGenerator::codegen(const FuncDefNode* node)
{
  // TODO: nested functions need special treatment here.  Or even better:
  // avoid nested functions by lambda lifting.
  fNamedValues.clear();

  llvm::FunctionType* ft = createFunctionSignature(node);
  assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string(StrHelper(node->funcName())),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  llvm::Function::arg_iterator aiter = func->arg_begin();
  for (size_t pidx = 0; pidx < node->fParams.size(); pidx++, ++aiter) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->fParams[pidx].obj());

    llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func, param->fSymbolName);
    fBuilder.CreateStore(aiter, stackSlot);
    fNamedValues[std::string(StrHelper(param->fSymbolName))] = stackSlot;
  }

  const BlockNode* blockNode = dynamic_cast<const BlockNode*>(node->fBody.obj());
  if (blockNode != NULL) {
    fCurrentValue = createEntryBlockAlloca(func, String("curval"));
    assert(fCurrentValue != NULL);

    codegen(blockNode->fChildren, bb);

    fBuilder.CreateRet(fBuilder.CreateLoad(fCurrentValue));
  }
  else {
    llvm::Value* val = codegenNode(node->fBody);
    if (val == NULL)
      return NULL;
    fBuilder.CreateRet(val);
  }

  verifyFunction(*func);

  if (fOptPassManager != NULL)
    fOptPassManager->run(*func);

  return func;
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const IfNode* node)
{
  llvm::Value *testValue = codegenNode(node->test());
  if (testValue == NULL)
    return NULL;

  // Convert condition to a bool by comparing equal to 0
  testValue = fBuilder.CreateICmpEQ(testValue,
                                    llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                           llvm::APInt(1, 0, true)),
                                    "ifcond");

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "then", curFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                       "ifcont");

  fBuilder.CreateCondBr(testValue, thenBB, elseBB);

  // Emit then value.
  fBuilder.SetInsertPoint(thenBB);

  llvm::Value *thenValue = codegenNode(node->consequent());
  if (thenValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'then' can change the current block, update thenBB for the PHI.
  thenBB = fBuilder.GetInsertBlock();


  // Emit else block.
  curFunction->getBasicBlockList().push_back(elseBB);
  fBuilder.SetInsertPoint(elseBB);

  llvm::Value *elseValue = codegenNode(node->alternate());
  if (elseValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = fBuilder.GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  fBuilder.SetInsertPoint(mergeBB);
  llvm::PHINode *pn = fBuilder.CreatePHI(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                                         "iftmp");

  pn->addIncoming(thenValue, thenBB);
  pn->addIncoming(elseValue, elseBB);

  return pn;
}


llvm::Value*
CodeGenerator::codegen(const KeyargNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->fDefined.obj());
  if (vardefNode != NULL) {
    llvm::Value* initval = NULL;
    if (vardefNode->fInitExpr != NULL) {
      initval = codegenNode(vardefNode->fInitExpr);
    }
    else {
      // TODO: init the temporary value.  We shouldn't have to care about this
      // here.
      initval = llvm::ConstantInt::get(llvm::getGlobalContext(),
                                       llvm::APInt(32, 0, true));
    }

    llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

    llvm::AllocaInst *stackSlot = createEntryBlockAlloca(curFunction,
                                                         vardefNode->fSymbolName);
    fBuilder.CreateStore(initval, stackSlot);
    fNamedValues[std::string(StrHelper(vardefNode->fSymbolName))] = stackSlot;

    return initval;
  }
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const MatchNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const NegateNode* node)
{
  llvm::Value *base = codegenNode(node->fBase);
  if (base == NULL)
    return NULL;

  return fBuilder.CreateMul(base,
                            llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                   llvm::APInt(32, (uint64_t)-1, true)),
                            "negtmp");
}


llvm::Value*
CodeGenerator::codegen(const OnNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ParamNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RangeNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RationalNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SelectNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SlotdefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ThenWhileNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const UnitConstant* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VectorNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const WhileNode* node)
{
  // TODO
  return NULL;
}
