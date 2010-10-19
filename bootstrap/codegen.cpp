/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"

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
#include "llvm/GlobalVariable.h"


using namespace heather;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator()
  : fModule(NULL),
    fBuilder(llvm::getGlobalContext()),
    fOptPassManager(NULL),
    fCurrentValue(NULL),
    fHasMainFunc(false)
{
  llvm::InitializeNativeTarget();

  static llvm::ExecutionEngine *theExecutionEngine = NULL;

  llvm::LLVMContext& context = llvm::getGlobalContext();
  fModule = new llvm::Module("compile-unit", context);

  // Create the JIT.  This takes ownership of the module.
  std::string errStr;
  theExecutionEngine = llvm::EngineBuilder(fModule).setErrorStr(&errStr)
                                                   .setEngineKind(llvm::EngineKind::JIT)
                                                   .create();
  if (!theExecutionEngine) {
    logf(kError, "Could not create ExecutionEngine: %s", errStr.c_str());
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

  emitCtorList(fGlobalCtors, "llvm.global_ctors");
  emitCtorList(fGlobalDtors, "llvm.global_dtors");

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


void
CodeGenerator::createDefaultCMainFunc()
{
  std::vector<const llvm::Type*> sign;
  sign.push_back(llvm::Type::getInt32Ty(llvm::getGlobalContext()));
  sign.push_back(llvm::Type::getInt8Ty(llvm::getGlobalContext())->getPointerTo()->getPointerTo());

  llvm::FunctionType *ft =
  llvm::FunctionType::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                          sign,
                          false);
  assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string("main"),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  String appMainFuncNm = heather::mangleToC(String("app|main"));
  llvm::Function* appMainFunc = fModule->getFunction(std::string(StrHelper(appMainFuncNm)));
  assert(appMainFunc != NULL);

  fBuilder.CreateRet(fBuilder.CreateCall(appMainFunc, "appMainTmp"));

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);
}


//------------------------------------------------------------------------------

static llvm::AllocaInst*
createEntryBlockAlloca(llvm::Function *func, const String& name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                          0,
                          std::string(StrHelper(name)));
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == String("unspecified")) {
    // TODO
    return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                  llvm::APInt(32, 0, true));
  }

  // Look this variable up in the function.
  llvm::Value* val = fNamedValues[node->string()];
  if (val == NULL) {
    logf(kError, "Unknown variable name: '%s'", (const char*)StrHelper(node->name()));
    return NULL;
  }

  // Load the value.
  return fBuilder.CreateLoad(val, node->string());
}


llvm::Value*
CodeGenerator::codegen(const ArraySymbolNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SlotdefNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


//! Add a function to the list that will be called before main() runs.
void
CodeGenerator::addGlobalCtor(llvm::Function* ctor, int priority)
{
  // FIXME: Type coercion of void()* types.
  fGlobalCtors.push_back(std::make_pair(ctor, priority));
}

//! Add a function to the list that will be called when the module is
//! unloaded.
void
CodeGenerator::addGlobalDtor(llvm::Function* dtor, int priority)
{
  // FIXME: Type coercion of void()* types.
  fGlobalDtors.push_back(std::make_pair(dtor, priority));
}

void
CodeGenerator::emitCtorList(const CtorList &fns, const char *globalName)
{
  // Ctor function type is void()*.
  llvm::FunctionType* ctorFTy = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()),
                                                        std::vector<const llvm::Type*>(),
                                                        false);
  llvm::Type *ctorPFTy = llvm::PointerType::getUnqual(ctorFTy);

  // Get the type of a ctor entry, { i32, void ()* }.
  llvm::StructType* ctorStructTy = llvm::StructType::get(llvm::getGlobalContext(),
                                                         llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                                                         llvm::PointerType::getUnqual(ctorFTy),
                                                         NULL);

  // Construct the constructor and destructor arrays.
  std::vector<llvm::Constant*> ctors;
  for (CtorList::const_iterator i = fns.begin(), e = fns.end(); i != e; ++i) {
    std::vector<llvm::Constant*> s;
    s.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                                       i->second, false));
    s.push_back(llvm::ConstantExpr::getBitCast(i->first, ctorPFTy));
    ctors.push_back(llvm::ConstantStruct::get(ctorStructTy, s));
  }

  if (!ctors.empty()) {
    llvm::ArrayType *at = llvm::ArrayType::get(ctorStructTy, ctors.size());
    new llvm::GlobalVariable(*fModule, at, false,
                             llvm::GlobalValue::AppendingLinkage,
                             llvm::ConstantArray::get(at, ctors),
                             globalName);
  }
}


llvm::Function*
CodeGenerator::createGlobalInitOrDtorFunction(const llvm::FunctionType *ft,
                                              const String& name)
{
  llvm::Function* fn =
  llvm::Function::Create(ft, llvm::GlobalValue::InternalLinkage,
                         std::string(StrHelper(name)),
                         fModule);

  // clang adds the following __TEXT,__StaticInit, etc. section to static
  // initializer functions.  Initialization however seems to work without
  // also.(?)

  // Set the section if needed.
  // if (const char* section = llvm::getGlobalContext().Target.getStaticInitSectionSpecifier())
  //   fn->setSection("__TEXT,__StaticInit,regular,pure_instructions");

  // fn->setDoesNotThrow();
  return fn;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  llvm::Value* initval = NULL;
  if (node->fInitExpr != NULL) {
    initval = codegenNode(node->fInitExpr);
  }
  else {
    // TODO: init the temporary value.  We shouldn't have to care about this
    // here.
    initval = llvm::ConstantInt::get(llvm::getGlobalContext(),
                                     llvm::APInt(32, 0, true));
  }

  if (!isLocal) {
    String varnm = heather::mangleToC(node->symbolName());
    llvm::GlobalVariable* gv =
      new llvm::GlobalVariable(llvm::Type::getInt32Ty(llvm::getGlobalContext()),
                               false, // isConstant,
                               llvm::GlobalValue::ExternalLinkage,
                               llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                  llvm::APInt(32, 0, true)),
                               std::string(StrHelper(varnm)),
                               false, // ThreadLocal
                               0);    // AddressSpace
    fModule->getGlobalList().push_back(gv);

    const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(llvm::getGlobalContext()),
                                                           false);

    assert(ft != NULL);

    String tmpName = uniqueName("gv");
    String funcnm = heather::mangleToC(tmpName);

    llvm::Function *func = createGlobalInitOrDtorFunction(ft, funcnm);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                    "entry", func);
    fBuilder.SetInsertPoint(bb);
    fBuilder.CreateStore(initval, gv);
    fBuilder.CreateRetVoid();

    verifyFunction(*func);

    if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fOptPassManager->run(*func);

    addGlobalCtor(func, 1);

    // TODO: put the global variable into fNamedValues (?).  GlobalVariables
    // are not AllocaInst however therefore it requires some extra work here.
  }
  else {
    llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

    llvm::AllocaInst* stackSlot = createEntryBlockAlloca(curFunction,
                                                         node->symbolName());
    fBuilder.CreateStore(initval, stackSlot);
    fNamedValues[std::string(StrHelper(node->symbolName()))] = stackSlot;
  }

  return initval;
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
    llvm::AllocaInst* var = fNamedValues[lsym->string()];
    if (var == NULL) {
      logf(kError, "Unknown variable name: '%s'", (const char*)StrHelper(lsym->name()));
      return NULL;
    }

    fBuilder.CreateStore(rvalue, var);
    return rvalue;
  }

  logf(kError, "Not supported yet: %s", __FUNCTION__);
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DefNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->fDefined.obj());
  if (vardefNode != NULL)
    return codegen(vardefNode, false);

  const FuncDefNode* func = dynamic_cast<const FuncDefNode*>(node->fDefined.obj());
  if (func != NULL)
    return codegen(func, false);

  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->fDefined.obj());
  if (vardefNode != NULL)
    return codegen(vardefNode, true);

  const FuncDefNode* funcDefNode = dynamic_cast<const FuncDefNode*>(node->fDefined.obj());
  if (funcDefNode != NULL) {
    logf(kError, "Compiling local functions not supported yet: %s", __FUNCTION__);
    return NULL;
  }

  return NULL;
}


//------------------------------------------------------------------------------

void
CodeGenerator::codegen(const NodeList& nl)
{
  assert(fCurrentValue != NULL);

  for (size_t bidx = 0; bidx < nl.size(); bidx++) {
    llvm::Value* val = codegenNode(nl[bidx]);
    if (val == NULL)
      return;
    if (val != fCurrentValue)
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
  // Insert an explicit fall through from the current block to the loopBB.
  fBuilder.CreateBr(bb);
  fBuilder.SetInsertPoint(bb);

  codegen(node->children());

  fBuilder.CreateBr(contBB);
  fBuilder.SetInsertPoint(contBB);

  return fCurrentValue;
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const BoolNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const CharNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const KeywordNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  return NULL;
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
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RationalNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ArrayNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DictNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VectorNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RangeNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


//------------------------------------------------------------------------------

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
CodeGenerator::codegen(const FuncDefNode* node, bool isLocal)
{
  // TODO: nested functions need special treatment here.  Or even better:
  // avoid nested functions by lambda lifting.
  fNamedValues.clear();

  llvm::FunctionType* ft = createFunctionSignature(node);
  assert(ft != NULL);

  String funcnm = heather::mangleToC(node->funcName());

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string(StrHelper(funcnm)),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  llvm::Function::arg_iterator aiter = func->arg_begin();
  for (size_t pidx = 0; pidx < node->fParams.size(); pidx++, ++aiter) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->fParams[pidx].obj());

    // TODO ende name
    llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func, param->fSymbolName);
    fBuilder.CreateStore(aiter, stackSlot);
    fNamedValues[std::string(StrHelper(param->fSymbolName))] = stackSlot;
  }

  const BlockNode* blockNode = dynamic_cast<const BlockNode*>(node->fBody.obj());
  if (blockNode != NULL) {
    fCurrentValue = createEntryBlockAlloca(func, String("curval"));
    assert(fCurrentValue != NULL);

    codegen(blockNode->children());

    fBuilder.CreateRet(fBuilder.CreateLoad(fCurrentValue));
  }
  else {
    llvm::Value* val = codegenNode(node->fBody);
    if (val == NULL)
      return NULL;
    fBuilder.CreateRet(val);
  }

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);

  if (!isLocal && node->funcName() == String("app|main")) {
    fHasMainFunc = true;
  }

  return func;
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->fBase.obj());
  if (symNode != NULL) {
    // Look up the name in the global module table.
    String funcnm = heather::mangleToC(symNode->fValue);

    calleeFunc = fModule->getFunction(std::string(StrHelper(funcnm)));
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
  const NodeList& nl = node->fChildren;
  if (calleeFunc->arg_size() != nl.size()) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  std::vector<llvm::Value*> argv;
  for (unsigned i = 0, e = nl.size(); i != e; ++i) {
    argv.push_back(codegenNode(nl[i]));
    if (argv.back() == NULL)
      return NULL;
  }

  return fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::codegen(const KeyargNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ParamNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


//------------------------------------------------------------------------------

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


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const IfNode* node)
{
  llvm::Value *testValue = codegenNode(node->test());
  if (testValue == NULL)
    return NULL;

  // Convert condition to a bool by comparing equal to 1
  testValue = fBuilder.CreateICmpEQ(testValue,
                                    llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                           llvm::APInt(1, 1, true)),
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
CodeGenerator::codegen(const MatchNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const OnNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SelectNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ThenWhileNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeDefNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const UnitConstNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const WhileNode* node)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::BasicBlock *loopHeadBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                          "loophead", curFunction);
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "loop", curFunction);
  // Create the "after loop" block and insert it.
  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                       "afterloop",
                                                       curFunction);

  // Insert an explicit fall through from the current block to the loopBB.
  fBuilder.CreateBr(loopHeadBB);

  // Start insertion in loopBB.
  fBuilder.SetInsertPoint(loopHeadBB);

  llvm::Value *testValue = codegenNode(node->test());
  if (testValue == NULL)
    return NULL;

  // Convert condition to a bool by comparing equal to 1
  testValue = fBuilder.CreateICmpEQ(testValue,
                                    llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                           llvm::APInt(1, 1, true)),
                                    "loopcond");
  // Insert the conditional branch into the end of loopEndBB.
  fBuilder.CreateCondBr(testValue, loopBB, afterBB);

  // Start insertion in loopBB.
  fBuilder.SetInsertPoint(loopBB);

  llvm::Value* bodyValue = codegenNode(node->body());
  if (bodyValue == NULL)
    return NULL;

  // jump back to loop start
  fBuilder.CreateBr(loopHeadBB);

  // Any new code will be inserted in AfterBB.
  fBuilder.SetInsertPoint(afterBB);

  return fCurrentValue;
}


llvm::Value*
CodeGenerator::codegen(const CastNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


