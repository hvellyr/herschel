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
  : fContext(llvm::getGlobalContext()),
    fModule(NULL),
    fBuilder(context()),
    fOptPassManager(NULL),
    fCurrentValue(NULL),
    fHasMainFunc(false)
{
  llvm::InitializeNativeTarget();

  static llvm::ExecutionEngine *theExecutionEngine = NULL;

  fModule = new llvm::Module("compile-unit", fContext);

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
  sign.push_back(llvm::Type::getInt32Ty(context()));
  sign.push_back(llvm::Type::getInt8Ty(context())->getPointerTo()->getPointerTo());

  llvm::FunctionType *ft =
  llvm::FunctionType::get(llvm::Type::getInt32Ty(context()),
                          sign,
                          false);
  assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string("main"),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  String appMainFuncNm = heather::mangleToC(String("app|main"));
  llvm::Function* appMainFunc = fModule->getFunction(llvm::StringRef(appMainFuncNm));
  assert(appMainFunc != NULL);

  fBuilder.CreateRet(fBuilder.CreateCall(appMainFunc, "appMainTmp"));

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);
}


//------------------------------------------------------------------------------

llvm::AllocaInst*
CodeGenerator::createEntryBlockAlloca(llvm::Function *func, const String& name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(llvm::Type::getInt32Ty(context()), 0, llvm::Twine(name));
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == String("lang|unspecified")) {
    // TODO
    return llvm::ConstantInt::get(context(),
                                  llvm::APInt(32, 0, true));
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
    assert(false && "unexpected symbol reference");
  }

  if (val == NULL) {
    logf(kError, "Unknown variable name: '%s'", (const char*)StrHelper(node->name()));
    return NULL;
  }

  return fBuilder.CreateLoad(val, node->string());
}


llvm::Value*
CodeGenerator::codegen(const ArrayTypeNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeNode* node)
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
  llvm::FunctionType* ctorFTy = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                        std::vector<const llvm::Type*>(),
                                                        false);
  llvm::Type *ctorPFTy = llvm::PointerType::getUnqual(ctorFTy);

  // Get the type of a ctor entry, { i32, void ()* }.
  llvm::StructType* ctorStructTy = llvm::StructType::get(context(),
                                                         llvm::Type::getInt32Ty(context()),
                                                         llvm::PointerType::getUnqual(ctorFTy),
                                                         NULL);

  // Construct the constructor and destructor arrays.
  std::vector<llvm::Constant*> ctors;
  for (CtorList::const_iterator i = fns.begin(), e = fns.end(); i != e; ++i) {
    std::vector<llvm::Constant*> s;
    s.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context()),
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
                         llvm::Twine(name), fModule);

  // clang adds the following __TEXT,__StaticInit, etc. section to static
  // initializer functions.  Initialization however seems to work without
  // also.(?)

  // Set the section if needed.
  // if (const char* section = context().Target.getStaticInitSectionSpecifier())
  //   fn->setSection("__TEXT,__StaticInit,regular,pure_instructions");

  // fn->setDoesNotThrow();
  return fn;
}


llvm::Value*
CodeGenerator::codegenForGlobalVars(const VardefNode* node)
{
  String varnm = heather::mangleToC(node->name());
  llvm::GlobalVariable* gv =
  new llvm::GlobalVariable(llvm::Type::getInt32Ty(context()),
                           false, // isConstant,
                           llvm::GlobalValue::ExternalLinkage,
                           llvm::ConstantInt::get(context(),
                                                  llvm::APInt(32, 0, true)),
                           llvm::Twine(varnm),
                           false, // ThreadLocal
                           0);    // AddressSpace
  assert(gv != NULL);
  fModule->getGlobalList().push_back(gv);

  fNamedValues.clear();

  const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         false);

  assert(ft != NULL);

  String tmpName = uniqueName("gv");
  String funcnm = heather::mangleToC(tmpName);

  llvm::Function *func = createGlobalInitOrDtorFunction(ft, funcnm);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  llvm::Value* initval = NULL;
  if (node->initExpr() != NULL) {
    initval = codegenNode(node->initExpr());
  }
  else {
    // TODO: init the temporary value.  We shouldn't really have to care about
    // this here, since this can be better done in the AST analysis.
    initval = llvm::ConstantInt::get(context(),
                                     llvm::APInt(32, 0, true));
  }

  fBuilder.CreateStore(initval, gv);
  fBuilder.CreateRetVoid();

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);

  addGlobalCtor(func, 1);

  assert(fGlobalVariables.find(node->name()) == fGlobalVariables.end());
  fGlobalVariables[node->name()] = gv;

  return initval;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  if (!isLocal) {
    return codegenForGlobalVars(node);
  }

  llvm::Value* initval = NULL;
  if (node->initExpr() != NULL) {
    initval = codegenNode(node->initExpr());
  }
  else {
    // TODO: init the temporary value.  We shouldn't really have to care about
    // this here, since this can be better done in the AST analysis.
    initval = llvm::ConstantInt::get(context(),
                                     llvm::APInt(32, 0, true));
  }

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::AllocaInst* stackSlot = createEntryBlockAlloca(curFunction,
                                                       node->name());
  fBuilder.CreateStore(initval, stackSlot);
  fNamedValues[node->name()] = stackSlot;

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
    llvm::AllocaInst* var = fNamedValues[lsym->name()];
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
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode != NULL)
    return codegen(vardefNode, false);

  const FuncDefNode* func = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (func != NULL)
    return codegen(func, false);

  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode != NULL)
    return codegen(vardefNode, true);

  const FuncDefNode* funcDefNode = dynamic_cast<const FuncDefNode*>(node->defNode());
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

  llvm::BasicBlock* bb = llvm::BasicBlock::Create(context(),
                                                  "inner", curFunction);
  llvm::BasicBlock* contBB = llvm::BasicBlock::Create(context(),
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
  return llvm::ConstantInt::get(context(),
                                llvm::APInt(32, node->value(), true));
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
  for (size_t pidx = 0; pidx < node->params().size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
    // TODO
    if (param->isRestArg())
      isVarArgs = true;
    else
      sign.push_back(llvm::Type::getInt32Ty(context()));
  }

  llvm::FunctionType *ft =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(context()),
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

  String funcnm;
  if (node->linkage() == String("C")) {
    funcnm = node->name();
  }
  else {
    funcnm = heather::mangleToC(node->name());
  }

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                llvm::Twine(funcnm),
                                                fModule);

  if (node->body() != NULL) {
    llvm::Function::arg_iterator aiter = func->arg_begin();
    llvm::Function::arg_iterator aiter_e = func->arg_end();
    for (size_t pidx = 0;
         pidx < node->params().size() && aiter != aiter_e;
         pidx++, ++aiter)
    {
      const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());

      // TODO ende name
      llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func, param->name());
      fBuilder.CreateStore(aiter, stackSlot);
      fNamedValues[param->name()] = stackSlot;
    }

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
    fBuilder.SetInsertPoint(bb);

    const BlockNode* blockNode = dynamic_cast<const BlockNode*>(node->body());
    if (blockNode != NULL) {
      fCurrentValue = createEntryBlockAlloca(func, String("curval"));
      assert(fCurrentValue != NULL);

      codegen(blockNode->children());

      fBuilder.CreateRet(fBuilder.CreateLoad(fCurrentValue));
    }
    else {
      llvm::Value* val = codegenNode(node->body());
      if (val == NULL)
        return NULL;
      fBuilder.CreateRet(val);
    }

    verifyFunction(*func);

    if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fOptPassManager->run(*func);

    if (!isLocal && node->name() == String("app|main")) {
      fHasMainFunc = true;
    }
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

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  if (symNode != NULL) {
    assert(symNode->refersTo() == kFunction);

    const AptNode* fn = symNode->scope()->lookupFunction(symNode->name(), false);
    const FuncDefNode* fdn = dynamic_cast<const FuncDefNode*>(fn);
    assert(fdn != NULL);

    String funcnm;
    if (fdn->linkage() == String("C")) {
      funcnm = symNode->name();
    }
    else {
      funcnm = heather::mangleToC(symNode->name());
    }

    calleeFunc = fModule->getFunction(llvm::StringRef(funcnm));
    if (calleeFunc == NULL) {
      errorf(node->srcpos(), 0, "Unknown function referenced: %s",
             (const char*)StrHelper(funcnm));
      return NULL;
    }
  }
  else {
    // TODO
    assert(0);
  }

  // TODO: proper argument mismatch check
  const NodeList& nl = node->children();
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
  llvm::Value *left = codegenNode(node->left());
  llvm::Value *right = codegenNode(node->right());
  if (left == NULL || right == NULL)
    return NULL;

  switch (node->op()) {
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
  llvm::Value *base = codegenNode(node->base());
  if (base == NULL)
    return NULL;

  return fBuilder.CreateMul(base,
                            llvm::ConstantInt::get(context(),
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
                                    llvm::ConstantInt::get(context(),
                                                           llvm::APInt(1, 1, true)),
                                    "ifcond");

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context(),
                                                      "then", curFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context(),
                                                      "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context(),
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
  llvm::PHINode *pn = fBuilder.CreatePHI(llvm::Type::getInt32Ty(context()),
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
  llvm::BasicBlock *loopHeadBB = llvm::BasicBlock::Create(context(),
                                                          "loophead", curFunction);
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(context(),
                                                      "loop", curFunction);
  // Create the "after loop" block and insert it.
  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(context(),
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
                                    llvm::ConstantInt::get(context(),
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



//------------------------------------------------------------------------------

llvm::LLVMContext&
CodeGenerator::context()
{
  return fContext;
}
