/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
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
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"

using namespace herschel;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator()
  : fContext(llvm::getGlobalContext()),
    fModule(NULL),
    fBuilder(context()),
    fOptPassManager(NULL),
    fHasMainFunc(false)
{
  llvm::InitializeNativeTarget();

  static llvm::ExecutionEngine *theExecutionEngine = NULL;

  fModule = new llvm::Module("compile-unit", fContext);

  // const llvm::Target *TheTarget = TargetRegistry::lookupTarget(Triple, Error);

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

  fOptPassManager->add(llvm::createScalarReplAggregatesPass());
  // fOptPassManager->add(llvm::createGlobalDCEPass());
  // fOptPassManager->add(llvm::createDeadArgEliminationPass());

  // fOptPassManager->add(llvm::createFunctionInliningPass());
  // fOptPassManager->add(llvm::createCondPropagationPass());
  fOptPassManager->add(llvm::createLoopRotatePass());

  fOptPassManager->add(llvm::createLICMPass());                  // Hoist loop invariants
  fOptPassManager->add(llvm::createLoopUnswitchPass());
  fOptPassManager->add(llvm::createLoopIndexSplitPass());        // Split loop index
  fOptPassManager->add(llvm::createInstructionCombiningPass());
  fOptPassManager->add(llvm::createIndVarSimplifyPass());        // Canonicalize indvars
  fOptPassManager->add(llvm::createLoopDeletionPass());          // Delete dead loops
  fOptPassManager->add(llvm::createLoopUnrollPass());          // Unroll small loops
  fOptPassManager->add(llvm::createInstructionCombiningPass());  // Clean up after the unroller
  fOptPassManager->add(llvm::createGVNPass());                   // Remove redundancies
  fOptPassManager->add(llvm::createMemCpyOptPass());             // Remove memcpy / form memset
  fOptPassManager->add(llvm::createSCCPPass());                  // Constant prop with SCCP
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
CodeGenerator::compileToCode(const CompileUnitNode* node,
                             const String& outputFile)
{
  node->codegen(this);

  emitClassInitFunc();

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

static std::vector<const llvm::Type*>
newLlvmTypeVector(const llvm::Type* ty)
{
  std::vector<const llvm::Type*> v;
  v.push_back(ty);
  return v;
}


static std::vector<const llvm::Type*>
newLlvmTypeVector(const llvm::Type* ty1, const llvm::Type* ty2)
{
  std::vector<const llvm::Type*> v;
  v.push_back(ty1);
  v.push_back(ty2);
  return v;
}


//------------------------------------------------------------------------------

const llvm::Type*
CodeGenerator::getAtomType()
{
  llvm::StringRef typeName("struct.ATOM");

  const llvm::Type* atomType = fModule->getTypeByName(typeName);
  if (atomType == NULL) {
    const llvm::Type* payloadType = NULL;
    if (fModule->getPointerSize() == llvm::Module::Pointer64)
      payloadType = llvm::Type::getInt64Ty(context());
    else
      payloadType = llvm::Type::getInt32Ty(context());

    const llvm::StructType* payloadStruct =
      llvm::StructType::get(context(), newLlvmTypeVector(payloadType), false);
    atomType =
      llvm::StructType::get(context(),
                            newLlvmTypeVector(llvm::Type::getInt32Ty(context()),
                                              payloadStruct),
                            false);
    fModule->addTypeName(llvm::StringRef("union.AtomPayload"), payloadStruct);
    fModule->addTypeName(typeName, atomType);
  }

  return atomType;
}


const llvm::Type*
CodeGenerator::getType(const Type& type)
{
  if (type.typeName() == String("clang|int")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("clang|char")) {
    return llvm::Type::getInt8Ty(context());
  }

  return getAtomType();
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::wrapLoad(llvm::Value* val)
{
  if (val != NULL && llvm::AllocaInst::classof(val))
    return fBuilder.CreateLoad(val);
  return val;
}


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

  String appMainFuncNm = herschel::mangleToC(String("app|main"));
  llvm::Function* appMainFunc = fModule->getFunction(llvm::StringRef(appMainFuncNm));
  assert(appMainFunc != NULL);

  llvm::AllocaInst* retv = createEntryBlockAlloca(func, String("tmp2"));
  std::vector<llvm::Value*> argv;
  argv.push_back(retv);
  fBuilder.CreateCall(appMainFunc, argv.begin(), argv.end());

  llvm::Value* retv2 = fBuilder.CreateLoad(retv);
  fBuilder.CreateRet(makeTypeCastAtomToClangInt(retv2));

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);
}


//------------------------------------------------------------------------------

llvm::AllocaInst*
CodeGenerator::createEntryBlockAlloca(llvm::Function *func, const String& name)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(getAtomType(), //llvm::Type::getInt32Ty(context()),
                          0, llvm::Twine(name));
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == String("lang|unspecified")) {
    // TODO
    return makeIntAtom(0);
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
    logf(kError, "Unknown symbol '%s'", (const char*)StrHelper(node->name()));
    return NULL;
  }

  return val;
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
  // FIXME: Type coercing of void()* types.
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
  String varnm = herschel::mangleToC(node->name());
  llvm::GlobalVariable* gv =
    new llvm::GlobalVariable(getAtomType(), //llvm::Type::getInt32Ty(context()),
                             false, // isConstant,
                             llvm::GlobalValue::ExternalLinkage,
                             llvm::ConstantInt::get(context(),
                                                    // TODO
                                                    llvm::APInt(32, 1013, true)),
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
  String funcnm = herschel::mangleToC(tmpName);

  llvm::Function *func = createGlobalInitOrDtorFunction(ft, funcnm);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  llvm::Value* initval = NULL;
  if (node->initExpr() != NULL) {
    initval = codegenNode(node->initExpr());
  }
  else {
    assert(0 && "no initval");
    // TODO: init the temporary value.  We shouldn't really have to care about
    // this here, since this can be better done in the AST analysis.
    // initval = llvm::ConstantInt::get(context(),
    //                                  llvm::APInt(32, 1011, true));
  }

  fBuilder.CreateStore(wrapLoad(initval), gv);
  fBuilder.CreateRetVoid();

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);

  addGlobalCtor(func, 2);

  assert(fGlobalVariables.find(node->name()) == fGlobalVariables.end());
  fGlobalVariables[node->name()] = gv;

  return initval;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  if (!isLocal)
    return codegenForGlobalVars(node);

  llvm::Value* initval = NULL;
  if (node->initExpr() != NULL) {
    initval = codegenNode(node->initExpr());
  }
  else {
    assert(0 && "no initval");
    // TODO: init the temporary value.  We shouldn't really have to care about
    // this here, since this can be better done in the AST analysis.
    // initval = llvm::ConstantInt::get(context(),
    //                                  llvm::APInt(32, 1014, true));
  }

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::AllocaInst* stackSlot = createEntryBlockAlloca(curFunction,
                                                       node->name());
  assignAtom(initval, stackSlot);

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
      logf(kError, "Unknown symbol '%s'", (const char*)StrHelper(lsym->name()));
      return NULL;
    }

    assignAtom(rvalue, var);
    // fBuilder.CreateStore(wrapLoad(rvalue), var);
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
    return codegen(vardefNode, true);

  const FuncDefNode* funcDefNode = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (funcDefNode != NULL) {
    logf(kError, "Compiling local functions not supported yet: %s", __FUNCTION__);
    return NULL;
  }

  return NULL;
}


//------------------------------------------------------------------------------

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
CodeGenerator::makeIntAtom(int val)
{
  return makeIntAtom(llvm::ConstantInt::get(context(),
                                            llvm::APInt(32, val, true)));
}


llvm::Value*
CodeGenerator::makeIntAtom(llvm::Value* val)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("int"));

  // set typeid
  setAtom(atom, kAtomInt, val);

  return atom;
}


llvm::Value*
CodeGenerator::makeBoolAtom(llvm::Value* val)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("bool"));

  // set typeid
  setAtom(atom, kAtomBool, val);

  return atom;
}


llvm::Value*
CodeGenerator::makeBoolAtom(bool val)
{
  if (val)
    return makeBoolAtom(llvm::ConstantInt::getTrue(context()));
  else
    return makeBoolAtom(llvm::ConstantInt::getFalse(context()));
}


llvm::Value*
CodeGenerator::codegen(const IntNode* node)
{
  return makeIntAtom(node->value());
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


llvm::FunctionType*
CodeGenerator::createFunctionSignature2(const FunctionNode* node, bool inlineRetv)
{
  std::vector<const llvm::Type*> sign;

  if (inlineRetv) {
    sign.push_back(getAtomType()->getPointerTo());
  }

  bool isVarArgs = false;
  for (size_t pidx = 0; pidx < node->params().size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
    // TODO
    if (param->isRestArg())
      isVarArgs = true;
    else
      sign.push_back(getType(param->type()));
  }

  llvm::FunctionType *ft = llvm::FunctionType::get(( inlineRetv
                                                     ? llvm::Type::getVoidTy(context())
                                                     : getType(node->retType()) ),
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

  bool inlineRetv = false;
  String funcnm;
  if (node->linkage() == String("C")) {
    funcnm = node->name();
    inlineRetv = false;
  }
  else {
    funcnm = herschel::mangleToC(node->name());
    inlineRetv = true;
  }

  llvm::FunctionType* ft = createFunctionSignature2(node, inlineRetv);
  assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                llvm::Twine(funcnm),
                                                fModule);

  if (node->body() != NULL) {
    llvm::Function::arg_iterator aiter = func->arg_begin();
    llvm::Function::arg_iterator aiter_e = func->arg_end();

    if (inlineRetv)
      aiter++;

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", func);
    fBuilder.SetInsertPoint(bb);

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

    const BlockNode* blockNode = dynamic_cast<const BlockNode*>(node->body());
    llvm::Value* retv = NULL;
    if (blockNode != NULL) {
      retv = codegen(blockNode->children());
    }
    else {
      llvm::Value* val = codegenNode(node->body());
      if (val == NULL)
        return NULL;
      retv = val;
    }

    if (inlineRetv) {
      // no wrap-load!
      assignAtom(retv, func->arg_begin());
      fBuilder.CreateRetVoid();
    }
    else
      fBuilder.CreateRet(wrapLoad(retv));

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
CodeGenerator::makeClassRegisterCall(const String& typeName, bool instantiable,
                                     int isize)
{
  llvm::Function* regFunc = fModule->getFunction(llvm::StringRef("class_register"));
  if (regFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(llvm::Type::getInt8PtrTy(context()));
    sign.push_back(llvm::Type::getInt1Ty(context()));
    sign.push_back(llvm::Type::getInt32Ty(context()));

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(context()),
                                                     sign,
                                                     false);

    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("class_register"),
                                     fModule);
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(fBuilder.CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("str")));
  argv.push_back(instantiable
                 ? llvm::ConstantInt::getTrue(context())
                 : llvm::ConstantInt::getFalse(context()));
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, isize, true)));
  return fBuilder.CreateCall(regFunc, argv.begin(), argv.end(), "callreg");
}


llvm::Value*
CodeGenerator::makeTypeCastAtomToClangInt(llvm::Value* val)
{
  llvm::Function* convFunc = fModule->getFunction(llvm::StringRef("atom_2_int"));
  if (convFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(getAtomType());

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt32Ty(context()),
                                                     sign,
                                                     false);

    convFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine("atom_2_int"),
                                      fModule);
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(val);
  return fBuilder.CreateCall(convFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::makeTypeCastAtomToClangChar(llvm::Value* val)
{
  printf("->clang|char\n");
  return val;
}


llvm::Value*
CodeGenerator::makeTypeCastAtomToClangBool(llvm::Value* val)
{
  std::vector<const llvm::Type*> sign;
  sign.push_back(getAtomType());

  llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt1Ty(context()),
                                                   sign,
                                                   false);

  llvm::Function* convFunc = fModule->getFunction(llvm::StringRef("atom_2_bool"));
  if (convFunc == NULL) {
    convFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine("atom_2_bool"),
                                      fModule);
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(val);
  return fBuilder.CreateCall(convFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;
  const FuncDefNode* fdn = NULL;
  bool inlineRetv = false;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  if (symNode != NULL) {
    assert(symNode->refersTo() == kFunction || symNode->refersTo() == kGeneric);

    // TODO handle generic function calls
    const AptNode* fn = symNode->scope()->lookupFunction(symNode->name(), false);
    fdn = dynamic_cast<const FuncDefNode*>(fn);
    assert(fdn != NULL);

    String funcnm;
    if (fdn->linkage() == String("C")) {
      funcnm = symNode->name();
      inlineRetv = false;
    }
    else {
      funcnm = herschel::mangleToC(symNode->name());
      inlineRetv = true;
    }

    // bool alwaysPassAtom = fdn->isGeneric() || fdn->isMethod();

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

  const NodeList& nl = node->children();
  if (calleeFunc->arg_size() != nl.size() + (inlineRetv ? 1 : 0)) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  llvm::AllocaInst* retv = NULL;
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  retv = createEntryBlockAlloca(curFunction, String("local_retv"));

  std::vector<llvm::Value*> argv;
  if (inlineRetv)
    argv.push_back(retv);

  for (unsigned i = 0, e = nl.size(); i != e; ++i) {
    llvm::Value* val = wrapLoad(codegenNode(nl[i]));

    if (fdn->params()[i]->type().typeName() == String("clang|int")) {
      val = makeTypeCastAtomToClangInt(val);
    }
    else if (fdn->params()[i]->type().typeName() == String("clang|char")) {
      val = makeTypeCastAtomToClangChar(val);
    }

    if (val != NULL)
      argv.push_back(val);
    else
      return NULL;
  }

  if (inlineRetv) {
    fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end());
    return retv;
  }
  else {
    llvm::Value* funcVal = fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end(),
                                               "xxx");
    if (node->isInTailPos()) {
      // TODO: return type id
      setAtom(retv, kAtomInt, funcVal);
      return retv;
    }
    else
      return funcVal;
  }
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
  llvm::Value *left = wrapLoad(codegenNode(node->left()));
  llvm::Value *right = wrapLoad(codegenNode(node->right()));
  if (left == NULL || right == NULL)
    return NULL;

  if (node->left()->type().isAnyInt() && node->right()->type().isAnyInt()) {
    llvm::Value* li = makeTypeCastAtomToClangInt(left);
    llvm::Value* ri = makeTypeCastAtomToClangInt(right);
    llvm::Value* rv = NULL;

    switch (node->op()) {
    case kOpPlus:     return makeIntAtom(fBuilder.CreateAdd(li, ri, "addtmp"));
    case kOpMinus:    return makeIntAtom(fBuilder.CreateSub(li, ri, "subtmp"));
    case kOpMultiply: return makeIntAtom(fBuilder.CreateMul(li, ri, "multmp"));
    case kOpLess:     return makeBoolAtom(fBuilder.CreateICmpULT(li, ri, "lttmp"));
    case kOpLessEqual: return makeBoolAtom(fBuilder.CreateICmpULE(li, ri, "letmp"));
    case kOpEqual:    return makeBoolAtom(fBuilder.CreateICmpEQ(li, ri, "eqtmp"));
    case kOpUnequal:  return makeBoolAtom(fBuilder.CreateICmpNE(li, ri, "netmp"));
    case kOpGreater:  return makeBoolAtom(fBuilder.CreateICmpUGT(li, ri, "gttmp"));
    case kOpGreaterEqual:
      return makeBoolAtom(fBuilder.CreateICmpUGE(li, ri, "getmp"));
    default:
      fprintf(stderr, "invalid binary operator: %d", node->op());
      return NULL;
    }

    return rv;
  }

  tyerror(node->left()->type(), "unsupported type in binary operator");
  tyerror(node->right()->type(), "unsupported type in binary operator");
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const NegateNode* node)
{
  llvm::Value *base = wrapLoad(codegenNode(node->base()));
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
  llvm::Value *testValue = wrapLoad(codegenNode(node->test()));
  if (testValue == NULL)
    return NULL;

  llvm::Value* extrTestVal = makeTypeCastAtomToClangBool(testValue);
  // Convert condition to a bool by comparing equal to 1
  testValue = fBuilder.CreateICmpEQ(extrTestVal,
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

  llvm::Value *thenValue = wrapLoad(codegenNode(node->consequent()));
  if (thenValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'then' can change the current block, update thenBB for the PHI.
  thenBB = fBuilder.GetInsertBlock();


  // Emit else block.
  curFunction->getBasicBlockList().push_back(elseBB);
  fBuilder.SetInsertPoint(elseBB);

  llvm::Value *elseValue = wrapLoad(codegenNode(node->alternate()));
  if (elseValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = fBuilder.GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  fBuilder.SetInsertPoint(mergeBB);
  llvm::PHINode *pn = fBuilder.CreatePHI(getAtomType(), //llvm::Type::getInt32Ty(context()),
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
CodeGenerator::codegen(const TypeDefNode* node)
{
  fClassInitFuncs.push_back(node);
  return NULL;
}


void
CodeGenerator::emitClassInitFunc()
{
  const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         false);
  String tmpName = uniqueName("classreg");
  String funcnm = herschel::mangleToC(tmpName);

  llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
  fBuilder.SetInsertPoint(bb);

  for (size_t i = 0; i < fClassInitFuncs.size(); i++) {
    const TypeDefNode* tdnode = fClassInitFuncs[i];
    makeClassRegisterCall(herschel::mangleToC(tdnode->name()),
                          tdnode->isClass(),
                          42);
  }
  fBuilder.CreateRetVoid();

  verifyFunction(*regFunc);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*regFunc);

  addGlobalCtor(regFunc, 1);
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

  llvm::Value *testValue = wrapLoad(codegenNode(node->test()));
  if (testValue == NULL)
    return NULL;

  llvm::Value* extrTestVal = makeTypeCastAtomToClangBool(testValue);
  // Convert condition to a bool by comparing equal to 1
  testValue = fBuilder.CreateICmpEQ(extrTestVal,
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

  return bodyValue;
}


llvm::Value*
CodeGenerator::codegen(const CastNode* node)
{
  logf(kError, "Not supported yet: %s", __FUNCTION__);
  // TODO
  return NULL;
}


//------------------------------------------------------------------------------

llvm::Function*
CodeGenerator::getIntrinsic(unsigned int iid,
                            const llvm::Type** tys, unsigned int numTys)
{
  return llvm::Intrinsic::getDeclaration(fModule,
                                         (llvm::Intrinsic::ID)iid, tys, numTys);
}


llvm::Function*
CodeGenerator::getMemCpyFn(const llvm::Type* dstType,
                           const llvm::Type* srcType,
                           const llvm::Type* sizeType)
{
  const llvm::Type* argTypes[3] = { dstType, srcType, sizeType };
  return getIntrinsic(llvm::Intrinsic::memcpy, argTypes, 3);
}


void
CodeGenerator::setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value)
{
  llvm::Value* typidSlot = fBuilder.CreateStructGEP(atom, 0);
  fBuilder.CreateStore(llvm::ConstantInt::get(context(),
                                              llvm::APInt(32, (int)typid, true)),
                       typidSlot);

  llvm::Value* payload = fBuilder.CreateStructGEP(atom, 1);
  llvm::Value* slot = fBuilder.CreateStructGEP(payload, 0);

  if (typid == kAtomBool) {
    const llvm::Type *dstBasePtr = llvm::Type::getInt1PtrTy(context());
    slot = fBuilder.CreateBitCast(slot, dstBasePtr, "tmp");
  }

  fBuilder.CreateStore(value, slot);
}


void
CodeGenerator::assignAtom(llvm::Value* src, llvm::Value* dst)
{
  const llvm::Type* dstBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* dst2 = fBuilder.CreateBitCast(dst, dstBasePtr, "tmp");

  const llvm::Type *srcBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* src2 = fBuilder.CreateBitCast(src, srcBasePtr, "tmp");

  std::vector<llvm::Value*> argv;
  argv.push_back(dst2);
  argv.push_back(src2);
  // number
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, 8, true)));
  // align
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, 4, true)));
  // is volatile
  argv.push_back(llvm::ConstantInt::getFalse(context()));


  fBuilder.CreateCall(getMemCpyFn(dst2->getType(), src2->getType(),
                                  llvm::Type::getInt32Ty(context())),
                      argv.begin(), argv.end());

}


//------------------------------------------------------------------------------

llvm::LLVMContext&
CodeGenerator::context()
{
  return fContext;
}
