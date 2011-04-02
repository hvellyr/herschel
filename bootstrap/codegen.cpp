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
#include "xmlout.h"
#include "predefined.h"

#include <vector>
#include <typeinfo>

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
//#include "llvm/DIBuilder.h"

using namespace herschel;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator()
  : fContext(llvm::getGlobalContext()),
    fModule(NULL),
    // fDIBuilder(NULL),
    fBuilder(context()),
    fOptPassManager(NULL),
    fHasMainFunc(false)
{
  llvm::InitializeNativeTarget();

  static llvm::ExecutionEngine *theExecutionEngine = NULL;

  fModule = new llvm::Module("compile-unit", fContext);

  // fDIBuilder = new llvm::DIBuilder(fModule);

  // fDIBuilder->createCompileUnit(dwarf::DW_LANG_C99,
  //                               "hello-world.c",
  //                               "/Users/gck/Dev/herschel/tests",
  //                               "herschel 1.0",
  //                               true, // isOptimized,
  //                               "",
  //                               0); // obj-c runtime version

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
  // fOptPassManager->add(llvm::createLoopIndexSplitPass());        // Split loop index
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
  emitGlobalVarInitFunc();

  emitCtorList(fGlobalCtors, "llvm.global_ctors");
  emitCtorList(fGlobalDtors, "llvm.global_dtors");

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

  else if (type.typeName() == String("lang|Int8")) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (type.typeName() == String("lang|Int16")) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (type.typeName() == String("lang|Int32")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|Int64")) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (type.typeName() == String("lang|UInt8")) {
    return llvm::Type::getInt8Ty(context());
  }
  else if (type.typeName() == String("lang|UInt16")) {
    return llvm::Type::getInt16Ty(context());
  }
  else if (type.typeName() == String("lang|UInt32")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|UInt64")) {
    return llvm::Type::getInt64Ty(context());
  }

  else if (type.typeName() == String("lang|Char")) {
    return llvm::Type::getInt32Ty(context());
  }
  else if (type.typeName() == String("lang|Bool")) {
    return llvm::Type::getInt1Ty(context());
  }

  else if (type.typeName() == String("lang|Float32")) {
    return llvm::Type::getFloatTy(context());
  }
  else if (type.typeName() == String("lang|Float64")) {
    return llvm::Type::getDoubleTy(context());
  }
  // else if (type.typeName() == String("lang|Float128")) {
  //   return llvm::Type::getInt1Ty(context());
  // }

  return getAtomType();
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::wrapLoad(llvm::Value* val)
{
  if (val != NULL) {
    if (llvm::AllocaInst::classof(val) ||
        llvm::GlobalValue::classof(val))
      return fBuilder.CreateLoad(val);
  }
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
  hr_assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string("main"),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  String appMainFuncNm = herschel::mangleToC(String("app|main"));
  llvm::Function* appMainFunc = fModule->getFunction(llvm::StringRef(appMainFuncNm));
  hr_assert(appMainFunc != NULL);

  llvm::AllocaInst* retv = createEntryBlockAlloca(func, String("tmp2"),
                                                  llvm::Type::getInt32Ty(context()));
  std::vector<llvm::Value*> argv;
  argv.push_back(retv);
  fBuilder.CreateCall(appMainFunc, argv.begin(), argv.end());

  llvm::Value* retv2 = fBuilder.CreateLoad(retv);
  fBuilder.CreateRet(retv2);

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);
}


//------------------------------------------------------------------------------

llvm::AllocaInst*
CodeGenerator::createEntryBlockAlloca(llvm::Function *func, const String& name,
                                      const llvm::Type* type)
{
  llvm::IRBuilder<> tmp(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tmp.CreateAlloca(type, 0, llvm::Twine(name));
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == String("lang|unspecified")) {
    // TODO
    return llvm::Constant::getNullValue(getType(node->type()));
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
  llvm::Type* ctorPFTy = llvm::PointerType::getUnqual(ctorFTy);

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
  const llvm::Type* constTy = getType(node->type());
  llvm::Constant* initConst = llvm::Constant::getNullValue(constTy);

  // TODO: base type if possible
  llvm::GlobalVariable* gv =
    new llvm::GlobalVariable(constTy,
                             false, // isConstant,
                             llvm::GlobalValue::ExternalLinkage,
                             initConst,
                             llvm::Twine(varnm),
                             false, // ThreadLocal
                             0);    // AddressSpace
  hr_assert(gv != NULL);
  fModule->getGlobalList().push_back(gv);

  fGlobalInitVars.push_back(node);

  hr_assert(fGlobalVariables.find(node->name()) == fGlobalVariables.end());
  fGlobalVariables[node->name()] = gv;

  return gv;
}


void
CodeGenerator::emitGlobalVarInitFunc()
{
  if (!fGlobalInitVars.empty())
  {
    const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                           false);
    String tmpName = uniqueName("gvinit");
    String funcnm = herschel::mangleToC(tmpName);

    llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
    fBuilder.SetInsertPoint(bb);

    for (size_t i = 0; i < fGlobalInitVars.size(); i++) {
      const VardefNode* varnode = fGlobalInitVars[i];

      String varnm = herschel::mangleToC(varnode->name());

      llvm::Value* initval = NULL;
      if (varnode->initExpr() != NULL) {
        initval = codegenNode(varnode->initExpr());
      }
      else {
        hr_invalid("no initval");
        // TODO: init the temporary value.  We shouldn't really have to care about
        // this here, since this can be better done in the AST analysis.
        // initval = llvm::ConstantInt::get(context(),
        //                                  llvm::APInt(32, 1011, true));
      }

      std::map<String, llvm::GlobalVariable*>::iterator it =
      fGlobalVariables.find(varnode->name());
      hr_assert(it != fGlobalVariables.end());

      llvm::Value* val = emitPackCode(varnode->initExpr()->dstType(),
                                      varnode->initExpr()->typeConv(),
                                      initval, varnode->initExpr()->type());
      fBuilder.CreateStore(val, it->second);
    }
    fBuilder.CreateRetVoid();

    verifyFunction(*regFunc);

    if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fOptPassManager->run(*regFunc);

    addGlobalCtor(regFunc, 2);
  }
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  if (!isLocal)
    return codegenForGlobalVars(node);

  llvm::Value* initval = NULL;
  Type dstType;
  Type type;
  TypeConvKind convKind = kNoConv;
  if (node->initExpr() != NULL) {
    if (dynamic_cast<UndefNode*>(node->initExpr())) {
      initval = llvm::Constant::getNullValue(getType(node->type()));

      dstType = node->type();
      type = node->type();
      convKind = kNoConv;
    }
    else {
      initval = wrapLoad(codegenNode(node->initExpr()));

      dstType = node->initExpr()->dstType();
      type = node->initExpr()->type();
      convKind = node->initExpr()->typeConv();
    }
  }
  else {
    hr_invalid("no initval");
    // TODO: init the temporary value.  We shouldn't really have to care about
    // this here, since this can be better done in the AST analysis.
    // initval = llvm::ConstantInt::get(context(),
    //                                  llvm::APInt(32, 1014, true));
  }

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::AllocaInst* stackSlot = createEntryBlockAlloca(curFunction,
                                                       node->name(),
                                                       getType(node->type()));

  llvm::Value* val = emitPackCode(dstType, convKind, initval, type);
  fBuilder.CreateStore(val, stackSlot);

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

    llvm::Value* val = emitPackCode(node->rvalue()->dstType(),
                                    node->rvalue()->typeConv(),
                                    wrapLoad(rvalue),
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
// TODO plaintype?
  if (node->dstType().isPlainType()) {
    if (node->value())
      return fBuilder.getTrue();
    else
      return fBuilder.getFalse();
  }

  return makeBoolAtom(node->value());
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
  logf(kError, "Not supported yet: %s", typeid(node).name());
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
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("int"),
                                                  getAtomType());

  // set typeid
  setAtom(atom, kAtomInt, val);

  return atom;
}


llvm::Value*
CodeGenerator::makeBoolAtom(llvm::Value* val)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("bool"),
                                                  getAtomType());

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
  return fBuilder.CreateIntCast(fBuilder.getInt32(node->value()),
                                getType(node->type()),
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


//------------------------------------------------------------------------------

llvm::FunctionType*
CodeGenerator::createFunctionSignature(const FunctionNode* node, bool inlineRetv,
                                       const Type& retty)
{
  std::vector<const llvm::Type*> sign;

  if (inlineRetv)
    sign.push_back(getType(retty)->getPointerTo());

  bool isVarArgs = false;
  for (size_t pidx = 0; pidx < node->params().size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
    // TODO
    if (param->isRestArg())
      isVarArgs = true;
    else
      sign.push_back(getType(param->type()));
  }

  return llvm::FunctionType::get(( inlineRetv
                                   ? llvm::Type::getVoidTy(context())
                                   : getType(node->retType()) ),
                                 sign,
                                 isVarArgs);
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

  Type retty;
  if (node->name() == String("app|main")) {
    retty = Type::newTypeRef(Names::kInt32TypeName, K(isValue));
  }
  else
    retty = node->retType();


  llvm::FunctionType* ft = createFunctionSignature(node, inlineRetv, retty);
  hr_assert(ft != NULL);

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
      llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func, param->name(),
                                                           getType(param->type()));
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
      if (node->name() == String("app|main")) {
        // the app|main function always returns lang|Int32
        if (node->body()->type().isPlainType()) {
          fBuilder.CreateStore(fBuilder.CreateIntCast(wrapLoad(retv),
                                                      llvm::Type::getInt32Ty(context()),
                                                      true),
                               func->arg_begin());
        }
        else {
          llvm::Value* convertedRetv = makeTypeCastAtomToPlain(wrapLoad(retv),
                                                               Type::newTypeRef("clang|int"));
          fBuilder.CreateStore(convertedRetv, func->arg_begin());
        }
      }
// TODO plaintype?
      else if (retty.isPlainType()) {
        fBuilder.CreateStore(wrapLoad(retv), func->arg_begin());
      }
      else {
        // no wrap-load!
        assignAtom(retv, func->arg_begin());
      }

      fBuilder.CreateRetVoid();
    }
    else
      fBuilder.CreateRet(wrapLoad(retv));

    if (Properties::isCodeDump())
      func->dump();

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
  logf(kError, "Not supported yet: %s", typeid(node).name());
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


static const char*
getConvFuncNameByType(const Type& type)
{
  if (type.typeName() == String("lang|Int32"))
    return "atom_2_int32";
  else if (type.typeName() == String("lang|Int64"))
    return "atom_2_int64";
  else if (type.typeName() == String("lang|Int16"))
    return "atom_2_int16";
  else if (type.typeName() == String("lang|Int8"))
    return "atom_2_int8";
  else if (type.typeName() == String("lang|UInt32"))
    return "atom_2_uint32";
  else if (type.typeName() == String("lang|UInt64"))
    return "atom_2_uint64";
  else if (type.typeName() == String("lang|UInt16"))
    return "atom_2_uint16";
  else if (type.typeName() == String("lang|UInt8"))
    return "atom_2_uint8";
  else if (type.typeName() == String("lang|Float32"))
    return "atom_2_float32";
  else if (type.typeName() == String("lang|Float64"))
    return "atom_2_float64";
  else if (type.typeName() == String("lang|Char"))
    return "atom_2_char";
  else if (type.typeName() == String("lang|Bool"))
    return "atom_2_bool";

  if (type.typeName() == String("clang|int")) // TODO
    return "atom_2_int32";

  hr_invalid("unhandled type");
  return NULL;
}


llvm::Value*
CodeGenerator::makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType)
{
  const char* funcName = getConvFuncNameByType(dstType);

  llvm::Function* convFunc = fModule->getFunction(llvm::StringRef(funcName));
  if (convFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(getAtomType());

    llvm::FunctionType *ft = llvm::FunctionType::get(getType(dstType),
                                                     sign,
                                                     false);

    convFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fModule);
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(val);
  return fBuilder.CreateCall(convFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::emitPackCode(const Type& dstType, TypeConvKind convKind,
                            llvm::Value* value,
                            const Type& valType)
{
  if (dstType.isDef()) {
    // fprintf(stderr, "-----------------------\n");
    // fprintf(stderr, "type conv: %d\n", convKind);
    // tyerror(dstType, "dstType");
    // tyerror(valType, "valType");
    // fprintf(stderr, "Value to emit: "); value->dump();

    switch (convKind) {
    case kNoConv:
      return value;
    case kAtom2PlainConv:
      return makeTypeCastAtomToPlain(value, dstType);
    case kPlain2AtomConv:
      if (valType.typeName() == String("lang|Int32"))
        return wrapLoad(makeIntAtom(value));
      else if (valType.typeName() == String("lang|Bool"))
        return wrapLoad(makeBoolAtom(value));
      //return value;

    case kTypeCheckConv:
      fprintf(stderr, "Not implemented yet\n");
      hr_invalid("not implemented yet");
    }
  }

  return value;
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;
  bool inlineRetv = false;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  if (symNode != NULL) {
    hr_assert(symNode->refersTo() == kFunction || symNode->refersTo() == kGeneric);

    String funcnm;
    if (symNode->linkage() == String("C")) {
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
    hr_invalid("apply(!symbol) -> TODO");
  }

  const NodeList& nl = node->children();
  if (calleeFunc->arg_size() != nl.size() + (inlineRetv ? 1 : 0)) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  llvm::AllocaInst* retv = NULL;
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  retv = createEntryBlockAlloca(curFunction, String("local_retv"),
                                getType(node->type()));

  std::vector<llvm::Value*> argv;
  if (inlineRetv)
    argv.push_back(retv);

  for (unsigned i = 0, e = nl.size(); i != e; ++i) {
    llvm::Value* val = wrapLoad(codegenNode(nl[i]));

    // warningf(nl[i]->srcpos(), 0, "emit pack code");
    val = emitPackCode(nl[i]->dstType(), nl[i]->typeConv(),
                       val, nl[i]->type());

    // val->dump();

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


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const BinaryNode* node)
{
  // fprintf(stderr, "BinaryNode: %s [%d]\n", XmlRenderer::operatorName(node->op()),
  //         node->typeConv());
  // tyerror(node->type(), "type");
  // tyerror(node->dstType(), "dsttype");
  llvm::Value *left = wrapLoad(codegenNode(node->left()));
  llvm::Value *right = wrapLoad(codegenNode(node->right()));
  if (left == NULL || right == NULL)
    return NULL;

  /*
    int -> to plain int, op, dsttype is atom -> make_int_atom
    float -> to plain float, op, dsttype is atom -> make_float_atom
    char -> to plain float, op, dsttype is atom -> make_char_atom
    bool -> to plain bool, op, dsttype is atom -> make_bool_atom
    atom -> call operator(), dsttype is plain -> make_plain
  */

  if (node->left()->type().isAnyInt() && node->right()->type().isAnyInt()) {
    return codegenOpIntInt(node, left, right);
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

  llvm::Value* extrTestVal = emitPackCode(node->test()->dstType(),
                                          node->test()->typeConv(),
                                          testValue,
                                          node->test()->type());

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
  llvm::Value* thenValue2 = emitPackCode(node->consequent()->dstType(),
                                         node->consequent()->typeConv(),
                                         thenValue,
                                         node->consequent()->type());

  fBuilder.CreateBr(mergeBB);
  // Get a reference to the current thenBB, since codegen of 'then' can change
  // the current block, update thenBB for the PHI.
  thenBB = fBuilder.GetInsertBlock();


  // Emit else block.
  curFunction->getBasicBlockList().push_back(elseBB);
  fBuilder.SetInsertPoint(elseBB);

  llvm::Value* elseValue = NULL;
  if (node->alternate() != NULL) {
    llvm::Value* elseValue0 = wrapLoad(codegenNode(node->alternate()));
    if (elseValue0 == NULL)
      return NULL;
    elseValue = emitPackCode(node->alternate()->dstType(),
                             node->alternate()->typeConv(),
                             elseValue0,
                             node->alternate()->type());
  }
  else
    elseValue = llvm::Constant::getNullValue(getType(node->type()));

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = fBuilder.GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  fBuilder.SetInsertPoint(mergeBB);

  llvm::PHINode *pn = fBuilder.CreatePHI(getType(node->type()), "iftmp");

  pn->addIncoming(thenValue2, thenBB);
  pn->addIncoming(elseValue, elseBB);

  return pn;
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
  fClassInitFuncs.push_back(node);
  return NULL;
}


void
CodeGenerator::emitClassInitFunc()
{
  if (!fClassInitFuncs.empty())
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

  llvm::Value* extrTestVal = emitPackCode(node->test()->dstType(),
                                          node->test()->typeConv(),
                                          testValue,
                                          node->test()->type());

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
  llvm::Value *val = wrapLoad(codegenNode(node->base()));
  if (val == NULL)
    return NULL;

  return emitPackCode(node->base()->dstType(),
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
