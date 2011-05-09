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
#include "llvm/Target/TargetRegistry.h"
//#include "llvm/DIBuilder.h"

using namespace herschel;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator()
  : fContext(llvm::getGlobalContext()),
    fModule(NULL),
    // fDIBuilder(NULL),
    fBuilder(context()),
    fOptPassManager(NULL),
    fTargetData(NULL),
    fInitializer(this),
    fTypes(this),
    fHasMainFunc(false)
{
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

  fTargetData = new llvm::TargetData(fModule);

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


bool
CodeGenerator::compileToCode(const CompileUnitNode* node,
                             const String& outputFile)
{
  node->codegen(this);

  fInitializer.finish();

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
    return llvm::Constant::getNullValue(fTypes.getType(node->type()));
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
CodeGenerator::codegenForGlobalVars(const VardefNode* node)
{
  String varnm = herschel::mangleToC(node->name());
  const llvm::Type* constTy = fTypes.getType(node->type());
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

  fInitializer.addGlobalVariable(node);

  hr_assert(fGlobalVariables.find(node->name()) == fGlobalVariables.end());
  fGlobalVariables[node->name()] = gv;

  return gv;
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
      initval = llvm::Constant::getNullValue(fTypes.getType(node->type()));

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
                                                       fTypes.getType(node->type()));

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
CodeGenerator::makeInt32Atom(int val)
{
  return makeIntAtom(llvm::ConstantInt::get(context(),
                                            llvm::APInt(32, val, true)),
                     kAtomInt32);
}


llvm::Value*
CodeGenerator::makeIntAtom(llvm::Value* val, Typeid atomTypeId)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("int"),
                                                  fTypes.getAtomType());

  setAtom(atom, atomTypeId, val);

  return atom;
}


llvm::Value*
CodeGenerator::makeBoolAtom(llvm::Value* val)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* atom = createEntryBlockAlloca(curFunction, String("bool"),
                                                  fTypes.getAtomType());

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
                                fTypes.getType(node->type()),
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

llvm::Value*
CodeGenerator::codegen(const FuncDefNode* node, bool isLocal)
{
  // TODO: nested functions need special treatment here.  Or even better:
  // avoid nested functions by lambda lifting in the transform passes above.
  fNamedValues.clear();

  if (node->isGeneric()) {
    hr_assert(!isLocal);
    return compileGenericFunctionDef(node);
  }
  else if (node->isMethod()) {
    hr_assert(!isLocal);
    return compileMethodDef(node);
  }
  else if (node->isAbstract()) {
    hr_assert(!isLocal);
    return compileAbstractFuncDef(node);
  }
  else {
    return compileNormalFuncDef(node, isLocal);
  }
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return NULL;
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

  hr_invalid((const char*)StrHelper(String("unhandled type: ") + type.typeId()));
  return NULL;
}


llvm::Value*
CodeGenerator::makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType)
{
  const char* funcName = getConvFuncNameByType(dstType);

  llvm::Function* convFunc = fModule->getFunction(llvm::StringRef(funcName));
  if (convFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(fTypes.getAtomType());

    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes.getType(dstType),
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
        return wrapLoad(makeIntAtom(value, kAtomInt32));
      else if (valType.typeName() == String("lang|Bool"))
        return wrapLoad(makeBoolAtom(value));
      // TODO
      //return value;

    case kTypeCheckConv:
      // fprintf(stderr, "Not implemented yet\n");
      // hr_invalid("not implemented yet");
      return value;
    }
  }

  return value;
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;
  bool inlineRetv = false;
  bool alwaysPassAtom = false;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  if (symNode != NULL) {
    hr_assert(symNode->refersTo() == kFunction || symNode->refersTo() == kGeneric);

    String funcnm;

    if (symNode->name() == Names::kLangAllocate)
      return emitAllocateApply(node);

    if (symNode->hasCLinkage()) {
      // generic functions are not allowed to have C linkage
      hr_assert(symNode->refersTo() != kGeneric);
      funcnm = symNode->name();
      inlineRetv = false;
    }
    else {
      funcnm = herschel::mangleToC(symNode->name());
      inlineRetv = true;
      alwaysPassAtom = symNode->refersTo() == kGeneric;
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
    hr_invalid("apply(!symbol) -> TODO");
  }

  const NodeList& args = node->children();
  if (calleeFunc->arg_size() != args.size() + (inlineRetv ? 1 : 0)) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  llvm::AllocaInst* retv = NULL;
  llvm::Function* curFunction = fBuilder.GetInsertBlock()->getParent();
  const llvm::Type* returnType = ( alwaysPassAtom
                                   ? fTypes.getAtomType()
                                   : fTypes.getType(node->type()) );
  retv = createEntryBlockAlloca(curFunction, String("local_retv"),
                                returnType);

  std::vector<llvm::Value*> argv;
  if (inlineRetv)
    argv.push_back(retv);

  for (unsigned i = 0, e = args.size(); i != e; ++i) {
    llvm::Value* val = wrapLoad(codegenNode(args[i]));

    // TODO: can we assert that spec args are ATOM typed.
    // warningf(args[i]->srcpos(), 0, "emit pack code");
    val = emitPackCode(args[i]->dstType(), args[i]->typeConv(),
                       val, args[i]->type());

    // val->dump();
    if (val == NULL)
      return NULL;

    argv.push_back(val);
  }

  if (inlineRetv) {
    fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end());

    // TODO: if in tail position enforce ATOM return type?
    return retv;
  }
  else {
    llvm::Value* funcVal = fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end(),
                                               "xxx");
    if (node->isInTailPos()) {
      // TODO: return type id
      setAtom(retv, kAtomInt32, funcVal);
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
    elseValue = llvm::Constant::getNullValue(fTypes.getType(node->type()));

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = fBuilder.GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  fBuilder.SetInsertPoint(mergeBB);

  llvm::PHINode *pn = fBuilder.CreatePHI(fTypes.getType(node->type()), "iftmp");

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
  fInitializer.addTypeDef(node);
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
  llvm::Value* payload = fBuilder.CreateStructGEP(atom, 1);
  llvm::Value* slot = fBuilder.CreateStructGEP(payload, 0);

  if (typid == kAtomBool) {
    const llvm::Type *dstBasePtr = llvm::Type::getInt1PtrTy(context());
    slot = fBuilder.CreateBitCast(slot, dstBasePtr, "tmp");
    fBuilder.CreateStore(value, slot);
  }
  else if (typid == kAtomInt32) {
    llvm::Value* val = ( is64Bit()
                         ? fBuilder.CreateIntCast(value,
                                                  llvm::Type::getInt64Ty(context()),
                                                  K(isSigned),
                                                  "tmp")
                         : value );
    fBuilder.CreateStore(val, slot);
  }
  else
    fBuilder.CreateStore(value, slot);


  llvm::Value* typidSlot = fBuilder.CreateStructGEP(atom, 0);
  llvm::Value* typeIdValue = NULL;
  if (is64Bit())
    typeIdValue =llvm::ConstantInt::get(context(),
                                        llvm::APInt(64, (int)typid, !K(isSigned)));
  else
    typeIdValue =llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, (int)typid, !K(isSigned)));

  fBuilder.CreateStore(typeIdValue, typidSlot);
}


void
CodeGenerator::assignAtom(llvm::Value* src, llvm::Value* dst)
{
  const llvm::Type* dstBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* dst2 = fBuilder.CreateBitCast(dst, dstBasePtr, "dst_tmp");

  const llvm::Type *srcBasePtr = llvm::Type::getInt8PtrTy(context());
  llvm::Value* src2 = fBuilder.CreateBitCast(src, srcBasePtr, "src_tmp");

  std::vector<llvm::Value*> argv;
  argv.push_back(dst2);
  argv.push_back(src2);
  // number
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, layout->getSizeInBytes(),
                                                    K(isSigned))));
  // align
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, layout->getAlignment(),
                                                    K(isSigned))));
  // is volatile
  argv.push_back(llvm::ConstantInt::getFalse(context()));


  fBuilder.CreateCall(getMemCpyFn(dst2->getType(), src2->getType(),
                                  llvm::Type::getInt32Ty(context())),
                      argv.begin(), argv.end());
}
