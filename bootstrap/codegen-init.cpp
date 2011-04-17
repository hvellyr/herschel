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


//----------------------------------------------------------------------------

#define K_RUNTIME_INIT_PRIORITY     0
#define K_CLASS_REG_PRIORITY        1
#define K_GLOBAL_VAR_REG_PRIORITY   2

//----------------------------------------------------------------------------

using namespace herschel;


ModuleRuntimeInitializer::ModuleRuntimeInitializer(CodeGenerator* generator)
  : fGenerator(generator)
{
}


void
ModuleRuntimeInitializer::finish()
{
  emitRuntimeInitFunc();
  emitClassInitFunc();
  emitGlobalVarInitFunc();
  emitTypeGetterFunctions();

  emitCtorList(fGlobalCtors, "llvm.global_ctors");
  emitCtorList(fGlobalDtors, "llvm.global_dtors");
}


//------------------------------------------------------------------------------

llvm::LLVMContext&
ModuleRuntimeInitializer::context()
{
  return fGenerator->fContext;
}


llvm::IRBuilder<>&
ModuleRuntimeInitializer::builder()
{
  return fGenerator->fBuilder;
}


llvm::Module*
ModuleRuntimeInitializer::module()
{
  return fGenerator->fModule;
}


CodegenTypeUtils&
ModuleRuntimeInitializer::types()
{
  return fGenerator->fTypes;
}


//! Add a function to the list that will be called before main() runs.
void
ModuleRuntimeInitializer::addGlobalCtor(llvm::Function* ctor, int priority)
{
  // FIXME: Type coercion of void()* types.
  fGlobalCtors.push_back(std::make_pair(ctor, priority));
}


//! Add a function to the list that will be called when the module is
//! unloaded.
void
ModuleRuntimeInitializer::addGlobalDtor(llvm::Function* dtor, int priority)
{
  // FIXME: Type coercing of void()* types.
  fGlobalDtors.push_back(std::make_pair(dtor, priority));
}


void
ModuleRuntimeInitializer::addGlobalVariable(const VardefNode* node)
{
  fGlobalInitVars.push_back(node);
}


void
ModuleRuntimeInitializer::addTypeDef(const TypeDefNode* node)
{
  fClassInitFuncs.push_back(node);
}


void
ModuleRuntimeInitializer::emitCtorList(const CtorList &fns, const char *globalName)
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
    new llvm::GlobalVariable(*module(), at, false,
                             llvm::GlobalValue::AppendingLinkage,
                             llvm::ConstantArray::get(at, ctors),
                             globalName);
  }
}


llvm::Function*
ModuleRuntimeInitializer::createGlobalInitOrDtorFunction(const llvm::FunctionType *ft,
                                                         const String& name)
{
  llvm::Function* fn =
  llvm::Function::Create(ft, llvm::GlobalValue::InternalLinkage,
                         llvm::Twine(name), module());

  // clang adds the following __TEXT,__StaticInit, etc. section to static
  // initializer functions.  Initialization however seems to work without
  // also.(?)

  // Set the section if needed.
  // if (const char* section = context().Target.getStaticInitSectionSpecifier())
  //   fn->setSection("__TEXT,__StaticInit,regular,pure_instructions");

  // fn->setDoesNotThrow();
  return fn;
}


void
ModuleRuntimeInitializer::emitGlobalVarInitFunc()
{
  if (!fGlobalInitVars.empty())
  {
    const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                           false);
    String tmpName = uniqueName("gvinit");
    String funcnm = herschel::mangleToC(tmpName);

    llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
    builder().SetInsertPoint(bb);

    for (size_t i = 0; i < fGlobalInitVars.size(); i++) {
      const VardefNode* varnode = fGlobalInitVars[i];

      String varnm = herschel::mangleToC(varnode->name());

      llvm::Value* initval = NULL;
      if (varnode->initExpr() != NULL) {
        initval = fGenerator->codegenNode(varnode->initExpr());
      }
      else {
        hr_invalid("no initval");
        // TODO: init the temporary value.  We shouldn't really have to care about
        // this here, since this can be better done in the AST analysis.
        // initval = llvm::ConstantInt::get(context(),
        //                                  llvm::APInt(32, 1011, true));
      }

      std::map<String, llvm::GlobalVariable*>::iterator it =
      fGenerator->fGlobalVariables.find(varnode->name());
      hr_assert(it != fGenerator->fGlobalVariables.end());

      llvm::Value* val = fGenerator->emitPackCode(varnode->initExpr()->dstType(),
                                                  varnode->initExpr()->typeConv(),
                                                  initval, varnode->initExpr()->type());
      builder().CreateStore(val, it->second);
    }
    builder().CreateRetVoid();

    verifyFunction(*regFunc);

    if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fGenerator->fOptPassManager->run(*regFunc);

    addGlobalCtor(regFunc, K_GLOBAL_VAR_REG_PRIORITY);
  }
}


void
ModuleRuntimeInitializer::emitRuntimeInitFunc()
{
  const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         false);
  String tmpName = uniqueName("rtinit");
  String funcnm = herschel::mangleToC(tmpName);

  llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
  builder().SetInsertPoint(bb);

  std::vector<const llvm::Type*> sign;

  llvm::FunctionType *ft2 = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                    sign,
                                                    !K(isVarArg));

  llvm::Function* rtinitFunc = llvm::Function::Create(ft2,
                                                      llvm::Function::ExternalLinkage,
                                                      llvm::Twine("runtime_init"),
                                                      module());

  std::vector<llvm::Value*> argv;
  builder().CreateCall(rtinitFunc, argv.begin(), argv.end());

  builder().CreateRetVoid();

  verifyFunction(*regFunc);

  if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fGenerator->fOptPassManager->run(*regFunc);

  addGlobalCtor(regFunc, K_RUNTIME_INIT_PRIORITY);
}


void
ModuleRuntimeInitializer::emitClassInitFunc()
{
  if (!fClassInitFuncs.empty())
  {
    const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         false);
    String tmpName = uniqueName("classreg");
    String funcnm = herschel::mangleToC(tmpName);

    llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
    builder().SetInsertPoint(bb);

    for (size_t i = 0; i < fClassInitFuncs.size(); i++) {
      const TypeDefNode* tdnode = fClassInitFuncs[i];
      makeTypeOrCallRegistration(tdnode);
    }
    builder().CreateRetVoid();

    verifyFunction(*regFunc);

    if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fGenerator->fOptPassManager->run(*regFunc);

    addGlobalCtor(regFunc, K_CLASS_REG_PRIORITY);
  }
}


//----------------------------------------------------------------------------

llvm::Value*
ModuleRuntimeInitializer::makeTypeLookupCall(const Type& ty)
{
  llvm::Function* lookupFunc = module()->getFunction(llvm::StringRef("type_lookup_by_name"));
  if (lookupFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(llvm::Type::getInt8PtrTy(context())); // char8*

    llvm::FunctionType *ft = llvm::FunctionType::get(types().getTypeType(),
                                                     sign,
                                                     !K(isVarArg));

    lookupFunc = llvm::Function::Create(ft,
                                        llvm::Function::ExternalLinkage,
                                        llvm::Twine("type_lookup_by_name"),
                                        module());
  }

  String typeName = herschel::mangleToC(ty.typeName());

  std::vector<llvm::Value*> argv;
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("typename")));

  return builder().CreateCall(lookupFunc, argv.begin(), argv.end()); //, "callreg");
}


llvm::Value*
ModuleRuntimeInitializer::makeIsaTypeLookupCall(const Type& ty)
{
  String typeClassLookupFuncName = String() + "get_" + ty.typeName() + "_type";
  String funcName = herschel::mangleToC(typeClassLookupFuncName);
  llvm::Function* typeFunc = module()->getFunction(llvm::StringRef(funcName));

  if (typeFunc == NULL)
  {
    std::vector<const llvm::Type*> sign;

    llvm::FunctionType *ft = llvm::FunctionType::get(types().getTypeType(),
                                                     sign,
                                                     !K(isVarArg));

    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      module());
  }

  std::vector<llvm::Value*> argv;
  return builder().CreateCall(typeFunc, argv.begin(), argv.end());
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeRegisterCall(llvm::Value* newType)
{
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("register_type"));
  if (regFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types().getTypeType()); // Type*

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("register_type"),
                                     module());
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(newType);

  builder().CreateCall(regFunc, argv.begin(), argv.end()); //, "callreg");
  return newType;
}


llvm::Value*
ModuleRuntimeInitializer::makeClassAllocCall(const Type& ty)
{
  const String& typeName = herschel::mangleToC(ty.typeName());

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("class_alloc"));
  if (allocFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(llvm::Type::getInt8PtrTy(context())); // typename
    sign.push_back(llvm::Type::getInt32Ty(context()));   // instance size
    sign.push_back(types().getTypeSlotPairType());       // const TypeSlotPair*
    sign.push_back(llvm::Type::getInt32Ty(context()));   // isa_size

    llvm::FunctionType *ft = llvm::FunctionType::get(types().getTypeType(),
                                                     sign,
                                                     K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("class_alloc"),
                                     module());

    //fTypesGetters.push_back(ty);
  }

  size_t instance_size = 0;

  std::vector<llvm::Value*> argv;
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("classname")));
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, instance_size, true)));
  // TODO
  argv.push_back(llvm::Constant::getNullValue(types().getTypeSlotPairType()));

  Type isa = ty.typeInheritance();
  if (isa.isSequence()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, isa.seqTypes().size(), true)));
    for (size_t i = 0; i < isa.seqTypes().size(); i++)
      argv.push_back(makeIsaTypeLookupCall(isa.seqTypes()[i]));
  }
  else if (isa.isDef()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 1, true)));
    argv.push_back(makeIsaTypeLookupCall(isa));
  }
  else {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 0, true)));
  }

  return builder().CreateCall(allocFunc, argv.begin(), argv.end(), "call_class_alloc");
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeAllocCall(const Type& ty)
{
  const String& typeName = herschel::mangleToC(ty.typeName());

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("type_alloc"));
  if (allocFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(llvm::Type::getInt8PtrTy(context())); // typename
    sign.push_back(llvm::Type::getInt32Ty(context()));   // isa_size

    llvm::FunctionType *ft = llvm::FunctionType::get(types().getTypeType(),
                                                     sign,
                                                     K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("type_alloc"),
                                     module());

    //fTypesGetters.push_back(ty);
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("typename")));

  Type isa = ty.typeInheritance();
  if (isa.isSequence()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, isa.seqTypes().size(), true)));
    for (size_t i = 0; i < isa.seqTypes().size(); i++)
      argv.push_back(makeIsaTypeLookupCall(isa.seqTypes()[i]));
  }
  else if (isa.isDef()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 1, true)));
    argv.push_back(makeIsaTypeLookupCall(isa));
  }
  else {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 0, true)));
  }

  return builder().CreateCall(allocFunc, argv.begin(), argv.end(), "call_type_alloc");
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeOrCallRegistration(const Type& ty)
{
  if (ty.isClass())
    return makeTypeRegisterCall(makeClassAllocCall(ty));

  return makeTypeRegisterCall(makeTypeAllocCall(ty));
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeOrCallRegistration(const TypeDefNode* tdnode)
{
  Type ty = tdnode->defType();

  fTypesGetters.push_back(ty);
  return makeIsaTypeLookupCall(ty);
}


void
ModuleRuntimeInitializer::emitTypeGetterFunctions()
{
  for (size_t i = 0; i < fTypesGetters.size(); i++) {
    const Type& ty = fTypesGetters[i];

    String typeClassLookupFuncName = String() + "get_" + ty.typeName() + "_type";
    String typeClassLookupGVName = ty.typeName() + "_type";

    String gvNm = herschel::mangleToC(typeClassLookupGVName);
    const llvm::Type* gvTy = llvm::Type::getInt8PtrTy(context());
    llvm::Constant* initGv = llvm::Constant::getNullValue(gvTy);

    llvm::GlobalVariable* gv = new llvm::GlobalVariable(gvTy,
                                                        !K(isConstant),
                                                        llvm::GlobalValue::InternalLinkage,
                                                        initGv,
                                                        llvm::Twine(gvNm),
                                                        !K(threadLocal),
                                                        0);    // AddressSpace
    hr_assert(gv != NULL);
    module()->getGlobalList().push_back(gv);


    String funcName = herschel::mangleToC(typeClassLookupFuncName);
    llvm::Function* typeFunc = module()->getFunction(llvm::StringRef(funcName));

    if (typeFunc == NULL)
    {
      std::vector<const llvm::Type*> sign;

      llvm::FunctionType *ft = llvm::FunctionType::get(types().getTypeType(),
                                                       sign,
                                                       !K(isVarArg));
      typeFunc = llvm::Function::Create(ft,
                                        llvm::Function::ExternalLinkage,
                                        llvm::Twine(funcName),
                                        module());
    }

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", typeFunc);
    builder().SetInsertPoint(bb);

    //-------- test
    llvm::Value* testValue = builder().CreateIsNull(builder().CreateLoad(gv));

    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context(), "then", typeFunc);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context(), "else");
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context(), "ifcont");

    builder().CreateCondBr(testValue, thenBB, elseBB);

    //-------- then
    builder().SetInsertPoint(thenBB);

    llvm::Value* thenValue = makeTypeOrCallRegistration(ty);
    builder().CreateStore(thenValue, gv);
    llvm::Value* thenValue2 = builder().CreateLoad(gv);

    builder().CreateBr(mergeBB);
    // Get a reference to the current thenBB, since codegen of 'then' can change
    // the current block, update thenBB for the PHI.
    thenBB = builder().GetInsertBlock();

    //-------- else
    typeFunc->getBasicBlockList().push_back(elseBB);
    builder().SetInsertPoint(elseBB);

    llvm::Value* elseValue = builder().CreateLoad(gv);

    builder().CreateBr(mergeBB);

    elseBB = builder().GetInsertBlock();

    // Emit merge block.
    typeFunc->getBasicBlockList().push_back(mergeBB);
    builder().SetInsertPoint(mergeBB);

    llvm::PHINode *pn = builder().CreatePHI(types().getTypeType(), "iftmp");

    pn->addIncoming(thenValue2, thenBB);
    pn->addIncoming(elseValue, elseBB);

    builder().CreateRet(pn);

    // llvm::Value* val = makeTypeLookupCall(ty);
    // builder().CreateRet(val);

    if (Properties::isCodeDump())
      typeFunc->dump();

    verifyFunction(*typeFunc);

    if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fGenerator->fOptPassManager->run(*typeFunc);
  }
}
