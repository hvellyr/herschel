/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
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
#define K_GF_REG_PRIORITY           2
#define K_METHOD_REG_PRIORITY       3
#define K_GLOBAL_VAR_REG_PRIORITY   4

//----------------------------------------------------------------------------

using namespace herschel;


namespace herschel
{
  class LazyCodeInitializingEmitter
  {
  protected:
    Ptr<CodeGenerator> fGenerator;

  public:
    LazyCodeInitializingEmitter(CodeGenerator* generator)
      : fGenerator(generator)
    {}

    virtual ~LazyCodeInitializingEmitter()
    {}

    virtual const llvm::Type* entityType() const = 0;
    virtual String entityGetterFunctionName() const = 0;
    virtual String entityGetterGlobalVarName() const = 0;
    virtual llvm::Value* makeEntity() const = 0;

    void emit();
  };


  //--------------------------------------------------------------------------

  class TypeLazyCodeInitializingEmitter : public LazyCodeInitializingEmitter
  {
  public:
    Type fType;
    ModuleRuntimeInitializer* fInitializer;

    TypeLazyCodeInitializingEmitter(const Type& ty,
                                    ModuleRuntimeInitializer* initializer)
      : LazyCodeInitializingEmitter(initializer->generator()),
        fType(ty),
        fInitializer(initializer)
    { }


    virtual const llvm::Type* entityType() const
    {
      return fInitializer->types().getTypeType();
    }


    static String getterFunctionName(const Type& ty)
    {
      return String() + "get_" + ty.typeName() + "_type";
    }


    virtual String entityGetterFunctionName() const
    {
      return getterFunctionName(fType);
    }


    virtual String entityGetterGlobalVarName() const
    {
      return fType.typeName() + "_type";
    }


    virtual llvm::Value* makeEntity() const
    {
      return fInitializer->makeTypeOrCallRegistration(fType);
    }
  };


  //--------------------------------------------------------------------------

  class GenericsLazyCodeInitializingEmitter : public LazyCodeInitializingEmitter
  {
  public:
    const FuncDefNode* fNode;
    ModuleRuntimeInitializer* fInitializer;

    GenericsLazyCodeInitializingEmitter(const FuncDefNode* fd,
                                        ModuleRuntimeInitializer* initializer)
      : LazyCodeInitializingEmitter(initializer->generator()),
        fNode(fd),
        fInitializer(initializer)
    { }


    virtual const llvm::Type* entityType() const
    {
      return fInitializer->types().getGenericFuncType();
    }


    static String getterFunctionName(const String& funName)
    {
      return String() + "get_" + funName + "_generic_function";
    }


    virtual String entityGetterFunctionName() const
    {
      return getterFunctionName(fNode->name());
    }


    virtual String entityGetterGlobalVarName() const
    {
      return fNode->name() + "_generic_function";
    }


    virtual llvm::Value* makeEntity() const
    {
      return fInitializer->makeGenericFunctionRegistration(fNode);
    }
  };
};


//----------------------------------------------------------------------------------------


ModuleRuntimeInitializer::ModuleRuntimeInitializer(CodeGenerator* generator)
  : fGenerator(generator)
{
}


void
ModuleRuntimeInitializer::finish()
{
  emitRuntimeInitFunc();
  emitClassInitFunc();
  emitGenericsInitFunc();
  emitMethodInitFunc();
  emitGlobalVarInitFunc();

  emitCtorList(fGlobalCtors, "llvm.global_ctors");
  emitCtorList(fGlobalDtors, "llvm.global_dtors");
}


//------------------------------------------------------------------------------

CodeGenerator*
ModuleRuntimeInitializer::generator() const
{
  return fGenerator;
}


llvm::LLVMContext&
ModuleRuntimeInitializer::context() const
{
  return fGenerator->fContext;
}


llvm::IRBuilder<>&
ModuleRuntimeInitializer::builder() const
{
  return fGenerator->fBuilder;
}


llvm::Module*
ModuleRuntimeInitializer::module() const
{
  return fGenerator->fModule;
}


CodegenTypeUtils&
ModuleRuntimeInitializer::types()
{
  return fGenerator->fTypes;
}


const CodegenTypeUtils&
ModuleRuntimeInitializer::types() const
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
ModuleRuntimeInitializer::addGenericFunctionDef(const FuncDefNode* node)
{
  hr_assert(node->isGeneric());
  hr_assert(!node->isMethod());

  fGenericsInitFuncs.push_back(node);
}


void
ModuleRuntimeInitializer::addMethodDef(const FuncDefNode* node,
                                       const String& methodImplName)
{
  fMethodInitFuncs.push_back(MethodImpl(node, methodImplName));
}


//----------------------------------------------------------------------------

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


template<typename NodeT, typename StrategyT>
void
ModuleRuntimeInitializer::emitEntityInitFunc(const char* suggestedTmpName,
                                             const std::vector<NodeT>& entities,
                                             StrategyT strategy,
                                             int priority)
{
  if (!entities.empty())
  {
    const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         false);
    String tmpName = uniqueName(suggestedTmpName);
    String funcnm = herschel::mangleToC(tmpName);

    llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, funcnm);

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
    builder().SetInsertPoint(bb);

    for (size_t i = 0; i < entities.size(); i++) {
      strategy.emitInitCall(entities[i], this);
    }
    builder().CreateRetVoid();

    verifyFunction(*regFunc);

    if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fGenerator->fOptPassManager->run(*regFunc);

    addGlobalCtor(regFunc, priority);


    for (size_t i = 0; i < entities.size(); i++) {
      strategy.emitEntityGetter(entities[i], this);
    }
  }
}


namespace herschel
{
  class ClassInitStrategy
  {
  public:
    void emitInitCall(const TypeDefNode* tdnode, ModuleRuntimeInitializer* initializer)
    {
      Type ty = tdnode->defType();
      initializer->makeGetTypeLookupCall(ty);
    }

    void emitEntityGetter(const TypeDefNode* tdnode, ModuleRuntimeInitializer* initializer)
    {
      TypeLazyCodeInitializingEmitter emitter(tdnode->defType(), initializer);
      emitter.emit();
    }
  };
};


void
ModuleRuntimeInitializer::emitClassInitFunc()
{
  emitEntityInitFunc("classreg", fClassInitFuncs, ClassInitStrategy(), K_CLASS_REG_PRIORITY);
}


namespace herschel
{
  class GenericsInitStrategy
  {
  public:
    void emitInitCall(const FuncDefNode* fd, ModuleRuntimeInitializer* initializer)
    {
      initializer->makeGetGenericFuncLookupCall(fd);
    }

    void emitEntityGetter(const FuncDefNode* fd, ModuleRuntimeInitializer* initializer)
    {
      GenericsLazyCodeInitializingEmitter emitter(fd, initializer);
      emitter.emit();
    }
  };
};


void
ModuleRuntimeInitializer::emitGenericsInitFunc()
{
  emitEntityInitFunc("generic_reg", fGenericsInitFuncs, GenericsInitStrategy(),
                     K_GF_REG_PRIORITY);
}


namespace herschel
{
  class MethodInitStrategy
  {
  public:
    void emitInitCall(const ModuleRuntimeInitializer::MethodImpl& methImpl,
                      ModuleRuntimeInitializer* initializer)
    {
      initializer->makeMethodRegisterCall(methImpl);
    }

    void emitEntityGetter(const ModuleRuntimeInitializer::MethodImpl& methImpl,
                         ModuleRuntimeInitializer* initializer)
    {
      // NOP.  This is not needed for method registration
    }
  };
};


void
ModuleRuntimeInitializer::emitMethodInitFunc()
{
  emitEntityInitFunc("method_reg", fMethodInitFuncs, MethodInitStrategy(),
                     K_METHOD_REG_PRIORITY);
}


//----------------------------------------------------------------------------

llvm::Value*
ModuleRuntimeInitializer::makeGetTypeLookupCall(const Type& ty) const
{
  return fGenerator->makeGetTypeLookupCall(ty);
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeRegisterCall(llvm::Value* newType) const
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

  builder().CreateCall(regFunc, argv.begin(), argv.end());
  return newType;
}


llvm::Value*
ModuleRuntimeInitializer::makeClassAllocCall(const Type& ty) const
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
      argv.push_back(makeGetTypeLookupCall(isa.seqTypes()[i]));
  }
  else if (isa.isDef()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 1, true)));
    argv.push_back(makeGetTypeLookupCall(isa));
  }
  else {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 0, true)));
  }

  return builder().CreateCall(allocFunc, argv.begin(), argv.end(), "call_class_alloc");
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeAllocCall(const Type& ty) const
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
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("typename")));

  Type isa = ty.typeInheritance();
  if (isa.isSequence()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, isa.seqTypes().size(), true)));
    for (size_t i = 0; i < isa.seqTypes().size(); i++)
      argv.push_back(makeGetTypeLookupCall(isa.seqTypes()[i]));
  }
  else if (isa.isDef()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 1, true)));
    argv.push_back(makeGetTypeLookupCall(isa));
  }
  else {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 0, true)));
  }

  return builder().CreateCall(allocFunc, argv.begin(), argv.end(), "call_type_alloc");
}


llvm::Value*
ModuleRuntimeInitializer::makeTypeOrCallRegistration(const Type& ty) const
{
  if (ty.isClass())
    return makeTypeRegisterCall(makeClassAllocCall(ty));
  else
    return makeTypeRegisterCall(makeTypeAllocCall(ty));
}


//----------------------------------------------------------------------------

llvm::Value*
ModuleRuntimeInitializer::makeGetGenericFuncLookupCall(const FuncDefNode* node) const
{
  return fGenerator->makeGetGenericFuncLookupCall(node);
}


llvm::Value*
ModuleRuntimeInitializer::makeGenericFuncRegisterCall(llvm::Value* newGF) const
{
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("register_generic_function"));
  if (regFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types().getGenericFuncType()); // GenericFunction*

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("register_generic_function"),
                                     module());
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(newGF);

  builder().CreateCall(regFunc, argv.begin(), argv.end());
  return newGF;
}


llvm::Value*
ModuleRuntimeInitializer::makeGenericFuncAllocCall(const FuncDefNode* node) const
{
  const String& funcName = herschel::mangleToC(node->name());

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("generic_function_alloc"));
  if (allocFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(llvm::Type::getInt8PtrTy(context())); // typename
    sign.push_back(llvm::Type::getInt32Ty(context()));   // argc

    llvm::FunctionType *ft = llvm::FunctionType::get(types().getGenericFuncType(),
                                                     sign,
                                                     !K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("generic_function_alloc"),
                                     module());
  }

  std::vector<llvm::Value*> argv;
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(funcName),
                                                 llvm::Twine("funname")));
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32,
                                                    node->specializedParamsCount(),
                                                    true)));

  return builder().CreateCall(allocFunc, argv.begin(), argv.end(), "call_gf_alloc");
}


llvm::Value*
ModuleRuntimeInitializer::makeGenericFunctionRegistration(const FuncDefNode* node) const
{
  return makeGenericFuncRegisterCall(makeGenericFuncAllocCall(node));
}


//----------------------------------------------------------------------------

void
ModuleRuntimeInitializer::makeMethodRegisterCall(const MethodImpl& impl) const
{
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("register_method"));
  if (regFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(types().getGenericFuncType()); // GenericFunction*
    sign.push_back(types().getMethodType());      // void*
    sign.push_back(llvm::Type::getInt32Ty(context())); // argc

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     K(isVarArg));

    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("register_method"),
                                     module());
  }

  llvm::Value* gfFuncCall = makeGetGenericFuncLookupCall(impl.fNode);
  hr_assert(gfFuncCall != NULL);

  llvm::Function* method = module()->getFunction(llvm::StringRef(StrHelper(impl.fMethodImplName)));
  // we should have registered the function before otherwise we wouldn't
  // had the name of the method in the list of methods to register.
  hr_assert(method != NULL);

  size_t countOfSpecs = impl.fNode->specializedParamsCount();

  std::vector<llvm::Value*> argv;
  argv.push_back(gfFuncCall);
  // push the function pointer as second argument.  This is the function to
  // be called for this method (properly casted to void*, as we need a
  // "general function signature" for all functions).
  argv.push_back(llvm::ConstantExpr::getBitCast(method, types().getMethodType()));
  // push the number of specialized type arguments as third argument
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, countOfSpecs, true)));
  // now push type lookups as 4th, etc. argument for the specialized
  // arguments.
  for (NodeList::const_iterator it = impl.fNode->params().begin(),
                                e = impl.fNode->params().end();
       it != e;
       it++)
  {
    if (const ParamNode* prm = dynamic_cast<const ParamNode*>(it->obj())) {
      if (prm->isSpecArg()) {
        llvm::Value* typeLookup = makeGetTypeLookupCall(prm->type());
        argv.push_back(typeLookup);
      }
    }
  }

  builder().CreateCall(regFunc, argv.begin(), argv.end());
}


//============================================================================

void
LazyCodeInitializingEmitter::emit()
{
  String gvNm = herschel::mangleToC(entityGetterGlobalVarName());
  const llvm::Type* gvTy = entityType();
  llvm::Constant* initGv = llvm::Constant::getNullValue(gvTy);

  llvm::GlobalVariable* gv = new llvm::GlobalVariable(gvTy,
                                                      !K(isConstant),
                                                      llvm::GlobalValue::InternalLinkage,
                                                      initGv,
                                                      llvm::Twine(gvNm),
                                                      !K(threadLocal),
                                                      0);    // AddressSpace
  hr_assert(gv != NULL);
  fGenerator->module()->getGlobalList().push_back(gv);


  String funcName = herschel::mangleToC(entityGetterFunctionName());
  llvm::Function* typeFunc = fGenerator->module()->getFunction(llvm::StringRef(funcName));

  if (typeFunc == NULL)
  {
    std::vector<const llvm::Type*> sign;

    llvm::FunctionType *ft = llvm::FunctionType::get(entityType(),
                                                     sign,
                                                     !K(isVarArg));
    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fGenerator->module());
  }

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(fGenerator->context(), "entry", typeFunc);
  fGenerator->builder().SetInsertPoint(bb);

  //-------- test
  llvm::Value* testValue = fGenerator->builder().CreateIsNull(fGenerator->builder().CreateLoad(gv));

  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(fGenerator->context(), "then", typeFunc);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(fGenerator->context(), "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(fGenerator->context(), "ifcont");

  fGenerator->builder().CreateCondBr(testValue, thenBB, elseBB);

  //-------- then
  fGenerator->builder().SetInsertPoint(thenBB);

  llvm::Value* thenValue = makeEntity();
  fGenerator->builder().CreateStore(thenValue, gv);
  llvm::Value* thenValue2 = fGenerator->builder().CreateLoad(gv);

  fGenerator->builder().CreateBr(mergeBB);
  // Get a reference to the current thenBB, since codegen of 'then' can change
  // the current block, update thenBB for the PHI.
  thenBB = fGenerator->builder().GetInsertBlock();

  //-------- else
  typeFunc->getBasicBlockList().push_back(elseBB);
  fGenerator->builder().SetInsertPoint(elseBB);

  llvm::Value* elseValue = fGenerator->builder().CreateLoad(gv);

  fGenerator->builder().CreateBr(mergeBB);

  elseBB = fGenerator->builder().GetInsertBlock();

  // Emit merge block.
  typeFunc->getBasicBlockList().push_back(mergeBB);
  fGenerator->builder().SetInsertPoint(mergeBB);

  llvm::PHINode *pn = fGenerator->builder().CreatePHI(entityType(), "iftmp");

  pn->addIncoming(thenValue2, thenBB);
  pn->addIncoming(elseValue, elseBB);

  fGenerator->builder().CreateRet(pn);

  if (Properties::isCodeDump())
    typeFunc->dump();

  verifyFunction(*typeFunc);

  if (fGenerator->optPassManager() != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fGenerator->optPassManager()->run(*typeFunc);
}


//============================================================================

llvm::Value*
CodeGenerator::makeGetTypeLookupCall(const Type& ty) const
{
  String typeClassLookupFuncName =
    TypeLazyCodeInitializingEmitter::getterFunctionName(ty);
  String funcName = herschel::mangleToC(typeClassLookupFuncName);
  llvm::Function* typeFunc = fModule->getFunction(llvm::StringRef(funcName));

  if (typeFunc == NULL)
  {
    std::vector<const llvm::Type*> sign;

    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes.getTypeType(),
                                                     sign,
                                                     !K(isVarArg));

    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fModule);
  }

  std::vector<llvm::Value*> argv;
  return builder().CreateCall(typeFunc, argv.begin(), argv.end());
}


llvm::Value*
CodeGenerator::makeGetGenericFuncLookupCall(const FuncDefNode* node) const
{
  String gfLookupFuncName =
    GenericsLazyCodeInitializingEmitter::getterFunctionName(node->name());
  String funcName = herschel::mangleToC(gfLookupFuncName);
  llvm::Function* gfFunc = module()->getFunction(llvm::StringRef(funcName));

  if (gfFunc == NULL)
  {
    std::vector<const llvm::Type*> sign;

    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes.getGenericFuncType(),
                                                     sign,
                                                     !K(isVarArg));

    gfFunc = llvm::Function::Create(ft,
                                    llvm::Function::ExternalLinkage,
                                    llvm::Twine(funcName),
                                    fModule);
  }

  std::vector<llvm::Value*> argv;
  return builder().CreateCall(gfFunc, argv.begin(), argv.end());
}
