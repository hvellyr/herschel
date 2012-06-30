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
#include "codegen-init.h"
#include "codegen-tools.h"
#include "codegen-types.h"
#include "utils.h"

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

#define K_NO_PRIORITY               65535

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
      return fInitializer->types()->getTypeType();
    }


    static String getterFunctionName(const Type& ty)
    {
      if (ty.hasGenerics()) {
        return String() + "get_" + ty.typeName() + "_type";
      }
      else {
        return String() + "get_" + ty.typeId() + "_type";
      }
    }


    virtual String entityGetterFunctionName() const
    {
      return getterFunctionName(fType);
    }


    virtual String entityGetterGlobalVarName() const
    {
      return fType.typeId() + "_type";
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
      return fInitializer->types()->getGenericFuncType();
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


  //----------------------------------------------------------------------------

  class ClassInitStrategy
  {
  public:
    void emitInitCall(const TypeDefNode* tdnode,
                      ModuleRuntimeInitializer* initializer)
    {
      Type ty = tdnode->defType();
      initializer->makeGetTypeLookupCall(ty);
    }

    void emitEntityGetter(const TypeDefNode* tdnode,
                          ModuleRuntimeInitializer* initializer)
    {
      TypeLazyCodeInitializingEmitter emitter(tdnode->defType(), initializer);
      emitter.emit();
    }
  };


  class GenericsInitStrategy
  {
  public:
    void emitInitCall(const FuncDefNode* fd,
                      ModuleRuntimeInitializer* initializer)
    {
      initializer->makeGetGenericFuncLookupCall(fd);
    }

    void emitEntityGetter(const FuncDefNode* fd,
                          ModuleRuntimeInitializer* initializer)
    {
      GenericsLazyCodeInitializingEmitter emitter(fd, initializer);
      emitter.emit();
    }
  };


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


//------------------------------------------------------------------------------


ModuleRuntimeInitializer::ModuleRuntimeInitializer(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


void
ModuleRuntimeInitializer::finish()
{
  emitModuleInitFunction();

  emitCtorList(fGlobalCtors, "llvm.global_ctors");
  emitCtorList(fGlobalDtors, "llvm.global_dtors");
}


//------------------------------------------------------------------------------

void
ModuleRuntimeInitializer::emitModuleInitFunction()
{
  const llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                         !K(isVarArg));
  llvm::Function *regFunc = createGlobalInitOrDtorFunction(ft, String("_module_init"));

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", regFunc);
  builder().SetInsertPoint(bb);

  emitRuntimeInitFunc();
  emitEntityInitFunc(fClassInitFuncs, ClassInitStrategy());
  emitEntityInitFunc(fGenericsInitFuncs, GenericsInitStrategy());
  emitEntityInitFunc(fMethodInitFuncs, MethodInitStrategy());
  emitKeywordInitFunc();
  emitGlobalVarInitFunc();

  builder().CreateRetVoid();

  verifyFunction(*regFunc);

  if (fGenerator->fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fGenerator->fOptPassManager->run(*regFunc);

  addGlobalCtor(regFunc, K_NO_PRIORITY);

  emitEntityGetterFunc(fClassInitFuncs, ClassInitStrategy());
  emitEntityGetterFunc(fGenericsInitFuncs, GenericsInitStrategy());
  emitEntityGetterFunc(fMethodInitFuncs, MethodInitStrategy());
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
    ctors.push_back(llvm::ConstantStruct::get(
                      ctorStructTy,
                      vector_of<llvm::Constant*>(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context()),
                                                                        i->second, false))
                                                (llvm::ConstantExpr::getBitCast(i->first, ctorPFTy)) ));
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
  fn->setSection("__TEXT,__StaticInit,regular,pure_instructions");
  fn->setDoesNotThrow();

  return fn;
}


void
ModuleRuntimeInitializer::emitKeywordInitFunc()
{
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("h7_keyword_register"));
  if (regFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getInt8PtrTy(context()),
                                                     vector_of<const llvm::Type*>(
                                                       llvm::Type::getInt8PtrTy(context())),
                                                     !K(isVarArg));
    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_keyword_register"),
                                     module());
  }

  for (KeywordMap::const_iterator it = fKeywords.begin(),
         end = fKeywords.end();
       it != end;
       it++)
  {
    std::vector<llvm::Value*> argv = vector_of(
      builder().CreateGlobalStringPtr(StrHelper(it->first),
                                      llvm::Twine(StrHelper(it->first + "_kw"))));

    llvm::Value* val = builder().CreateCall(regFunc, argv.begin(), argv.end(),
                                            "keyw_init");

    builder().CreateStore(val, it->second);
  }
}


void
ModuleRuntimeInitializer::emitGlobalVarInitFunc()
{
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

    llvm::Value* val = tools()->emitPackCode(varnode->initExpr()->dstType(),
                                             varnode->initExpr()->typeConv(),
                                             initval, varnode->initExpr()->type());
    builder().CreateStore(val, it->second);
  }
}


void
ModuleRuntimeInitializer::emitRuntimeInitFunc()
{
  std::vector<const llvm::Type*> sign;

  llvm::FunctionType *ft2 = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                    sign,
                                                    !K(isVarArg));

  llvm::Function* rtinitFunc = llvm::Function::Create(ft2,
                                                      llvm::Function::ExternalLinkage,
                                                      llvm::Twine("h7_runtime_init"),
                                                      module());

  std::vector<llvm::Value*> argv;
  builder().CreateCall(rtinitFunc, argv.begin(), argv.end());
}


template<typename NodeT, typename StrategyT>
void
ModuleRuntimeInitializer::emitEntityInitFunc(const std::vector<NodeT>& entities,
                                             StrategyT strategy)
{
  if (!entities.empty())
  {
    for (size_t i = 0; i < entities.size(); i++)
      strategy.emitInitCall(entities[i], this);
  }
}


template<typename NodeT, typename StrategyT>
void
ModuleRuntimeInitializer::emitEntityGetterFunc(const std::vector<NodeT>& entities,
                                               StrategyT strategy)
{
  if (!entities.empty())
  {
    for (size_t i = 0; i < entities.size(); i++)
      strategy.emitEntityGetter(entities[i], this);
  }
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
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("h7_register_type"));
  if (regFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     vector_of(types()->getTypeType()),
                                                     !K(isVarArg));
    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_register_type"),
                                     module());
  }

  std::vector<llvm::Value*> argv = vector_of(newType);
  builder().CreateCall(regFunc, argv.begin(), argv.end());
  return newType;
}


llvm::Constant*
ModuleRuntimeInitializer::createTypeSlotSpec(const String& slotName,
                                             size_t slotOffset) const
{
  llvm::Value* slotStrValue =
    builder().CreateGlobalStringPtr((const char*)StrHelper(slotName),
                                    (const char*)StrHelper(slotName + "_str"));

  return llvm::ConstantStruct::get(
    types()->getTypeSlotPairType(),
    vector_of(static_cast<llvm::Constant*>(slotStrValue))
             (llvm::ConstantInt::get(context(),
                                     llvm::APInt(32,
                                                 (uint64_t)slotOffset,
                                                 !K(isSigned)))));
}


ModuleRuntimeInitializer::OrderedTypeSlots
ModuleRuntimeInitializer::orderTypeSlots(const TypeSlotList& typeSlots) const
{
  OrderedTypeSlots retv;
  retv.fTotalSize = 0;

  if (!typeSlots.empty()) {
    std::map<size_t, std::set<String> > slotsGroupedBySize;

    //---- group slots by size
    for (size_t i = 0; i < typeSlots.size(); i++) {
      size_t slotSize = types()->getSlotSize(typeSlots[i].type());

      // should not have any types larger than 16 bytes
      hr_assert(slotSize == 1 || slotSize == 2 ||
                slotSize == 4 || slotSize == 8 ||
                slotSize == 16);

      slotsGroupedBySize[slotSize].insert(typeSlots[i].name());
    }

    //---- arrange them best fitting in order 16, 8, 4, 2, 1,
    size_t offset = 0;
    for (size_t i = 16; i > 0; i = i / 2) {
      std::set<String> slotNames = slotsGroupedBySize[i];
      for (std::set<String>::const_iterator it = slotNames.begin(),
             end = slotNames.end();
           it != end;
           it++)
      {
        retv.fSlotNames.push_back(*it);
        retv.fSlotOffsets.push_back(offset);

        offset += i;
      }
    }

    const size_t quantizeSize = generator()->is64Bit()
      ? 8
      : 4;
    retv.fTotalSize = (offset % quantizeSize != 0
                       ? (offset / quantizeSize + 1) * quantizeSize
                       : offset);
  }

  hr_assert(retv.fSlotNames.size() == retv.fSlotOffsets.size());

  return retv;
}


ModuleRuntimeInitializer::SlotAndClassSpecs
ModuleRuntimeInitializer::computeTypeSlotAndClassSpecs(const Type& ty) const
{
  OrderedTypeSlots orderedSlots = orderTypeSlots(ty.slots());
  hr_assert(orderedSlots.fSlotNames.size() == orderedSlots.fSlotOffsets.size());
  hr_assert(implies(ty.slots().empty(), orderedSlots.fTotalSize == 0));
  hr_assert(implies(!ty.slots().empty(), orderedSlots.fTotalSize > 0));

  std::vector<llvm::Constant*> slotSpecs;
  for (size_t i = 0; i < orderedSlots.fSlotNames.size(); i++) {
    llvm::Constant* slotSpec = createTypeSlotSpec(orderedSlots.fSlotNames[i],
                                                  orderedSlots.fSlotOffsets[i]);
    slotSpecs.push_back(slotSpec);
  }
  slotSpecs.push_back(llvm::Constant::getNullValue(types()->getTypeSlotPairType()));

  const llvm::ArrayType* arrayType = llvm::ArrayType::get(types()->getTypeSlotPairType(),
                                                          slotSpecs.size());

  SlotAndClassSpecs retv;
  retv.fInstanceSize = orderedSlots.fTotalSize;
  retv.fTypeSlotSpecs = llvm::ConstantArray::get(arrayType, slotSpecs);

  return retv;
}


llvm::Value*
ModuleRuntimeInitializer::makeClassAllocCall(const Type& ty) const
{
  const String& typeName = herschel::mangleToC(ty.typeId());

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("h7_class_alloc"));
  if (allocFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(
      types()->getTypeType(),
      vector_of<const llvm::Type*>(llvm::Type::getInt8PtrTy(context())) // typename
                                  (llvm::Type::getInt32Ty(context()))   // instance size
                                  (types()->getTypeSlotPairType()->getPointerTo()) // const TypeSlotPair*
                                  (llvm::Type::getInt32Ty(context())),   // isa_size
      K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_class_alloc"),
                                     module());
  }

  // get the class layout and instance size
  SlotAndClassSpecs specs = computeTypeSlotAndClassSpecs(ty);

  std::vector<llvm::Value*> argv;
  // arg 1: the class name
  argv.push_back(builder().CreateGlobalStringPtr(StrHelper(typeName), llvm::Twine("classname")));
  // arg 2: the instance size
  argv.push_back(llvm::ConstantInt::get(context(),
                                        llvm::APInt(32, specs.fInstanceSize, true)));

  // arg 3: the list of slot specifications
  llvm::GlobalVariable *gv =
    new llvm::GlobalVariable(specs.fTypeSlotSpecs->getType(),
                             K(isConstant),
                             llvm::GlobalValue::InternalLinkage,
                             specs.fTypeSlotSpecs,
                             llvm::Twine(StrHelper(typeName + "_slot_specs")),
                             !K(isThreadLocal),
                             0);
  hr_assert(gv != NULL);
  module()->getGlobalList().push_back(gv);

  llvm::Value* v2 = builder().CreateConstGEP1_32(gv, 0);
  llvm::Value* v3 = builder().CreatePointerCast(v2,
                                                types()->getTypeSlotPairType()->getPointerTo());
  argv.push_back(v3);

  // arg 4: the number of inherited classes/types
  Type isa = ty.typeInheritance();
  if (isa.isSequence()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, isa.seqTypes().size(), true)));
    // arg 5-n: the inherited classes/types
    for (size_t i = 0; i < isa.seqTypes().size(); i++)
      argv.push_back(makeGetTypeLookupCall(isa.seqTypes()[i]));
  }
  else if (isa.isDef()) {
    argv.push_back(llvm::ConstantInt::get(context(),
                                          llvm::APInt(32, 1, true)));
    // arg 5: the inherited classes/types
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

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("h7_type_alloc"));
  if (allocFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(
      types()->getTypeType(),
      vector_of<const llvm::Type*>(llvm::Type::getInt8PtrTy(context())) // typename
                                  (llvm::Type::getInt32Ty(context())),   // isa_size
      K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_type_alloc"),
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
  llvm::Function* regFunc =
    module()->getFunction(llvm::StringRef("h7_register_generic_function"));
  if (regFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     vector_of(types()->getGenericFuncType()),
                                                     !K(isVarArg));
    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_register_generic_function"),
                                     module());
  }

  std::vector<llvm::Value*> argv = vector_of(newGF);

  builder().CreateCall(regFunc, argv.begin(), argv.end());
  return newGF;
}


llvm::Value*
ModuleRuntimeInitializer::makeGenericFuncAllocCall(const FuncDefNode* node) const
{
  const String& funcName = herschel::mangleToC(node->name());

  llvm::Function* allocFunc = module()->getFunction(llvm::StringRef("h7_generic_function_alloc"));
  if (allocFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(
      types()->getGenericFuncType(),
      vector_of<const llvm::Type*>(llvm::Type::getInt8PtrTy(context())) // typename,
                                  (llvm::Type::getInt32Ty(context())), // argc
      !K(isVarArg));
    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_generic_function_alloc"),
                                     module());
  }

  std::vector<llvm::Value*> argv =
    vector_of(builder().CreateGlobalStringPtr(StrHelper(funcName),
                                              llvm::Twine("funname")))
             (llvm::ConstantInt::get(context(),
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
  llvm::Function* regFunc = module()->getFunction(llvm::StringRef("h7_register_method"));
  if (regFunc == NULL) {
    llvm::FunctionType* ft = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context()),
      vector_of(types()->getGenericFuncType()) // GenericFunction*,
               (types()->getMethodType())      // void*
               (llvm::Type::getInt32Ty(context())), // argc
      K(isVarArg));

    regFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("h7_register_method"),
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
  argv.push_back(llvm::ConstantExpr::getBitCast(method, types()->getMethodType()));
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


//----------------------------------------------------------------------------

llvm::Value*
ModuleRuntimeInitializer::registerKeyword(const String& keyword)
{
  KeywordMap::iterator it = fKeywords.find(keyword);

  if (it == fKeywords.end()) {
    String varnm = herschel::mangleToC(String("keyw$") + keyword);
    const llvm::Type* constTy = llvm::Type::getInt8PtrTy(context());
    llvm::Constant* initConst = llvm::Constant::getNullValue(constTy);

    llvm::GlobalVariable* gv =
      new llvm::GlobalVariable(constTy,
                               !K(isConstant),
                               llvm::GlobalValue::PrivateLinkage,
                               initConst,
                               llvm::Twine(varnm),
                               !K(threadLocal),
                               0);    // AddressSpace
    hr_assert(gv != NULL);
    module()->getGlobalList().push_back(gv);

    fKeywords.insert(std::make_pair(keyword, gv));

    return gv;
  }

  return it->second;
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
    llvm::FunctionType *ft = llvm::FunctionType::get(entityType(),
                                                     std::vector<const llvm::Type*>(),
                                                     !K(isVarArg));
    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fGenerator->module());
  }

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(fGenerator->context(), "entry", typeFunc);
  fGenerator->builder().SetInsertPoint(bb);

  //-------- test
  llvm::Value* testValue = fGenerator->builder()
    .CreateIsNull(fGenerator->builder().CreateLoad(gv));

  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(fGenerator->context(),
                                                      "then",
                                                      typeFunc);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(fGenerator->context(),
                                                      "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(fGenerator->context(),
                                                       "ifcont");

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
CodeGenerator::makeGetBaseTypeLookupCall(const Type& ty) const
{
  /*
    if ty has no generics than we use a simple callback function:

      get_base_type()

    if ty has generics though, do the following:

      lookup_derived_type(get_base_type(), "full-type-name")

    This looks up a type for "full-type-name", which is a variation of
    get_base_type().  If the full type has been looked up (and cached)
    already, simply return that type instance, otherwise create a new one as
    derivation of base type.
  */
  String typeClassLookupFuncName;

  if (ty.hasGenerics()) {
    typeClassLookupFuncName = String("get_") + ty.typeName() + "_type";
  }
  else {
    typeClassLookupFuncName = TypeLazyCodeInitializingEmitter::getterFunctionName(ty);
  }

  String funcName = herschel::mangleToC(typeClassLookupFuncName);
  llvm::Function* typeFunc = fModule->getFunction(llvm::StringRef(funcName));

  if (typeFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes->getTypeType(),
                                                     std::vector<const llvm::Type*>(),
                                                     !K(isVarArg));
    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fModule);
  }

  std::vector<llvm::Value*> argv;
  llvm::Value* type_value = builder().CreateCall(typeFunc, argv.begin(), argv.end());

  if (ty.hasGenerics()) {
    llvm::Function* lookupTypeFunc = fModule->getFunction(llvm::StringRef(String("lookup_derived_type")));

    if (lookupTypeFunc == NULL) {
      llvm::FunctionType *ft = llvm::FunctionType::get(fTypes->getTypeType(),
                                                       vector_of(fTypes->getTypeType())
                                                                (llvm::Type::getInt8PtrTy(context())),
                                                       !K(isVarArg));

      lookupTypeFunc = llvm::Function::Create(ft,
                                              llvm::Function::ExternalLinkage,
                                              llvm::Twine(String("lookup_derived_type")),
                                              fModule);
    }
    std::vector<llvm::Value*> lookup_argv = vector_of(type_value)
                                                     (builder().CreateGlobalStringPtr(StrHelper(ty.typeId()),
                                                                                    llvm::Twine("tyname")));

    llvm::Value* lookup_type_value = builder().CreateCall(lookupTypeFunc,
                                                          lookup_argv.begin(), lookup_argv.end());
    return lookup_type_value;
  }
  else {
    return type_value;
  }
}


llvm::Value*
CodeGenerator::makeGetArrayTypeLookupCall(const Type& ty) const
{
  llvm::Value* baseTyValue = makeGetTypeLookupCall(ty.arrayBaseType());

  String funcName = String("h7_type_lookup_array_type");
  llvm::Function* typeFunc = fModule->getFunction(llvm::StringRef(funcName));

  if (typeFunc == NULL) {
    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes->getTypeType(),
                                                     vector_of(fTypes->getTypeType()),
                                                     !K(isVarArg));
    typeFunc = llvm::Function::Create(ft,
                                      llvm::Function::ExternalLinkage,
                                      llvm::Twine(funcName),
                                      fModule);
  }

  std::vector<llvm::Value*> argv = vector_of(baseTyValue);
  return builder().CreateCall(typeFunc, argv.begin(), argv.end());
}


llvm::Value*
CodeGenerator::makeGetTypeLookupCall(const Type& ty) const
{
  if (ty.isArray())
    return makeGetArrayTypeLookupCall(ty);

  // TODO: union and sequence type, function types, etc.
  else
    return makeGetBaseTypeLookupCall(ty);
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

    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes->getGenericFuncType(),
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


//------------------------------------------------------------------------------

void
CodeGenerator::createDefaultCMainFunc()
{
  llvm::FunctionType *ft =
    llvm::FunctionType::get(
      llvm::Type::getInt32Ty(context()),
      vector_of<const llvm::Type*>(llvm::Type::getInt32Ty(context()))
                                  (llvm::Type::getInt8Ty(context())
                                     ->getPointerTo()
                                     ->getPointerTo()),
      false);
  hr_assert(ft != NULL);

  llvm::Function *func = llvm::Function::Create(ft,
                                                llvm::Function::ExternalLinkage,
                                                std::string("main"),
                                                fModule);

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(),
                                                  "entry", func);
  fBuilder.SetInsertPoint(bb);

  String appMainFuncNm = herschel::mangleToC(Names::kAppMain);
  llvm::Function* appMainFunc = fModule->getFunction(llvm::StringRef(appMainFuncNm));
  hr_assert(appMainFunc != NULL);

  llvm::AllocaInst* retv = fTools->createEntryBlockAlloca(func, String("tmp2"),
                                                          llvm::Type::getInt32Ty(context()));
  std::vector<llvm::Value*> argv = vector_of<llvm::Value*>(retv);
  fBuilder.CreateCall(appMainFunc, argv.begin(), argv.end());

  llvm::Value* retv2 = fBuilder.CreateLoad(retv);
  fBuilder.CreateRet(retv2);

  verifyFunction(*func);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func);
}


