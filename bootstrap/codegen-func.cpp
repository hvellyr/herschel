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
#include "strbuf.h"
#include "codegen-init.h"
#include "codegen-func.h"
#include "codegen-types.h"
#include "codegen-tools.h"

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

using namespace herschel;

CodegenFuncDef::CodegenFuncDef(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenFuncDef::emit(const FuncDefNode* node, bool isLocal) const
{
  fGenerator->fNamedValues.clear();

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


llvm::Function*
CodegenFuncDef::emitExternFuncDef(const FuncDefNode* node) const
{
  FuncPair func = createFunction(node, String(), !K(isGeneric));
  return func.fFunc;
}



llvm::FunctionType*
CodegenFuncDef::createFunctionSignature(const FunctionNode* node, bool inlineRetv,
                                        const Type& retty,
                                        bool isGeneric) const
{
  std::vector<const llvm::Type*> sign;

  const llvm::Type* llvmRetty = NULL;
  if (isGeneric) {
    sign.push_back(types()->getAtomType()->getPointerTo());
    llvmRetty = llvm::Type::getVoidTy(context());
  }
  else if (inlineRetv) {
    sign.push_back(types()->getType(retty)->getPointerTo());
    llvmRetty = llvm::Type::getVoidTy(context());
  }
  else
    llvmRetty = types()->getType(node->retType());

  bool isVarArgs = false;
  for (size_t pidx = 0; pidx < node->params().size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
    // TODO: restargs
    if (param->isRestArg())
      isVarArgs = true;
    else if (param->isSpecArg())
      sign.push_back(types()->getAtomType());
    else
      sign.push_back(types()->getType(param->type()));
  }

  return llvm::FunctionType::get(llvmRetty, sign, isVarArgs);
}


String
CodegenFuncDef::makeFunctionName(const FuncDefNode* node,
                                 const String& methodNameSuffix) const
{
  if (node->hasCLinkage()) {
    hr_assert(methodNameSuffix.isEmpty());
    return node->name();
  }
  else {
    String nm = node->name();
    if (!methodNameSuffix.isEmpty())
      nm = nm + methodNameSuffix;

    return herschel::mangleToC(nm);
  }
}


CodegenFuncDef::FuncPair
CodegenFuncDef::createFunction(const FuncDefNode* node,
                               const String& methodNameSuffix,
                               bool isGeneric) const
{
  FuncPair p;

  hr_assert(implies(node->hasCLinkage(), !isGeneric));

  bool inlineRetv = !node->hasCLinkage();
  String funcnm = makeFunctionName(node, methodNameSuffix);

  llvm::Function* func = module()->getFunction(llvm::StringRef(funcnm));
  if (func != NULL)
    logf(kError, "Redefinition of method: %s", (const char*)StrHelper(funcnm));

  Type retty;
  if (node->isAppMain()) {
    hr_assert(!isGeneric);
    p.fRetType = Type::newTypeRef(Names::kInt32TypeName, K(isValue));
  }
  else
    p.fRetType = node->retType();

  p.fType = createFunctionSignature(node, inlineRetv,
                                    p.fRetType, isGeneric);
  hr_assert(p.fType != NULL);

  p.fFunc = llvm::Function::Create(p.fType,
                                   llvm::Function::ExternalLinkage,
                                   llvm::Twine(funcnm),
                                   module());
  return p;
}


llvm::Value*
CodegenFuncDef::compileGenericFunctionDef(const FuncDefNode* node) const
{
  hr_assert(fGenerator->fNamedValues.empty());

  fGenerator->fInitializer->addGenericFunctionDef(node);

  FuncPair func = createFunction(node, String(), K(isGeneric));

  llvm::Function::arg_iterator aiter = func.fFunc->arg_begin();
  llvm::Function::arg_iterator aiter_e = func.fFunc->arg_end();

  // start with the first real argument.  The first is the return value
  aiter++;

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", func.fFunc);
  builder().SetInsertPoint(bb);

  std::vector<llvm::Value*> realFuncArgv;
  std::vector<llvm::Value*> lookupArgv;
  size_t specArgCount = 0;

  for (size_t pidx = 0;
       pidx < node->params().size() && aiter != aiter_e;
       pidx++, ++aiter)
  {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());

    // TODO: enforce ATOM types for spec args and returnvalue in generic functions
    // TODO ende name
    llvm::AllocaInst *stackSlot =
      tools()->createEntryBlockAlloca(func.fFunc, param->name(),
                                     types()->getType(param->type()));
    llvm::Value* tmpValue = tools()->emitPackCode(param->dstType(),
                                                 param->typeConv(),
                                                 aiter, param->type());
    builder().CreateStore(tmpValue, stackSlot);

    realFuncArgv.push_back(stackSlot);

    if (param->isSpecArg()) {
      specArgCount++;

      llvm::Value* typeidSlot = builder().CreateStructGEP(stackSlot, 0, "typeid");
      lookupArgv.push_back(builder().CreateLoad(typeidSlot));
    }
  }

  llvm::Value* gf = fGenerator->makeGetGenericFuncLookupCall(node);
  hr_assert(gf != NULL);

  lookupArgv.insert(lookupArgv.begin(), gf);

  String lookupFuncName = String("lookup_func") + fromInt(specArgCount);
  llvm::Function* lookupFunc = module()->getFunction(llvm::StringRef(lookupFuncName));
  if (lookupFunc == NULL) {
    // Method* m = lookup_func*(gf, ty0);
    std::vector<const llvm::Type*> sign;
    sign.push_back(types()->getGenericFuncType());
    // add a list of tagid arguments
    for (size_t i = 0; i < specArgCount; i++)
      sign.push_back(types()->getTagIdType());

    llvm::FunctionType *ft = llvm::FunctionType::get(types()->getMethodType(),
                                                     sign,
                                                     !K(isVarArg));
    lookupFunc = llvm::Function::Create(ft,
                                        llvm::Function::ExternalLinkage,
                                        llvm::Twine(lookupFuncName),
                                        module());
  }

  llvm::CallInst* method = builder().CreateCall(lookupFunc,
                                                lookupArgv.begin(), lookupArgv.end());
  if (method == NULL)
    return NULL;

  // the function pointer in the Method* structure is the third member
  llvm::Value* realFuncPtr = builder().CreateLoad(builder().CreateStructGEP(method,
                                                                          2,
                                                                          "method"));

  for (size_t i = 0; i < realFuncArgv.size(); i++)
    realFuncArgv[i] = builder().CreateLoad(realFuncArgv[i]);

  // insert the return value into the argument list
  llvm::AllocaInst *retv = tools()->createEntryBlockAlloca(func.fFunc,
                                                          String("retv"),
                                                          types()->getAtomType());
  realFuncArgv.insert(realFuncArgv.begin(), retv); //func.fFunc->arg_begin());

  llvm::Value* f = builder().CreateBitCast(realFuncPtr, func.fType->getPointerTo());
  builder().CreateCall(f, realFuncArgv.begin(), realFuncArgv.end());

  // no wrap-load!  The generic function always returns as ATOM.
  tools()->assignAtom(retv, func.fFunc->arg_begin());

  builder().CreateRetVoid();


  if (Properties::isCodeDump())
    func.fFunc->dump();

  verifyFunction(*func.fFunc);

  if (fGenerator->fOptPassManager != NULL &&
      Properties::optimizeLevel() > kOptLevelNone)
    fGenerator->fOptPassManager->run(*func.fFunc);

  return func.fFunc;
}


llvm::Value*
CodegenFuncDef::compileMethodDef(const FuncDefNode* node) const
{
  hr_assert(fGenerator->fNamedValues.empty());

  StringBuffer msgbuf;
  msgbuf << "=method";

  for (NodeList::const_iterator it = node->params().begin(),
                                e = node->params().end();
       it != e;
       it++)
  {
    if (const ParamNode* prm = dynamic_cast<const ParamNode*>(it->obj())) {
      if (prm->isSpecArg())
        msgbuf << String(".") << prm->type().typeId();
    }
  }

  String methodNameSuffix = msgbuf.toString();
  fGenerator->fInitializer->addMethodDef(node, makeFunctionName(node, methodNameSuffix));

  FuncPair func = createFunction(node, methodNameSuffix, K(isGeneric));

  return compileNormalFuncDefImpl(func, node, !K(isLocal), K(atomRetType));
}


llvm::Value*
CodegenFuncDef::compileAbstractFuncDef(const FuncDefNode* node) const
{
  hr_assert(fGenerator->fNamedValues.empty());

  FuncPair func = createFunction(node, String(), !K(isGeneric));

  hr_assert(node->body() == NULL);

  return func.fFunc;
}


llvm::Value*
CodegenFuncDef::compileNormalFuncDef(const FuncDefNode* node,
                                     bool isLocal) const
{
  hr_assert(fGenerator->fNamedValues.empty());

  FuncPair func = createFunction(node, String(), !K(isGeneric));
  return compileNormalFuncDefImpl(func, node, isLocal, !K(atomRetType));
}


llvm::Value*
CodegenFuncDef::compileNormalFuncDefImpl(const FuncPair& func,
                                         const FuncDefNode* node,
                                         bool isLocal,
                                         bool forceAtomReturnType) const
{
  // fprintf(stderr, "In function def: %s\n", (const char*)StrHelper(node->name()));
  hr_assert(!node->isAbstract());

  if (node->body() != NULL) {
    llvm::Function::arg_iterator aiter = func.fFunc->arg_begin();
    llvm::Function::arg_iterator aiter_e = func.fFunc->arg_end();

    if (!node->hasCLinkage())
      aiter++;

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", func.fFunc);
    builder().SetInsertPoint(bb);

    for (size_t pidx = 0;
         pidx < node->params().size() && aiter != aiter_e;
         pidx++, ++aiter)
    {
      const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
      llvm::AllocaInst *stackSlot =
        tools()->createEntryBlockAlloca(func.fFunc,
                                       param->name(),
                                       types()->getType(param->type()));
      llvm::Value* tmpValue = tools()->emitPackCode(param->dstType(),
                                                   param->typeConv(),
                                                   aiter, param->type());
      builder().CreateStore(tmpValue, stackSlot);

      fGenerator->fNamedValues[param->name()] = stackSlot;
    }

    const BlockNode* blockNode = dynamic_cast<const BlockNode*>(node->body());
    llvm::Value* retv = NULL;
    if (blockNode != NULL) {
      // Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stderr));
      // out->render(const_cast<BlockNode*>(blockNode));
      retv = fGenerator->codegen(blockNode->children());
    }
    else {
      retv = fGenerator->codegenNode(node->body());
    }
    hr_assert(retv != NULL);
    if (retv == NULL)
      return NULL;

    if (!node->hasCLinkage()) {
      if (node->isAppMain()) {
        hr_assert(!node->isMethod());
        // the app|main function always returns lang|Int32
        if (node->body()->type().isPlainType()) {
          builder().CreateStore(builder().CreateIntCast(tools()->wrapLoad(retv),
                                                      llvm::Type::getInt32Ty(context()),
                                                      true),
                               func.fFunc->arg_begin());
        }
        else {
          llvm::Value* convertedRetv =
            tools()->makeTypeCastAtomToPlain(tools()->wrapLoad(retv),
                                            Type::newTypeRef("clang|int"));
          builder().CreateStore(convertedRetv, func.fFunc->arg_begin());
        }
      }
      else if (forceAtomReturnType) {
        if (node->body()->type().isPlainType()) {
          llvm::Value* tmpValue =
            tools()->emitPackCode(node->body()->dstType(),
                                 node->body()->typeConv(),
                                 tools()->wrapLoad(retv),
                                 node->body()->type());
          // mmh.  Don't know why a createstore does work here.  But it does ...?
          builder().CreateStore(tmpValue, func.fFunc->arg_begin());
          //assignAtom(tmpValue, func.fFunc->arg_begin());
        }
        else
          tools()->assignAtom(retv, func.fFunc->arg_begin());
      }
      else if (func.fRetType.isPlainType()) {
        builder().CreateStore(tools()->wrapLoad(retv),
                             func.fFunc->arg_begin());
      }
      else {
        // no wrap-load!
        tools()->assignAtom(retv, func.fFunc->arg_begin());
      }

      builder().CreateRetVoid();
    }
    else
      builder().CreateRet(tools()->wrapLoad(retv));

    if (Properties::isCodeDump())
      func.fFunc->dump();

    verifyFunction(*func.fFunc);

    if (fGenerator->fOptPassManager != NULL &&
        Properties::optimizeLevel() > kOptLevelNone)
      fGenerator->fOptPassManager->run(*func.fFunc);

    if (!isLocal && node->isAppMain())
      fGenerator->fHasMainFunc = true;
  }

  return func.fFunc;
}
