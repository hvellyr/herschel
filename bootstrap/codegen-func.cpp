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
#include "strbuf.h"

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

using namespace herschel;


llvm::FunctionType*
CodeGenerator::createFunctionSignature(const FunctionNode* node, bool inlineRetv,
                                       const Type& retty,
                                       bool isGeneric)
{
  std::vector<const llvm::Type*> sign;

  const llvm::Type* llvmRetty = NULL;
  if (isGeneric) {
    sign.push_back(fTypes.getAtomType()->getPointerTo());
    llvmRetty = llvm::Type::getVoidTy(context());
  }
  else if (inlineRetv) {
    sign.push_back(fTypes.getType(retty)->getPointerTo());
    llvmRetty = llvm::Type::getVoidTy(context());
  }
  else
    llvmRetty = fTypes.getType(node->retType());

  bool isVarArgs = false;
  for (size_t pidx = 0; pidx < node->params().size(); pidx++) {
    const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());
    // TODO: restargs
    if (param->isRestArg())
      isVarArgs = true;
    else if (param->isSpecArg())
      sign.push_back(fTypes.getAtomType());
    else
      sign.push_back(fTypes.getType(param->type()));
  }

  return llvm::FunctionType::get(llvmRetty, sign, isVarArgs);
}


String
CodeGenerator::makeFunctionName(const FuncDefNode* node,
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


CodeGenerator::FuncPair
CodeGenerator::createFunction(const FuncDefNode* node,
                              const String& methodNameSuffix,
                              bool isGeneric)
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
                                   fModule);
  return p;
}


llvm::Value*
CodeGenerator::compileGenericFunctionDef(const FuncDefNode* node)
{
  hr_assert(fNamedValues.empty());

  fInitializer.addGenericFunctionDef(node);

  FuncPair func = createFunction(node, String(), K(isGeneric));

  llvm::Function::arg_iterator aiter = func.fFunc->arg_begin();
  llvm::Function::arg_iterator aiter_e = func.fFunc->arg_end();

  // start with the first real argument.  The first is the return value
  aiter++;

  llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", func.fFunc);
  fBuilder.SetInsertPoint(bb);

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
    llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func.fFunc, param->name(),
                                                         fTypes.getType(param->type()));
    fBuilder.CreateStore(aiter, stackSlot);

    realFuncArgv.push_back(stackSlot);

    if (param->isSpecArg()) {
      specArgCount++;

      llvm::Value* typeidSlot = fBuilder.CreateStructGEP(stackSlot, 0, "typeid");
      lookupArgv.push_back(fBuilder.CreateLoad(typeidSlot));
    }
  }

  llvm::Value* gf = makeGetGenericFuncLookupCall(node);
  hr_assert(gf != NULL);

  lookupArgv.insert(lookupArgv.begin(), gf);

  String lookupFuncName = String("lookup_func") + fromInt(specArgCount);
  llvm::Function* lookupFunc = module()->getFunction(llvm::StringRef(lookupFuncName));
  if (lookupFunc == NULL) {
    // Method* m = lookup_func*(gf, ty0);
    std::vector<const llvm::Type*> sign;
    sign.push_back(fTypes.getGenericFuncType());
    // add a list of tagid arguments
    for (size_t i = 0; i < specArgCount; i++)
      sign.push_back(llvm::Type::getInt32Ty(context()));

    llvm::FunctionType *ft = llvm::FunctionType::get(fTypes.getMethodType(),
                                                     sign,
                                                     !K(isVarArg));
    lookupFunc = llvm::Function::Create(ft,
                                        llvm::Function::ExternalLinkage,
                                        llvm::Twine(lookupFuncName),
                                        fModule);
  }

  llvm::CallInst* method = fBuilder.CreateCall(lookupFunc,
                                               lookupArgv.begin(), lookupArgv.end());
  if (method == NULL)
    return NULL;

  // the function pointer in the Method* structure is the third member
  llvm::Value* realFuncPtr = fBuilder.CreateLoad(fBuilder.CreateStructGEP(method, 2, "method"));

  for (size_t i = 0; i < realFuncArgv.size(); i++)
    realFuncArgv[i] = fBuilder.CreateLoad(realFuncArgv[i]);

  // insert the return value into the argument list
  realFuncArgv.insert(realFuncArgv.begin(), func.fFunc->arg_begin());

  llvm::Value* f = fBuilder.CreateBitCast(realFuncPtr, func.fType->getPointerTo());
  fBuilder.CreateCall(f, realFuncArgv.begin(), realFuncArgv.end());

  fBuilder.CreateRetVoid();


  if (Properties::isCodeDump())
    func.fFunc->dump();

  verifyFunction(*func.fFunc);

  if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
    fOptPassManager->run(*func.fFunc);

  return func.fFunc;
}


llvm::Value*
CodeGenerator::compileMethodDef(const FuncDefNode* node)
{
  hr_assert(fNamedValues.empty());

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
  fInitializer.addMethodDef(node, makeFunctionName(node, methodNameSuffix));

  FuncPair func = createFunction(node, methodNameSuffix, !K(isGeneric));

  return compileNormalFuncDefImpl(func, node, !K(isLocal));
}


llvm::Value*
CodeGenerator::compileAbstractFuncDef(const FuncDefNode* node)
{
  hr_assert(fNamedValues.empty());

  FuncPair func = createFunction(node, String(), !K(isGeneric));

  hr_assert(node->body() == NULL);

  return func.fFunc;
}


llvm::Value*
CodeGenerator::compileNormalFuncDef(const FuncDefNode* node, bool isLocal)
{
  hr_assert(fNamedValues.empty());

  FuncPair func = createFunction(node, String(), !K(isGeneric));
  return compileNormalFuncDefImpl(func, node, isLocal);
}


llvm::Value*
CodeGenerator::compileNormalFuncDefImpl(const FuncPair& func,
                                        const FuncDefNode* node,
                                        bool isLocal)
{
  hr_assert(!node->isAbstract());

  if (node->body() != NULL) {
    llvm::Function::arg_iterator aiter = func.fFunc->arg_begin();
    llvm::Function::arg_iterator aiter_e = func.fFunc->arg_end();

    if (!node->hasCLinkage())
      aiter++;

    llvm::BasicBlock *bb = llvm::BasicBlock::Create(context(), "entry", func.fFunc);
    fBuilder.SetInsertPoint(bb);

    for (size_t pidx = 0;
         pidx < node->params().size() && aiter != aiter_e;
         pidx++, ++aiter)
    {
      const ParamNode* param = dynamic_cast<const ParamNode*>(node->params()[pidx].obj());

      // TODO: enforce ATOM types for spec args and returnvalue in generic functions
      // TODO ende name
      llvm::AllocaInst *stackSlot = createEntryBlockAlloca(func.fFunc, param->name(),
                                                           fTypes.getType(param->type()));
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

    if (!node->hasCLinkage()) {
      if (node->isAppMain()) {
        // the app|main function always returns lang|Int32
        if (node->body()->type().isPlainType()) {
          fBuilder.CreateStore(fBuilder.CreateIntCast(wrapLoad(retv),
                                                      llvm::Type::getInt32Ty(context()),
                                                      true),
                               func.fFunc->arg_begin());
        }
        else {
          llvm::Value* convertedRetv = makeTypeCastAtomToPlain(wrapLoad(retv),
                                                               Type::newTypeRef("clang|int"));
          fBuilder.CreateStore(convertedRetv, func.fFunc->arg_begin());
        }
      }
      else if (func.fRetType.isPlainType()) {
        fBuilder.CreateStore(wrapLoad(retv), func.fFunc->arg_begin());
      }
      else {
        // no wrap-load!
        assignAtom(retv, func.fFunc->arg_begin());
      }

      fBuilder.CreateRetVoid();
    }
    else
      fBuilder.CreateRet(wrapLoad(retv));

    if (Properties::isCodeDump())
      func.fFunc->dump();

    verifyFunction(*func.fFunc);

    if (fOptPassManager != NULL && Properties::optimizeLevel() > kOptLevelNone)
      fOptPassManager->run(*func.fFunc);

    if (!isLocal && node->isAppMain())
      fHasMainFunc = true;
  }

  return func.fFunc;
}
