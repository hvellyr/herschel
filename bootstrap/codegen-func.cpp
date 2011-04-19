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

using namespace herschel;


llvm::FunctionType*
CodeGenerator::createFunctionSignature(const FunctionNode* node, bool inlineRetv,
                                       const Type& retty)
{
  std::vector<const llvm::Type*> sign;

  const llvm::Type* llvmRetty = NULL;
  if (inlineRetv) {
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
    else
      sign.push_back(fTypes.getType(param->type()));
  }

  return llvm::FunctionType::get(llvmRetty, sign, isVarArgs);
}


llvm::Value*
CodeGenerator::compileGenericFunctionDef(const FuncDefNode* node)
{
  fInitializer.addGenericFunctionDef(node);
  return NULL;
}


llvm::Value*
CodeGenerator::compileMethodDef(const FuncDefNode* node)
{
  hr_invalid("TODO");
  return NULL;
}


CodeGenerator::FuncPair
CodeGenerator::createFunction(const FuncDefNode* node)
{
  FuncPair p;

  bool inlineRetv = false;
  String funcnm;
  if (node->hasCLinkage()) {
    funcnm = node->name();
    inlineRetv = false;
  }
  else {
    funcnm = herschel::mangleToC(node->name());
    inlineRetv = true;
  }

  Type retty;
  if (node->isAppMain()) {
    p.fRetType = Type::newTypeRef(Names::kInt32TypeName, K(isValue));
  }
  else
    p.fRetType = node->retType();


  llvm::FunctionType* ft = createFunctionSignature(node, inlineRetv, p.fRetType);
  hr_assert(ft != NULL);

  p.fFunc = llvm::Function::Create(ft,
                                   llvm::Function::ExternalLinkage,
                                   llvm::Twine(funcnm),
                                   fModule);
  return p;
}


llvm::Value*
CodeGenerator::compileAbstractFuncDef(const FuncDefNode* node)
{
  FuncPair func = createFunction(node);

  hr_assert(node->body() == NULL);

  return func.fFunc;
}


llvm::Value*
CodeGenerator::compileNormalFuncDef(const FuncDefNode* node, bool isLocal)
{
  FuncPair func = createFunction(node);

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

    if (!isLocal && node->isAppMain()) {
      fHasMainFunc = true;
    }
  }

  return func.fFunc;
}
