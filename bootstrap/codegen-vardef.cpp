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
#include "codegen-vardef.h"
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

CodegenVardef::CodegenVardef(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenVardef::emit(const VardefNode* node, bool isLocal) const
{
  if (!isLocal)
    return codegenForGlobalVars(node);

  return codegenForLocalVars(node);
}


llvm::Value*
CodegenVardef::codegenForLocalVars(const VardefNode* node) const
{
  llvm::Value* initval = NULL;
  Type dstType;
  Type type;
  TypeConvKind convKind = kNoConv;
  if (node->initExpr() != NULL) {
    if (dynamic_cast<UndefNode*>(node->initExpr())) {
      initval = llvm::Constant::getNullValue(types()->getType(node->type()));

      dstType = node->type();
      type = node->type();
      convKind = kNoConv;
    }
    else {
      initval = tools()->wrapLoad(generator()->codegenNode(node->initExpr()));

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

  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();

  llvm::AllocaInst* stackSlot = tools()->createEntryBlockAlloca(curFunction,
                                                               node->name(),
                                                               types()->getType(node->type()));

  llvm::Value* val = tools()->emitPackCode(dstType, convKind, initval, type);
  builder().CreateStore(val, stackSlot);

  generator()->fNamedValues[node->name()] = stackSlot;

  return initval;
}


llvm::Value*
CodegenVardef::codegenForGlobalVars(const VardefNode* node) const
{
  String varnm = herschel::mangleToC(node->name());
  const llvm::Type* constTy = types()->getType(node->type());
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
  module()->getGlobalList().push_back(gv);

  initializer()->addGlobalVariable(node);

  hr_assert(generator()->fGlobalVariables.find(node->name()) == generator()->fGlobalVariables.end());
  generator()->fGlobalVariables[node->name()] = gv;

  return gv;
}


