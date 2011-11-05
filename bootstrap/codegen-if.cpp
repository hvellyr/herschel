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
#include "codegen-if.h"
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

CodegenIf::CodegenIf(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenIf::emit(const IfNode* node) const
{
  llvm::Value *testValue =
    tools()->wrapLoad(generator()->codegenNode(node->test()));
  if (testValue == NULL)
    return NULL;

  llvm::Value* extrTestVal = tools()->emitPackCode(node->test()->dstType(),
                                                   node->test()->typeConv(),
                                                   testValue,
                                                   node->test()->type());

  // Convert condition to a bool by comparing equal to 1
  testValue = builder().CreateICmpEQ(tools()->wrapLoad(extrTestVal),
                                    llvm::ConstantInt::get(context(),
                                                           llvm::APInt(1, 1, true)),
                                    "ifcond");

  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context(),
                                                      "then", curFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context(),
                                                      "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context(),
                                                       "ifcont");

  builder().CreateCondBr(testValue, thenBB, elseBB);

  // Emit then value.
  builder().SetInsertPoint(thenBB);

  llvm::Value *thenValue =
    tools()->wrapLoad(generator()->codegenNode(node->consequent()));
  if (thenValue == NULL)
    return NULL;

  llvm::Value* thenValue2 = tools()->wrapLoad(
    tools()->emitPackCode(node->consequent()->dstType(),
                          node->consequent()->typeConv(),
                          thenValue,
                          node->consequent()->type()));

  builder().CreateBr(mergeBB);
  // Get a reference to the current thenBB, since codegen of 'then' can change
  // the current block, update thenBB for the PHI.
  thenBB = builder().GetInsertBlock();


  // Emit else block.
  curFunction->getBasicBlockList().push_back(elseBB);
  builder().SetInsertPoint(elseBB);

  llvm::Value* elseValue = NULL;
  if (node->alternate() != NULL) {
    llvm::Value* elseValue0 =
      tools()->wrapLoad(generator()->codegenNode(node->alternate()));
    if (elseValue0 == NULL)
      return NULL;
    elseValue = tools()->wrapLoad(tools()->emitPackCode(node->alternate()->dstType(),
                                                        node->alternate()->typeConv(),
                                                        elseValue0,
                                                        node->alternate()->type()));
  }
  else
    elseValue = llvm::Constant::getNullValue(types()->getType(node->type()));

  builder().CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = builder().GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  builder().SetInsertPoint(mergeBB);

  llvm::PHINode *pn = builder().CreatePHI(types()->getType(node->type()),
                                          "iftmp");

  pn->addIncoming(thenValue2, thenBB);
  pn->addIncoming(elseValue, elseBB);

  return pn;
}
