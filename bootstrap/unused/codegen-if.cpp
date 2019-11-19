/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "codegen.hpp"
#include "apt.hpp"
#include "log.hpp"
#include "properties.hpp"
#include "symbol.hpp"
#include "xmlrenderer.hpp"
#include "predefined.hpp"
#include "strbuf.hpp"
#include "codegen-init.hpp"
#include "codegen-if.hpp"
#include "codegen-types.hpp"
#include "codegen-tools.hpp"

#include <vector>

#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"


//----------------------------------------------------------------------------

using namespace herschel;

CodegenIf::CodegenIf(CodeGenerator& generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenIf::emit(const IfNode* node) const
{
  llvm::Value *testValue =
    tools().wrapLoad(generator().codegenNode(*node->test()));
  if (!testValue)
    return nullptr;

  auto extrTestVal = tools().emitPackCode(node->test()->dstType(),
                                          node->test()->typeConv(),
                                          testValue,
                                          node->test()->type());

  // Convert condition to a bool by comparing equal to 1
  testValue = builder().CreateICmpEQ(tools().wrapLoad(extrTestVal),
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
    tools().wrapLoad(generator().codegenNode(*node->consequent()));
  if (!thenValue)
    return nullptr;

  auto thenValue2 = tools().wrapLoad(
    tools().emitPackCode(node->consequent()->dstType(),
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

  llvm::Value* elseValue = nullptr;
  if (node->alternate()) {
    llvm::Value* elseValue0 =
      tools().wrapLoad(generator().codegenNode(*node->alternate()));
    if (!elseValue0)
      return nullptr;
    elseValue = tools().wrapLoad(tools().emitPackCode(node->alternate()->dstType(),
                                                      node->alternate()->typeConv(),
                                                      elseValue0,
                                                      node->alternate()->type()));
  }
  else
    elseValue = llvm::Constant::getNullValue(types().getType(node->type()));

  builder().CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = builder().GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  builder().SetInsertPoint(mergeBB);

  llvm::PHINode *pn = builder().CreatePHI(types().getType(node->type()),
                                          0, "iftmp");

  pn->addIncoming(thenValue2, thenBB);
  pn->addIncoming(elseValue, elseBB);

  return pn;
}
