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
#include "codegen-while.h"
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

CodegenWhile::CodegenWhile(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenWhile::emit(const WhileNode* node) const
{
  llvm::Function *curFunction = builder().GetInsertBlock()->getParent();
  llvm::BasicBlock *loopHeadBB = llvm::BasicBlock::Create(context(),
                                                          "loophead", curFunction);
  llvm::BasicBlock *loopBB = llvm::BasicBlock::Create(context(),
                                                      "loop", curFunction);
  // Create the "after loop" block and insert it.
  llvm::BasicBlock *afterBB = llvm::BasicBlock::Create(context(),
                                                       "afterloop",
                                                       curFunction);

  // Insert an explicit fall through from the current block to the loopBB.
  builder().CreateBr(loopHeadBB);

  // Start insertion in loopBB.
  builder().SetInsertPoint(loopHeadBB);

  llvm::Value *testValue =
    tools()->wrapLoad(generator()->codegenNode(node->test()));
  if (testValue == NULL)
    return NULL;

  llvm::Value* extrTestVal = tools()->emitPackCode(node->test()->dstType(),
                                                   node->test()->typeConv(),
                                                   testValue,
                                                   node->test()->type());

  // Convert condition to a bool by comparing equal to 1
  testValue = builder().CreateICmpEQ(extrTestVal,
                                    llvm::ConstantInt::get(context(),
                                                           llvm::APInt(1, 1, true)),
                                    "loopcond");
  // Insert the conditional branch into the end of loopEndBB.
  builder().CreateCondBr(testValue, loopBB, afterBB);

  // Start insertion in loopBB.
  builder().SetInsertPoint(loopBB);

  llvm::Value* bodyValue = generator()->codegenNode(node->body());
  if (bodyValue == NULL)
    return NULL;

  // jump back to loop start
  builder().CreateBr(loopHeadBB);

  // Any new code will be inserted in AfterBB.
  builder().SetInsertPoint(afterBB);

  return bodyValue;
}
