/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "codegen.h"
#include "apt.h"
#include "log.h"

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

//----------------------------------------------------------------------------

using namespace heather;


CodeGenerator::CodeGenerator()
  : fModule(NULL),
    fBuilder(llvm::getGlobalContext())
{
  llvm::LLVMContext& context = llvm::getGlobalContext();
  fModule = new llvm::Module("compile-unit", context);
}


CodeGenerator::~CodeGenerator()
{
  if (fModule != NULL)
    delete fModule;
}


void
CodeGenerator::generateCode(AptNode* node)
{
  llvm::Value* value = node->codegen(this);
  if (value != NULL) {
    value->dump();
  }
}


llvm::Value*
CodeGenerator::codegenNode(AptNode* node)
{
  return node->codegen(this);
}


llvm::Value*
CodeGenerator::codegen(const IntNode* node)
{
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, node->fValue, true));
}


llvm::Value*
CodeGenerator::codegen(const RealNode* node)
{
  return llvm::ConstantFP::get(llvm::getGlobalContext(),
                               llvm::APFloat(node->fValue));
}


llvm::Value*
CodeGenerator::codegen(const StringNode* node)
{
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const KeywordNode* node)
{
  return NULL;
}




llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  llvm::Function *calleeFunc = NULL;

  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->fBase.obj());
  if (symNode != NULL) {
    // Look up the name in the global module table.
    calleeFunc = fModule->getFunction(symNode->string());
    if (calleeFunc == NULL) {
      errorf(node->srcpos(), 0, "Unknown function referenced");
      return NULL;
    }
  }
  else {
    // TODO
    assert(0);
  }

  // TODO: proper argument mismatch check
  if (calleeFunc->arg_size() != node->fChildren.size()) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  std::vector<llvm::Value*> argv;
  for (unsigned i = 0, e = node->fChildren.size(); i != e; ++i) {
    argv.push_back(codegenNode(node->fChildren[i]));
    if (argv.back() == NULL)
      return NULL;
  }

  return fBuilder.CreateCall(calleeFunc, argv.begin(), argv.end(), "calltmp");
}


llvm::Value*
CodeGenerator::codegen(const ArrayNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ArraySymbolNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const AssignNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BinaryNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BlockNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const BoolNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const CharNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const CompileUnitNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const DictNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const FuncDefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const IfNode* node)
{
  llvm::Value *testValue = codegenNode(node->test());
  if (testValue == NULL)
    return NULL;

  // Convert condition to a bool by comparing equal to 0
  testValue = fBuilder.CreateICmpEQ(testValue,
                                    llvm::ConstantInt::get(llvm::getGlobalContext(),
                                                           llvm::APInt(1, 0, true)),
                                    "ifcond");

  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "then", curFunction);
  llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                      "else");
  llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(llvm::getGlobalContext(),
                                                       "ifcont");

  fBuilder.CreateCondBr(testValue, thenBB, elseBB);

  // Emit then value.
  fBuilder.SetInsertPoint(thenBB);

  llvm::Value *thenValue = codegenNode(node->consequent());
  if (thenValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'then' can change the current block, update thenBB for the PHI.
  thenBB = fBuilder.GetInsertBlock();


  // Emit else block.
  curFunction->getBasicBlockList().push_back(elseBB);
  fBuilder.SetInsertPoint(elseBB);

  llvm::Value *elseValue = codegenNode(node->alternate());
  if (elseValue == NULL)
    return NULL;

  fBuilder.CreateBr(mergeBB);
  // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
  elseBB = fBuilder.GetInsertBlock();

  // Emit merge block.
  curFunction->getBasicBlockList().push_back(mergeBB);
  fBuilder.SetInsertPoint(mergeBB);
  llvm::PHINode *pn = fBuilder.CreatePHI(llvm::Type::getDoubleTy(llvm::getGlobalContext()),
                                         "iftmp");

  pn->addIncoming(thenValue, thenBB);
  pn->addIncoming(elseValue, elseBB);

  return pn;
}


llvm::Value*
CodeGenerator::codegen(const KeyargNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const MatchNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const NegateNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const OnNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ParamNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RangeNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const RationalNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SelectNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const SlotdefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const ThenWhileNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const TypeNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const UnitConstant* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const VectorNode* node)
{
  // TODO
  return NULL;
}


llvm::Value*
CodeGenerator::codegen(const WhileNode* node)
{
  // TODO
  return NULL;
}
