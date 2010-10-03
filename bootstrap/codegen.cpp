/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/


#include "codegen.h"
#include "apt.h"

#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Support/IRBuilder.h"


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
CodeGenerator::codegen(IntNode* node)
{
  return llvm::ConstantInt::get(llvm::getGlobalContext(),
                                llvm::APInt(32, node->fValue, true));
}


llvm::Value*
CodeGenerator::codegen(RealNode* node)
{
  return llvm::ConstantFP::get(llvm::getGlobalContext(),
                               llvm::APFloat(node->fValue));
}
