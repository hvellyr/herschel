/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "refcountable.h"

#include "llvm/Support/IRBuilder.h"

namespace llvm
{
  class Value;
  class Module;
};


namespace heather
{
  class AptNode;
  class IntNode;
  class RealNode;

  //----------------------------------------------------------------------------

  class CodeGenerator : public RefCountable
  {
  public:
    CodeGenerator();
    ~CodeGenerator();

    void generateCode(AptNode* node);

    llvm::Value* codegen(IntNode* node);
    llvm::Value* codegen(RealNode* node);

  private:
    llvm::Module*     fModule;
    llvm::IRBuilder<> fBuilder;
  };
};                              // namespace

#endif                          // bootstrap_codegen_h
