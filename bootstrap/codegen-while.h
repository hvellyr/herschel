/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_while_h
#define bootstrap_codegen_while_h

#include "llvm/Support/IRBuilder.h"

#include "codegen-proxy.h"


namespace llvm
{
  class Module;
  class Value;
};


namespace herschel
{
  class CodeGenerator;
  class CodegenTypeUtils;
  class BinaryNode;
  class Type;


  class CodegenWhile : public CodeGeneratorProxy
  {
  public:
    CodegenWhile(CodeGenerator* generator);

    llvm::Value* emit(const WhileNode* node) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_while_h
