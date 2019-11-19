/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "llvm/IR/IRBuilder.h"

#include "codegen-proxy.hpp"


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


  class CodegenIf : public CodeGeneratorProxy
  {
  public:
    CodegenIf(CodeGenerator& generator);

    llvm::Value* emit(const IfNode* node) const;
  };

} // namespace
