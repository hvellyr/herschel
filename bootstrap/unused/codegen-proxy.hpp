/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "llvm/IR/IRBuilder.h"

#include "codegen.hpp"

namespace llvm
{
  class LLVMContext;
  class Module;
};

namespace herschel
{
  class CodegenTypeUtils;
  class CodegenTools;

  class CodeGeneratorProxy
  {
  public:
    CodeGeneratorProxy(CodeGenerator& generator)
      : fGenerator(generator)
    {
    }

    CodeGenerator& generator() const
    {
      return fGenerator;
    }

    llvm::LLVMContext& context() const
    {
      return fGenerator.context();
    }

    llvm::IRBuilder<>& builder() const
    {
      return fGenerator.builder();
    }

    llvm::Module* module() const
    {
      return fGenerator.fModule;
    }

    CodegenTypeUtils& types()
    {
      return *fGenerator.fTypes;
    }

    const CodegenTypeUtils& types() const
    {
      return *fGenerator.fTypes;
    }

    CodegenTools& tools() const
    {
      return *fGenerator.fTools;
    }

    ModuleRuntimeInitializer& initializer() const
    {
      return *fGenerator.fInitializer;
    }

  protected:
    CodeGenerator& fGenerator;
  };

} // namespace
