/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_types_h
#define bootstrap_codegen_types_h

#include "llvm/Support/IRBuilder.h"

namespace llvm
{
  class LLVMContext;
  class Module;
  class Type;
};

namespace herschel
{
  class Type;
  class CodeGenerator;

  //----------------------------------------------------------------------------

  class CodegenTypeUtils
  {
  public:
    CodegenTypeUtils(CodeGenerator* generator);

    const llvm::Type* getAtomType() const;
    const llvm::Type* getTagIdType() const;
    const llvm::Type* getTypeType() const;
    const llvm::Type* getTypeSlotPairType() const;
    const llvm::Type* getGenericFuncType() const;
    const llvm::Type* getMethodType() const;
    const llvm::Type* getType(const Type& type) const;

  private:
    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;

    //-------- data members

    Ptr<CodeGenerator> fGenerator; 
  };
};                              // namespace

#endif                          // bootstrap_codegen_types_h
