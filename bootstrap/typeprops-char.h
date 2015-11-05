/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "typeprops.h"
#include "typeenum.h"
#include "codegen-tools.h"
#include "codegen-types.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

namespace herschel
{
  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the bool type.
  class CharTypeProperty : public TypeProperty
  {
  public:
    CharTypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_char"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeCharAtom(value);
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt32Ty(typeUtils->context());
    }

    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const { return 4; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return false; }

    virtual bool isAnyInt() const { return false; }

    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return 32; }

    virtual std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const
    {
      return std::unique_ptr<TypeEnumMaker>(new CharTypeEnumMaker);
    }
  };


  //! TypeProperty implementation for the clang|char type.
  class ClangCharTypeProperty : public TypeProperty
  {
  public:
    ClangCharTypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int8"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeIntAtom(value, CodegenTools::kAtomInt8);
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt8Ty(typeUtils->context());
    }

    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const { return 1; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return true; }

    virtual bool isAnyNumber() const { return false; }

    virtual bool isAnyInt() const { return false; }

    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return 8; }

    virtual std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const
    {
      return std::unique_ptr<TypeEnumMaker>(new Int8TypeEnumMaker);
    }
  };
};                              // namespace

