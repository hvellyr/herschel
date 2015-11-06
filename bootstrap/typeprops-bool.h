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
  class BoolTypeProperty : public TypeProperty
  {
  public:
    BoolTypeProperty() {}

    zstring convFuncName() const override { return "h7_atom_2_bool"; }

    llvm::Value*
    emitPackCode(CodegenTools* tools, llvm::Value* value) const override
    {
      return tools->makeBoolAtom(value);
    }

    llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
    {
      return llvm::Type::getInt1Ty(typeUtils->context());
    }

    size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
    {
      return 1;
    }

    bool isBaseType() const override { return true; }

    bool isPlainType() const override { return true; }

    bool isSigned() const override { return false; }

    bool isAnyNumber() const override { return false; }

    bool isAnyInt() const override { return false; }

    bool isAnyFloat() const override { return false; }

    int typeBitsize() const override { return 0; }

    std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
    {
      return std::unique_ptr<TypeEnumMaker>(new BoolTypeEnumMaker);
    }
  };
};                              // namespace

