/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "typeenum.hpp"
#include "typeprops.hpp"
// #include "codegen-tools.hpp"
// #include "codegen-types.hpp"

// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/LLVMContext.h"


namespace herschel {

//! TypeProperty implementation for the bool type.
class StringTypeProperty : public TypeProperty {
public:
  StringTypeProperty() {}

  // zstring convFuncName() const override { return nullptr; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   hr_invalid("strings are always atoms");
  //   return nullptr;
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return typeUtils->getAtomType();
  // }

  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return typeUtils->getAtomTypeSize();
  // }

  bool isBaseType() const override { return true; }

  bool isPlainType() const override { return false; }

  bool isSigned() const override { return false; }

  bool isAnyNumber() const override { return false; }

  bool isAnyInt() const override { return false; }

  bool isAnyFloat() const override { return false; }

  int typeBitsize() const override { return 0; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return nullptr;
  }
};

}  // namespace herschel
