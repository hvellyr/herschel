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
class CharTypeProperty : public TypeProperty {
public:
  CharTypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_char"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   return tools->makeCharAtom(value);
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt32Ty(typeUtils->context());
  // }

  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return 4;
  // }

  bool isBaseType() const override { return true; }

  bool isPlainType() const override { return true; }

  bool isSigned() const override { return false; }

  bool isAnyNumber() const override { return false; }

  bool isAnyInt() const override { return false; }

  bool isAnyFloat() const override { return false; }

  int typeBitsize() const override { return 32; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return std::unique_ptr<TypeEnumMaker>(new CharTypeEnumMaker);
  }
};


//! TypeProperty implementation for the clang.char type.
class ClangCharTypeProperty : public TypeProperty {
public:
  ClangCharTypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_int8"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   return tools->makeIntAtom(value, CodegenTools::kAtomInt8);
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getInt8Ty(typeUtils->context());
  // }

  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override { return 1; }

  bool isBaseType() const override { return true; }

  bool isPlainType() const override { return true; }

  bool isSigned() const override { return true; }

  bool isAnyNumber() const override { return false; }

  bool isAnyInt() const override { return false; }

  bool isAnyFloat() const override { return false; }

  int typeBitsize() const override { return 8; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return std::unique_ptr<TypeEnumMaker>(new Int8TypeEnumMaker);
  }
};

}  // namespace herschel
