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

// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/LLVMContext.h"


namespace herschel {

template <typename EnumMaker, int bitsize>
class BaseFloatTypeProperty : public TypeProperty {
  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return bitsize / 8;
  // }

  bool isBaseType() const override { return true; }

  bool isPlainType() const override { return true; }

  bool isSigned() const override { return true; }

  bool isAnyNumber() const override { return true; }

  bool isAnyInt() const override { return false; }

  bool isAnyFloat() const override { return true; }

  int typeBitsize() const override { return bitsize; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return std::unique_ptr<TypeEnumMaker>(new EnumMaker);
  }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the float32 type.
class Float32TypeProperty : public BaseFloatTypeProperty<Float32TypeEnumMaker, 32> {
public:
  Float32TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_float32"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   // TODO
  //   return tools->makeFloatAtom(value, CodegenTools::kAtomFloat32);
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getFloatTy(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the float64 type.
class Float64TypeProperty : public BaseFloatTypeProperty<Float64TypeEnumMaker, 64> {
public:
  Float64TypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_float64"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   // TODO.  Float64 do not fit into the atom payload structure?
  //   return nullptr;
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getDoubleTy(typeUtils->context());
  // }
};


// -----------------------------------------------------------------------------

//! TypeProperty implementation for the float128 type.
class Float128TypeProperty : public BaseFloatTypeProperty<Float128TypeEnumMaker, 128> {
public:
  Float128TypeProperty() {}

  // zstring convFuncName() const override { return "atom_2_float128"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   // TODO.  Float128 do not fit into the atom payload structure
  //   return nullptr;
  // }

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return llvm::Type::getFP128Ty(typeUtils->context());
  // }
};

}  // namespace herschel
