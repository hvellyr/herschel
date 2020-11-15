/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "ast.hpp"
#include "typeenum.hpp"
#include "typeprops.hpp"
// #include "codegen-tools.hpp"
// #include "codegen-types.hpp"

// #include "llvm/IR/DerivedTypes.h"
// #include "llvm/IR/LLVMContext.h"


namespace herschel {

//! TypeProperty implementation for the keyword type.
class KeywordTypeProperty : public TypeProperty {
public:
  KeywordTypeProperty() {}

  // zstring convFuncName() const override { return "h7_atom_2_keyword"; }

  // llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const override
  // {
  //   hr_invalid("keywords are atoms always");
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
    return std::unique_ptr<TypeEnumMaker>(new KeywordTypeEnumMaker);
  }

  std::shared_ptr<AstNode> makeNullValueNode() const override
  {
    return std::make_shared<KeywordNode>(SrcPos(), String(""));
  }
};

}  // namespace herschel
