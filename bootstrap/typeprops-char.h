/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeprops_char_h
#define bootstrap_typeprops_char_h

#include "typeprops.h"
#include "typeenum.h"
#include "codegen-tools.h"
#include "codegen-types.h"

#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"

namespace herschel
{
  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the bool type.
  class CharTypeProperty : public TypeProperty
  {
  public:
    CharTypeProperty() {}

    virtual const char* convFuncName() const { return "atom_2_char"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeCharAtom(value);
    }

    virtual const llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
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

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new CharTypeEnumMaker;
    }
  };


  //! TypeProperty implementation for the clang|char type.
  class ClangCharTypeProperty : public TypeProperty
  {
  public:
    ClangCharTypeProperty() {}

    virtual const char* convFuncName() const { return "atom_2_int8"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeIntAtom(value, CodegenTools::kAtomInt8);
    }

    virtual const llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
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

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new Int8TypeEnumMaker;
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_char_h
