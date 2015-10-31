/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeprops_string_h
#define bootstrap_typeprops_string_h

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
  class StringTypeProperty : public TypeProperty
  {
  public:
    StringTypeProperty() {}

    virtual zstring convFuncName() const { return NULL; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      hr_invalid("strings are always atoms");
      return NULL;
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return typeUtils->getAtomType();
    }

    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const
    {
      return typeUtils->getAtomTypeSize();
    }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return false; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return false; }

    virtual bool isAnyInt() const { return false; }

    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return 0; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return NULL;
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_string_h
