/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeprops_bool_h
#define bootstrap_typeprops_bool_h

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

    virtual const char* convFuncName() const { return "h7_atom_2_bool"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeBoolAtom(value);
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt1Ty(typeUtils->context());
    }

    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const { return 1; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return false; }

    virtual bool isAnyInt() const { return false; }

    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return 0; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new BoolTypeEnumMaker;
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_bool_h
