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

#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"

namespace herschel
{
  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the bool type.
  class BoolTypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const
    {
      return "atom_2_bool";
    }


    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeBoolAtom(value));
    }


    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt1Ty(llvm::getGlobalContext());
    }


    virtual size_t getSlotSize() const
    {
      return 1; // llvm::Type::getInt1Ty(llvm::getGlobalContext());
    }


    virtual bool isBaseType() const
    {
      return true;
    }


    virtual bool isPlainType() const
    {
      return true;
    }


    virtual bool isSigned() const
    {
      return false;
    }


    virtual bool isAnyNumber() const
    {
      return false;
    }


    virtual bool isAnyInt() const
    {
      return false;
    }


    virtual int intTypeBitsize() const
    {
      return 0;
    }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new BoolTypeEnumMaker;
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_bool_h
