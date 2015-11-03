/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeprops_float_h
#define bootstrap_typeprops_float_h

#include "typeprops.h"
#include "typeenum.h"
#include "codegen-tools.h"

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

namespace herschel
{
  template<typename EnumMaker, int bitsize>
  class BaseFloatTypeProperty : public TypeProperty
  {
    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const { return bitsize / 8; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return true; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return false; }

    virtual bool isAnyFloat() const { return true; }

    virtual int typeBitsize() const { return bitsize; }

    virtual TypeEnumMaker* makeBaseTypeEnumMaker() const
    {
      return new EnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the float32 type.
  class Float32TypeProperty : public BaseFloatTypeProperty<Float32TypeEnumMaker, 32>
  {
  public:
    Float32TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_float32"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      // TODO
      return tools->makeFloatAtom(value, CodegenTools::kAtomFloat32);
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getFloatTy(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the float64 type.
  class Float64TypeProperty : public BaseFloatTypeProperty<Float64TypeEnumMaker, 64>
  {
  public:
    Float64TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_float64"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      // TODO.  Float64 do not fit into the atom payload structure?
      return nullptr;
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getDoubleTy(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the float128 type.
  class Float128TypeProperty : public BaseFloatTypeProperty<Float128TypeEnumMaker, 128>
  {
  public:
    Float128TypeProperty() {}

    virtual zstring convFuncName() const { return "atom_2_float128"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      // TODO.  Float128 do not fit into the atom payload structure
      return nullptr;
    }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getFP128Ty(typeUtils->context());
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_float_h
