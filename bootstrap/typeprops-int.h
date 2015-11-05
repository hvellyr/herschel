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

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"

namespace herschel
{
  template<typename EnumMaker, int bitsize, bool issigned, int atominttype>
  class BaseIntTypeProperty : public TypeProperty
  {
    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->makeIntAtom(value, (CodegenTools::Typeid)atominttype);
    }

    virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const { return bitsize / 8; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return issigned; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }
    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return bitsize; }

    virtual std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const
    {
      return std::unique_ptr<TypeEnumMaker>(new EnumMaker);
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int32 type.
  class Int32TypeProperty : public BaseIntTypeProperty<Int32TypeEnumMaker,
                                                       32,
                                                       K(issigned),
                                                       CodegenTools::kAtomInt32>
  {
  public:
    Int32TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int32"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt32Ty(typeUtils->context());
    }
  };


  //! TypeProperty implementation for the clang|int type.  TODO: shouldn't
  //! this be 32/64 depending on host platform?
  class ClangIntTypeProperty : public BaseIntTypeProperty<Int32TypeEnumMaker,
                                                          32,
                                                          K(issigned),
                                                          CodegenTools::kAtomInt32>
  {
  public:
    ClangIntTypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int32"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt32Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int32 type.
  class UInt32TypeProperty : public BaseIntTypeProperty<UInt32TypeEnumMaker,
                                                       32,
                                                       !K(issigned),
                                                       CodegenTools::kAtomUInt32>
  {
  public:
    UInt32TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_uint32"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt32Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int16 type.
  class Int16TypeProperty : public BaseIntTypeProperty<Int16TypeEnumMaker,
                                                       16,
                                                       K(issigned),
                                                       CodegenTools::kAtomInt16>
  {
  public:
    Int16TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int16"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt16Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the uint16 type.
  class UInt16TypeProperty : public BaseIntTypeProperty<UInt16TypeEnumMaker,
                                                        16,
                                                        !K(issigned),
                                                        CodegenTools::kAtomUInt16>
  {
  public:
    UInt16TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_uint16"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt16Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int8 type.
  class Int8TypeProperty : public BaseIntTypeProperty<Int8TypeEnumMaker,
                                                      8,
                                                      K(issigned),
                                                      CodegenTools::kAtomInt8>
  {
  public:
    Int8TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int8"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt8Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the uint8 type.
  class UInt8TypeProperty : public BaseIntTypeProperty<UInt8TypeEnumMaker,
                                                       8,
                                                       !K(issigned),
                                                       CodegenTools::kAtomUInt8>
  {
  public:
    UInt8TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_uint8"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt8Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int8 type.
  class Int64TypeProperty : public BaseIntTypeProperty<Int64TypeEnumMaker,
                                                       64,
                                                       K(issigned),
                                                       CodegenTools::kAtomInt64>
  {
  public:
    Int64TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_int64"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt64Ty(typeUtils->context());
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the uint8 type.
  class UInt64TypeProperty : public BaseIntTypeProperty<UInt64TypeEnumMaker,
                                                        64,
                                                        !K(issigned),
                                                        CodegenTools::kAtomUInt64>
  {
  public:
    UInt64TypeProperty() {}

    virtual zstring convFuncName() const { return "h7_atom_2_uint64"; }

    virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const
    {
      return llvm::Type::getInt64Ty(typeUtils->context());
    }
  };
};                              // namespace

