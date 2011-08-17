/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_typeprops_int_h
#define bootstrap_typeprops_int_h

#include "typeprops.h"
#include "typeenum.h"
#include "codegen-tools.h"

#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"

namespace herschel
{
  template<typename EnumMaker, int bitsize, bool issigned, int atominttype>
  class BaseIntTypeProperty : public TypeProperty
  {
    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                (CodegenTools::Typeid)atominttype));
    }

    virtual size_t getSlotSize() const { return bitsize / 8; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return issigned; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }
    virtual bool isAnyFloat() const { return false; }

    virtual int typeBitsize() const { return bitsize; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new EnumMaker;
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
    virtual const char* convFuncName() const { return "atom_2_int32"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt32Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_uint32"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt32Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_int16"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt16Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_uint16"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt16Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_int8"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt8Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_uint8"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt8Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_int64"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt64Ty(llvm::getGlobalContext());
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
    virtual const char* convFuncName() const { return "atom_2_uint64"; }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt64Ty(llvm::getGlobalContext());
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_int_h
