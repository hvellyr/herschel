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
  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int32 type.
  class Int32TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_int32"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomInt32));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt32Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const
    {
      return 4; // llvm::Type::getInt32Ty(llvm::getGlobalContext());
    }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return true; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 32; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new Int32TypeEnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int32 type.
  class UInt32TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_uint32"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomUInt32));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt32Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const
    {
      return 4; // llvm::Type::getInt32Ty(llvm::getGlobalContext());
    }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 32; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new UInt32TypeEnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int16 type.
  class Int16TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_int16"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomInt16));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt16Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const
    {
      return 2; // llvm::Type::getInt16Ty(llvm::getGlobalContext());
    }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return true; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 16; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new Int16TypeEnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the uint16 type.
  class UInt16TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_uint16"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomUInt16));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt16Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const
    {
      return 2; // llvm::Type::getInt16Ty(llvm::getGlobalContext());
    }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 16; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new UInt16TypeEnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the int8 type.
  class Int8TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_int8"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomInt8));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt8Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const { return 1; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return true; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 8; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new Int8TypeEnumMaker;
    }
  };


  // -----------------------------------------------------------------------------

  //! TypeProperty implementation for the uint8 type.
  class UInt8TypeProperty : public TypeProperty
  {
  public:
    virtual const char* convFuncName() const { return "atom_2_uint8"; }

    virtual llvm::Value* emitPackCode(CodegenTools* tools, llvm::Value* value) const
    {
      return tools->wrapLoad(tools->makeIntAtom(value,
                                                CodegenTools::kAtomUInt8));
    }

    virtual const llvm::Type* getLLVMType() const
    {
      return llvm::Type::getInt8Ty(llvm::getGlobalContext());
    }

    virtual size_t getSlotSize() const { return 1; }

    virtual bool isBaseType() const { return true; }

    virtual bool isPlainType() const { return true; }

    virtual bool isSigned() const { return false; }

    virtual bool isAnyNumber() const { return true; }

    virtual bool isAnyInt() const { return true; }

    virtual int intTypeBitsize() const { return 8; }

    virtual TypeEnumMaker* newBaseTypeEnumMaker() const
    {
      return new UInt8TypeEnumMaker;
    }
  };
};                              // namespace

#endif                          // bootstrap_typeprops_int_h
