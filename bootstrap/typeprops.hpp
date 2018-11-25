/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "typeenum.hpp"


namespace llvm {
class Value;
class Type;
}  // namespace llvm

namespace herschel {
// class CodegenTools;
// class CodegenTypeUtils;

//! Defines abstract properties of types as defined by the language
//! specification.  This can only be used for predefined types like Int32,
//! Float64, String, etc., but not for user defined types.
class TypeProperty {
public:
  virtual bool isValid() const { return true; }

  // //@{ Code generation

  // //! Return the C function name used to convert an object of this type from
  // //! atom layout to the plain layout (e.g. "h7_atom_2_int32").  C function
  // //! returned here must be implemented in the C runtime.
  // virtual zstring convFuncName() const = 0;

  // //! Emit the necessary LLVM instructions to convert a object of this type
  // //! from plain layout to atom layout (when destination type conversion is
  // //! kPlain2AtomConv).
  // virtual llvm::Value* emitPackCode(CodegenTools* tools,
  //                                   llvm::Value* value) const = 0;

  // //! Return the appropriate LLVM type for this type.
  // virtual llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const = 0;

  // //! Return the required slot size in octets for an object of this type.
  // virtual size_t getSlotSize(const CodegenTypeUtils* typeUtils) const = 0;
  // //@}

  //@{ Type properties

  //! Indicate whether this type is a base type?
  virtual bool isBaseType() const = 0;

  //! Indicate whether this type is a plain type?
  virtual bool isPlainType() const = 0;

  //! Indicate whether this type is signed.  If this type is not a number
  //! returns \c false.
  virtual bool isSigned() const = 0;

  //! Indicates whether this type is a kind of number.
  virtual bool isAnyNumber() const = 0;

  //! Indicates whether this type is any integer.
  virtual bool isAnyInt() const = 0;

  //! Indicates whether this type is any float.
  virtual bool isAnyFloat() const = 0;

  //! Return the bitsize for this type.  Returns \c 0 if not a number.
  virtual int typeBitsize() const = 0;

  //@}


  //! Return a new instance of the appropriate \c TypeEnumMaker to be used
  //! to determine auto assigned enumeration values.
  virtual std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const = 0;
};


class InvalidTypeProperty : public TypeProperty {
public:
  InvalidTypeProperty() {}

  bool isValid() const override { return false; }

  // zstring convFuncName() const override { return nullptr; };

  // llvm::Value* emitPackCode(CodegenTools* tools,
  //                           llvm::Value* value) const override
  // {
  //   return nullptr;
  // };

  // llvm::Type* getLLVMType(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return nullptr;
  // };

  // size_t getSlotSize(const CodegenTypeUtils* typeUtils) const override
  // {
  //   return 0;
  // }

  bool isBaseType() const override { return false; }

  bool isPlainType() const override { return false; }

  bool isSigned() const override { return false; }

  bool isAnyNumber() const override { return false; }

  bool isAnyInt() const override { return false; }
  bool isAnyFloat() const override { return false; }

  int typeBitsize() const override { return 0; }

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const override
  {
    return nullptr;
  }
};

}  // namespace herschel
