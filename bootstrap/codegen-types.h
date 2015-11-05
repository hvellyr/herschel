/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "llvm/IR/IRBuilder.h"

#include "codegen-proxy.h"


namespace llvm
{
  class LLVMContext;
  class Module;
  class Type;
  class StructType;
};

namespace herschel
{
  class Type;
  class CodeGenerator;

  //----------------------------------------------------------------------------

  class CodegenTypeUtils : public CodeGeneratorProxy
  {
  public:
    CodegenTypeUtils(CodeGenerator& generator);

    llvm::Type* getAtomPayloadType() const;
    llvm::Type* getAtomType() const;
    llvm::Type* getTagIdType() const;
    llvm::Type* getTypeType() const;
    llvm::StructType* getTypeSlotPairType() const;
    llvm::Type* getGenericFuncType() const;
    llvm::Type* getMethodStructType() const;
    llvm::Type* getMethodType() const;
    llvm::Type* getSizeTTy() const;

    //! returns a struct type which can be used to access array data.  This
    //! denotes to
    //!
    //! struct {
    //!   size_t array_size;
    //!   void* data;
    //! }
    //!
    //! NOTE however that the array payload is actually allocated as:
    //!
    //!  payload = malloc(sizeof(size_t) + array_size);
    //!
    //! To successfully access an array value the data member has to be casted
    //! into the appropriate array base type.
    llvm::Type* getArrayPayloadType() const;

    llvm::Type* getType(const Type& type) const;
    size_t getSlotSize(const Type& type) const;
    size_t getAtomTypeSize() const;
  };

} // namespace
