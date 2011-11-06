/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_types_h
#define bootstrap_codegen_types_h

#include "llvm/Support/IRBuilder.h"

#include "refcountable.h"
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

  class CodegenTypeUtils : public RefCountable,
                           public CodeGeneratorProxy
  {
  public:
    CodegenTypeUtils(CodeGenerator* generator);

    const llvm::Type* getAtomType() const;
    const llvm::Type* getTagIdType() const;
    const llvm::Type* getTypeType() const;
    const llvm::StructType* getTypeSlotPairType() const;
    const llvm::Type* getGenericFuncType() const;
    const llvm::Type* getMethodType() const;
    const llvm::Type* getSizeTTy() const;

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
    const llvm::Type* getArrayPayloadType() const;

    const llvm::Type* getType(const Type& type) const;
    size_t getSlotSize(const Type& type) const;
    size_t getAtomTypeSize() const;


  };
};                              // namespace

#endif                          // bootstrap_codegen_types_h
