/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_tools_h
#define bootstrap_codegen_tools_h

#include "llvm/Support/IRBuilder.h"

#include "refcountable.h"
#include "codegen-proxy.h"

namespace llvm
{
  class AllocaInst;
  class LLVMContext;
  class Function;
  class Module;
  class Type;
  class Value;
};

namespace herschel
{
  class Type;
  class CodeGenerator;


  class CodegenTools : public RefCountable,
                       public CodeGeneratorProxy
  {
  public:
    CodegenTools(CodeGenerator* generator);

    enum Typeid {
      kAtomAny     = 0x01,
      kAtomBool    = 0x02,
      kAtomChar    = 0x03,
      kAtomInt32   = 0x04,
      kAtomUInt32  = 0x05,
      kAtomKeyword = 0x06,
      kAtomInt16   = 0x07,
      kAtomUInt16  = 0x08,
      kAtomInt8    = 0x09,
      kAtomUInt8   = 0x0a,
      kAtomInt64   = 0x0b,
      kAtomUInt64  = 0x0c,

      kAtomFloat32 = 0x0d,
      kAtomFloat64 = 0x0e,

      kAtomString  = 0x10,
      kAtomStringImpl = 0x11,

      kAtomInt32Array   = kAtomInt32 + 0x40,
      kAtomUInt32Array  = kAtomUInt32 + 0x40,
      kAtomInt16Array   = kAtomInt16 + 0x40,
      kAtomUInt16Array  = kAtomUInt16 + 0x40,
      kAtomInt8Array    = kAtomInt8 + 0x40,
      kAtomUInt8Array   = kAtomUInt8 + 0x40,
      kAtomInt64Array   = kAtomInt64 + 0x40,
      kAtomUInt64Array  = kAtomUInt64 + 0x40,
      kAtomCharArray    = kAtomChar + 0x40,
      kAtomKeywordArray = kAtomKeyword + 0x40,
      kAtomStringArray  = kAtomString + 0x40,
      kAtomStringImplArray  = kAtomStringImpl + 0x40,

      kAtomFloat32Array = kAtomFloat32 + 0x40,
      kAtomFloat64Array = kAtomFloat64 + 0x40
    };

    void setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value);

    void assignAtom(llvm::Value* src, llvm::Value* dst);
    llvm::Function* getIntrinsic(unsigned int iid,
                                 const llvm::Type** tys, unsigned int numTys);
    llvm::Function* getMemCpyFn(const llvm::Type* dstType,
                                const llvm::Type* srcType,
                                const llvm::Type* sizeType);

    llvm::Value* wrapLoad(llvm::Value* val);

    llvm::Value* makeInt32Atom(int val);
    llvm::Value* makeIntAtom(llvm::Value* val, Typeid atomTypeId);
    llvm::Value* makeFloatAtom(llvm::Value* val, Typeid atomTypeId);
    llvm::Value* makeBoolAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(bool val);

    llvm::Value* makeCharAtom(Char val);
    llvm::Value* makeCharAtom(llvm::Value* val);

    llvm::Value* makeKeywordAtom(const String& keyword);
    llvm::Value* makeStringAtom(const String& str);


    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *func,
                                             const String& name,
                                             const llvm::Type* type);

    //! Create code snippet to extract the plain value from an atom.  This
    //! snippet may contain an external function call which includes type
    //! checking etc.
    llvm::Value* makeTypeCastAtomToPlain(llvm::Value* val,
                                         const Type& dstType) const;
    llvm::Value* emitPackCode(const Type& dstType, TypeConvKind convKind,
                              llvm::Value* value,
                              const Type& valType);

    //! create a ptr-to-int cast using the integer type which is large
    //! enough to fit to the pointer.
    llvm::Value* createCastPtrToNativeInt(llvm::Value* value) const;

    llvm::Value* emitTypeId(Typeid typid) const;

    llvm::Value* emitSizeTValue(size_t value) const;

    llvm::Value* coerceIntOperand(llvm::Value* value, const Type& dstType) const;
    llvm::Value* convertToPlainInt(llvm::Value* value,
                                   const Type& dstType,
                                   TypeConvKind typeConv) const;

  private:
    const char* getConvFuncNameByType(const Type& type) const;
    const llvm::Type* getConvTypeByType(const Type& type) const;
  };

};                              // namespace

#endif                          // bootstrap_codegen_tools_h
