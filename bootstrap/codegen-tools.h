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

  //----------------------------------------------------------------------------

  class CodegenTools : public RefCountable
  {
  public:
    CodegenTools(CodeGenerator* generator);

    enum Typeid {
      //kAtomAny    = 'A',
      kAtomBool   = 'b',
      kAtomInt32  = 'i',
      kAtomChar   = 'c'
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
    llvm::Value* makeBoolAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(bool val);

    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *func,
                                             const String& name,
                                             const llvm::Type* type);

    llvm::Value* makeTypeCastAtomToPlain(llvm::Value* val,
                                         const Type& dstType);
    llvm::Value* emitPackCode(const Type& dstType, TypeConvKind convKind,
                              llvm::Value* value,
                              const Type& valType);

  private:
    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;
    const CodegenTypeUtils* types() const;

    //-------- data members

    Ptr<CodeGenerator> fGenerator; 
  };

};                              // namespace

#endif                          // bootstrap_codegen_tools_h
