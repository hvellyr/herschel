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
  class LLVMContext;
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

    void assignAtom(llvm::Value* src, llvm::Value* dst);
    llvm::Function* getIntrinsic(unsigned int iid,
                                 const llvm::Type** tys, unsigned int numTys);
    llvm::Function* getMemCpyFn(const llvm::Type* dstType,
                                const llvm::Type* srcType,
                                const llvm::Type* sizeType);

  private:
    //-------- data members

    Ptr<CodeGenerator> fGenerator; 
  };

};                              // namespace

#endif                          // bootstrap_codegen_tools_h
