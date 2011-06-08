/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_binnode_h
#define bootstrap_codegen_binnode_h

#include "llvm/Support/IRBuilder.h"

#include "codegen-proxy.h"


namespace llvm
{
  class Module;
  class Value;
};


namespace herschel
{
  class CodeGenerator;
  class CodegenTypeUtils;
  class BinaryNode;
  class Type;
  class CodegenTools;

  class CodegenBinaryNode : public CodeGeneratorProxy
  {
  public:
    CodegenBinaryNode(CodeGenerator* generator);

    llvm::Value* emit(const BinaryNode* node) const;

  private:
    llvm::Value* coerceIntOperand(const Type& dstType, const Type& isType,
                                  llvm::Value* value) const;
    llvm::Value* wrapInt(llvm::Value* value, const Type& type) const;
    llvm::Value* wrapBool(llvm::Value* value, const Type& type) const;
    llvm::Value* codegenOpIntInt(const BinaryNode* node,
                                 llvm::Value* left,
                                 llvm::Value* right) const;
    llvm::Value* convertToPlainInt(const AptNode* dst,
                                   const AptNode* right,
                                   llvm::Value* value) const;

    bool isPlainInt(const Type& type) const;

    llvm::Value* codegenOpKeywKeyw(const BinaryNode* node,
                                   llvm::Value* left,
                                   llvm::Value* right) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_binnode_h
