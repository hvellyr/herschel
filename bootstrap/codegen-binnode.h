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
    CodegenBinaryNode(CodeGenerator& generator);

    llvm::Value* emit(const BinaryNode* node) const;

  private:
    llvm::Value* codegenOpDucktype(const BinaryNode& node) const;
    llvm::Value* codegenOpDuckTypeBinary(const BinaryNode& node,
                                         const String& funcnm,
                                         const Type& funcRetType) const;

    llvm::Value* coerceIntOperand(const Type& dstType, const Type& isType,
                                  llvm::Value* value) const;
    llvm::Value* wrapInt(llvm::Value* value, const Type& type,
                         bool forceSigned = false) const;
    llvm::Value* wrapBool(llvm::Value* value, const Type& type) const;
    llvm::Value* codegenOpIntInt(const BinaryNode& node,
                                 llvm::Value* left,
                                 llvm::Value* right) const;
    llvm::Value* convertToPlainInt(const Type& dstType,
                                   const AptNode& right,
                                   llvm::Value* value) const;

    llvm::Value* codegenOpKeywKeyw(const BinaryNode& node,
                                   llvm::Value* left,
                                   llvm::Value* right) const;

    llvm::Value* coerceBoolOperand(const Type& dstType,
                                   llvm::Value* value) const;
    llvm::Value* convertToPlainBool(const AptNode& dst,
                                    const AptNode& right,
                                    llvm::Value* value) const;
    llvm::Value* codegenOpBoolBool(const BinaryNode& node,
                                   llvm::Value* left,
                                   llvm::Value* right) const;

    llvm::Value* coerceCharOperand(const Type& dstType,
                                   llvm::Value* value) const;
    llvm::Value* convertToPlainChar(const AptNode& dst,
                                    const AptNode& right,
                                    llvm::Value* value) const;
    llvm::Value* codegenOpCharChar(const BinaryNode& node,
                                   llvm::Value* left,
                                   llvm::Value* right) const;
  };

} // namespace
