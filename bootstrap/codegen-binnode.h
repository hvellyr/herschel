/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_binnode_h
#define bootstrap_codegen_binnode_h

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


  class CodegenBinaryNode
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

    bool isPlainInt(const Type& type) const;

    CodeGenerator* generator() const;
    llvm::LLVMContext& context() const;
    llvm::IRBuilder<>& builder() const;
    llvm::Module* module() const;
    CodegenTypeUtils* types();
    const CodegenTypeUtils* types() const;
    CodegenTools* tools() const;

    Ptr<CodeGenerator> fGenerator;
  };
};                              // namespace

#endif                          // bootstrap_codegen_binnode_h
