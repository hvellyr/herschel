/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_codegen_apply_h
#define bootstrap_codegen_apply_h

#include "llvm/IR/IRBuilder.h"

#include "codegen-proxy.h"


namespace llvm
{
  class Module;
  class Value;
  class Function;
};


namespace herschel
{
  class BinaryNode;
  class CodeGenerator;
  class CodegenTypeUtils;
  class SymbolNode;
  class Type;


  class CodegenApply : public CodeGeneratorProxy
  {
  public:
    CodegenApply(CodeGenerator* generator);

    llvm::Value* emit(const ApplyNode* node) const;

    llvm::Value* emitFunctionCall(const SrcPos& srcpos,
                                  const String& clearFuncnm,
                                  const String& mangledFuncnm,
                                  const NodeList& args,
                                  const Type& returnType,
                                  const Type& dstType,
                                  TypeConvKind dstConvKind,
                                  bool isInTailPos,
                                  bool inlineRetv,
                                  bool alwaysPassAtom) const;

  private:
    friend class ArrayAllocateStrategy;
    friend class AtomArrayAllocateStrategy;
    friend class Int32ArrayAllocateStrategy;

    llvm::Value* emitTypeNameForAllocate(const AptNode* node) const;
    llvm::Value* emitAllocateApply(const ApplyNode* node) const;
    llvm::Value* emitAllocateApplyImpl(const AptNode* typeNode) const;
    llvm::Value* emitAllocateArrayApply(const ApplyNode* node) const;

    llvm::Value* emitGetSlotApply(const ApplyNode* node) const;
    llvm::Value* emitSetSlotApply(const ApplyNode* node) const;

    llvm::Value* emitPtrToSlot(const ApplyNode* node, bool isStore) const;

    //! take an unmangled function name (as written in herschel code)
    llvm::Function* lazyDeclareExternFunction(const String& funcnm) const;

    llvm::Value* emitArraySliceAccess(const ApplyNode* node) const;
    llvm::Value* emitArraySliceSet(const ApplyNode* node) const;
    llvm::Value* emitArrayNumItems(const ApplyNode* node) const;

    llvm::Value* emitIsaApply(const ApplyNode* applyNode) const;
    llvm::Value* emitToCharApply(const ApplyNode* applyNode) const;

    llvm::Value* convertToPlainInt(const Type& dstType,
                                   const AptNode* isNode,
                                   llvm::Value* value) const;

    struct ArraySliceAccessData
    {
      llvm::Value* fArray;
      llvm::Value* fAddr;
    };

    ArraySliceAccessData emitArraySliceAddress(const ApplyNode* node) const;
    ArraySliceAccessData emitArraySliceAddress(llvm::Value* arrayAtom,
                                               const Type& arrayBaseType,
                                               llvm::Value* idxValue) const;

    llvm::Value* emitArraySize(const ApplyNode* node) const;
  };
};                              // namespace

#endif                          // bootstrap_codegen_apply_h
