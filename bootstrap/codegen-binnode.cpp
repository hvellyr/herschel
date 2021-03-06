/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"
#include "codegen-apply.h"
#include "codegen-init.h"
#include "codegen-tools.h"
#include "codegen-types.h"
#include "codegen-binnode.h"
#include "codegen-func.h"
#include "utils.h"

#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"

#include <vector>
#include <memory>


//----------------------------------------------------------------------------

using namespace herschel;

CodegenBinaryNode::CodegenBinaryNode(CodeGenerator& generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenBinaryNode::emit(const BinaryNode* node) const
{
  // fprintf(stderr, "BinaryNode: %s [%d]\n", herschel::operatorName(node->op()),
  //         node->typeConv());
  // tyerror(node->type(), "type");
  // tyerror(node->dstType(), "dsttype");

  /*
    int -> to plain int, op, dsttype is atom -> make_int_atom
    float -> to plain float, op, dsttype is atom -> make_float_atom
    char -> to plain float, op, dsttype is atom -> make_char_atom
    bool -> to plain bool, op, dsttype is atom -> make_bool_atom
    atom -> call operator(), dsttype is plain -> make_plain
  */

  if (node->left()->type().isAnyInt() && node->right()->type().isAnyInt()) {
    auto left = tools().wrapLoad(generator().codegenNode(*node->left()));
    auto right = tools().wrapLoad(generator().codegenNode(*node->right()));
    if (!left || !right)
      return nullptr;
    return codegenOpIntInt(*node, left, right);
  }

  else if (node->left()->type().isKeyword() && node->right()->type().isKeyword()) {
    auto left = tools().wrapLoad(generator().codegenNode(*node->left()));
    auto right = tools().wrapLoad(generator().codegenNode(*node->right()));
    if (!left || !right)
      return nullptr;
    return codegenOpKeywKeyw(*node, left, right);
  }

  else if (node->left()->type().isBool() && node->right()->type().isBool()) {
    auto left = tools().wrapLoad(generator().codegenNode(*node->left()));
    auto right = tools().wrapLoad(generator().codegenNode(*node->right()));
    if (!left || !right)
      return nullptr;
    return codegenOpBoolBool(*node, left, right);
  }

  else if (node->left()->type().isChar() && node->right()->type().isChar()) {
    auto left = tools().wrapLoad(generator().codegenNode(*node->left()));
    auto right = tools().wrapLoad(generator().codegenNode(*node->right()));
    if (!left || !right)
      return nullptr;
    return codegenOpCharChar(*node, left, right);
  }

  return codegenOpDucktype(*node);
}


llvm::Value*
CodegenBinaryNode::coerceIntOperand(const Type& dstType, const Type& isType,
                                    llvm::Value* value) const
{
  return builder().CreateIntCast(value, types().getType(dstType), isType.isSigned());
}


llvm::Value*
CodegenBinaryNode::wrapInt(llvm::Value* value, const Type& type, bool forceSigned) const
{
  if (type.isPlainType()) {
    if (forceSigned) {
      return coerceIntOperand(type, Type::makeInt32(), value);
    }
    else
      return coerceIntOperand(type, type, value);
  }

  hr_assert(type.isAnyInt());
  if (type.isInt32())
    return tools().makeIntAtom(value, CodegenTools::kAtomInt32);

  hr_invalid("TODO unhandled int type");

  return nullptr;
}


llvm::Value*
CodegenBinaryNode::wrapBool(llvm::Value* value, const Type& type) const
{
  if (type.isPlainType())
    return value;

  hr_assert(type.isBool());
  return tools().makeBoolAtom(value);
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::convertToPlainInt(const Type& dstType,
                                     const AptNode& right,
                                     llvm::Value* value) const
{
  switch (right.typeConv()) {
  case kNoConv:
    return coerceIntOperand(dstType, right.type(), value);
  case kAtom2PlainConv:
    return tools().makeTypeCastAtomToPlain(value, dstType);
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return nullptr;
}


llvm::Value*
CodegenBinaryNode::codegenOpIntInt(const BinaryNode& node,
                                   llvm::Value* left,
                                   llvm::Value* right) const
{
  llvm::Value* coleft = nullptr;
  llvm::Value* coright = nullptr;
  Type dstType;

  switch (node.op()) {
  case kOpPlus:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateAdd(coleft, coright, "addtmp"),
                   node.type());
  case kOpMinus:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateSub(coleft, coright, "subtmp"),
                   node.type());
  case kOpMultiply:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateMul(coleft, coright, "multmp"),
                   node.type());
  case kOpDivide:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateSDiv(coleft, coright, "divtmp"),
                   node.type());
  case kOpMod:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateSRem(coleft, coright, "modtmp"),
                   node.type());
  case kOpRem:
    coleft = convertToPlainInt(node.type(), *node.left(), left);
    coright = convertToPlainInt(node.type(), *node.right(), right);
    return wrapInt(builder().CreateURem(coleft, coright, "remtmp"),
                   node.type());

  case kOpLess:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    if (node.left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLT(coleft, coright, "lttmp"),
                      node.type());
    else
      return wrapBool(builder().CreateICmpULT(coleft, coright, "lttmp"),
                      node.type());
  case kOpLessEqual:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    if (node.left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLE(coleft, coright, "letmp"),
                      node.type());
    else
      return wrapBool(builder().CreateICmpULE(coleft, coright, "letmp"),
                      node.type());
  case kOpEqual:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqtmp"),
                    node.type());
  case kOpUnequal:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    return wrapBool(builder().CreateICmpNE(coleft, coright, "netmp"),
                    node.type());
  case kOpGreater:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    if (node.left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGT(coleft, coright, "gttmp"),
                      node.type());
    else
      return wrapBool(builder().CreateICmpUGT(coleft, coright, "gttmp"),
                      node.type());
  case kOpGreaterEqual:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    if (node.left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGE(coleft, coright, "getmp"),
                      node.type());
    else
      return wrapBool(builder().CreateICmpUGE(coleft, coright, "getmp"),
                      node.type());

  case kOpCompare:
    dstType = maxIntType(node.left()->type(), node.right()->type());
    coleft = convertToPlainInt(dstType, *node.left(), left);
    coright = convertToPlainInt(dstType, *node.right(), right);
    return wrapInt(builder().CreateSub(coleft, coright, "cmptmp"),
                   node.type(), K(isSigned));
  default:
    fprintf(stderr, "invalid binary operator: %s",
            herschel::operatorName(node.op()));
    return nullptr;
  }

  return nullptr;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::codegenOpKeywKeyw(const BinaryNode& node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  llvm::Value* plainLeft = tools().makeTypeCastAtomToPlain(left,
                                                            Type::makeKeyword());
  llvm::Value* plainRight = tools().makeTypeCastAtomToPlain(right,
                                                             Type::makeKeyword());

  llvm::Value* leftAsInt = tools().createCastPtrToNativeInt(plainLeft);
  llvm::Value* rightAsInt = tools().createCastPtrToNativeInt(plainRight);

  switch (node.op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(leftAsInt, rightAsInt, "eqkw"),
                    node.type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(leftAsInt, rightAsInt, "nekw"),
                    node.type());

  default:
    fprintf(stderr, "invalid binary operator for keyword: %s\n",
            herschel::operatorName(node.op()));
    return nullptr;
  }

  return nullptr;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::coerceBoolOperand(const Type& dstType,
                                     llvm::Value* value) const
{
  return builder().CreateTruncOrBitCast(value,
                                        types().getType(dstType));
}


llvm::Value*
CodegenBinaryNode::convertToPlainBool(const AptNode& dst,
                                      const AptNode& right,
                                      llvm::Value* value) const
{
  switch (right.typeConv()) {
  case kNoConv:
    return coerceBoolOperand(dst.type(), value);
  case kAtom2PlainConv:
    return tools().makeTypeCastAtomToPlain(value, dst.type());
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return nullptr;
}


llvm::Value*
CodegenBinaryNode::codegenOpBoolBool(const BinaryNode& node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  llvm::Value* coleft = convertToPlainBool(*node.left(), *node.left(), left);
  llvm::Value* coright = convertToPlainBool(*node.left(), *node.right(), right);

  switch (node.op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqbool"),
                    node.type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(coleft, coright, "nebool"),
                    node.type());

  case kOpLogicalAnd:
    return wrapBool(builder().CreateAnd(coleft, coright, "andbool"),
                    node.type());
  case kOpLogicalOr:
    return wrapBool(builder().CreateOr(coleft, coright, "orbool"),
                    node.type());

  default:
    fprintf(stderr, "invalid binary operator for bool: %s\n",
            herschel::operatorName(node.op()));
    return nullptr;
  }

  return nullptr;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::coerceCharOperand(const Type& dstType,
                                     llvm::Value* value) const
{
  return builder().CreateIntCast(value, types().getType(dstType), !K(isSigned));
}


llvm::Value*
CodegenBinaryNode::convertToPlainChar(const AptNode& dst,
                                      const AptNode& right,
                                      llvm::Value* value) const
{
  switch (right.typeConv()) {
  case kNoConv:
    return coerceCharOperand(dst.type(), value);
  case kAtom2PlainConv:
    return tools().makeTypeCastAtomToPlain(value, dst.type());
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return nullptr;
}


llvm::Value*
CodegenBinaryNode::codegenOpCharChar(const BinaryNode& node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  auto coleft = convertToPlainChar(*node.left(), *node.left(), left);
  auto coright = convertToPlainChar(*node.left(), *node.right(), right);

  switch (node.op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqchr"),
                    node.type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(coleft, coright, "nechr"),
                    node.type());

  case kOpLess:
    return wrapBool(builder().CreateICmpULT(coleft, coright, "ltchr"),
                    node.type());
  case kOpLessEqual:
    return wrapBool(builder().CreateICmpULE(coleft, coright, "lechr"),
                    node.type());
  case kOpGreater:
    return wrapBool(builder().CreateICmpUGT(coleft, coright, "gtchr"),
                    node.type());
  case kOpGreaterEqual:
    return wrapBool(builder().CreateICmpUGE(coleft, coright, "gechr"),
                    node.type());
  case kOpCompare:
    return wrapInt(builder().CreateSub(coleft, coright, "cmpchr"),
                   node.type(), K(isSigned));

  default:
    fprintf(stderr, "invalid binary operator for char: %s\n",
            herschel::operatorName(node.op()));
    return nullptr;
  }

  return nullptr;
}


//-----------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::codegenOpDucktype(const BinaryNode& node) const
{
  switch (node.op()) {
  case kOpPlus:
    return codegenOpDuckTypeBinary(node, Names::kLangAdd, Type::makeAny());
  case kOpMinus:
    return codegenOpDuckTypeBinary(node, Names::kLangSubtract, Type::makeAny());
  case kOpMultiply:
    return codegenOpDuckTypeBinary(node, Names::kLangMultiply, Type::makeAny());
  case kOpDivide:
    return codegenOpDuckTypeBinary(node, Names::kLangDivide, Type::makeAny());

  case kOpEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangEqualQ, Type::makeBool());
  case kOpUnequal:
    return codegenOpDuckTypeBinary(node, Names::kLangUnequalQ, Type::makeBool());
  case kOpLess:
    return codegenOpDuckTypeBinary(node, Names::kLangLessQ, Type::makeBool());
  case kOpGreater:
    return codegenOpDuckTypeBinary(node, Names::kLangGreaterQ, Type::makeBool());
  case kOpLessEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangLessEqualQ, Type::makeBool());
  case kOpGreaterEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangGreaterEqualQ, Type::makeBool());
  case kOpCompare:
    return codegenOpDuckTypeBinary(node, Names::kLangCompare, Type::makeInt32());

  case kOpConcat:
    return codegenOpDuckTypeBinary(node, Names::kLangConcat, Type::makeAny());

  default:
    fprintf(stderr, "binary operator not supported in ducktyping yet: %s\n",
            herschel::operatorName(node.op()));
    return nullptr;
  }

  tyerror(node.left()->type(), "unsupported type in binary operator");
  tyerror(node.right()->type(), "unsupported type in binary operator");
  return nullptr;
}



llvm::Value*
CodegenBinaryNode::codegenOpDuckTypeBinary(const BinaryNode& node,
                                           const String& funcnm,
                                           const Type& funcRetType) const
{
  String mangledFuncNm = herschel::mangleToC(funcnm);
  llvm::Function *calleeFunc = module()->getFunction(llvm::StringRef(mangledFuncNm));
  if (!calleeFunc) {
    const AptNode* var = node.scope()->lookupVarOrFunc(funcnm,
                                                        K(showAmbiguousSymDef));
    if (const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(var)) {
      calleeFunc = CodegenFuncDef(generator()).emitExternFuncDef(funcdef);
      if (!calleeFunc) {
        errorf(node.srcpos(), 0, "Unknown function referenced: %s",
               (zstring)StrHelper(funcnm));
        return nullptr;
      }
    }
  }

  auto leftp = node.left();
  leftp->setTypeConv(leftp->type().isPlainType() ? kPlain2AtomConv : kNoConv);

  auto rightp = node.right();
  rightp->setTypeConv(rightp->type().isPlainType() ? kPlain2AtomConv : kNoConv);

  return CodegenApply(generator()).emitFunctionCall(node.srcpos(),
                                                    funcnm,
                                                    mangledFuncNm,
                                                    makeVector(leftp, rightp),
                                                    Type::makeAny(),
                                                    funcRetType,
                                                    kNoConv, // ???
                                                    !K(isInTailPos),
                                                    K(inlineRetv),
                                                    K(alwaysPassAtom));
}
