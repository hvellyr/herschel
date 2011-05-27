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
#include "codegen-init.h"
#include "codegen-tools.h"
#include "codegen-types.h"
#include "codegen-binnode.h"

#include <vector>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"


//----------------------------------------------------------------------------

using namespace herschel;

CodegenBinaryNode::CodegenBinaryNode(CodeGenerator* generator)
  : CodeGeneratorProxy(generator)
{
}


llvm::Value*
CodegenBinaryNode::emit(const BinaryNode* node) const
{
  // fprintf(stderr, "BinaryNode: %s [%d]\n", XmlRenderer::operatorName(node->op()),
  //         node->typeConv());
  // tyerror(node->type(), "type");
  // tyerror(node->dstType(), "dsttype");
  llvm::Value *left = tools()->wrapLoad(generator()->codegenNode(node->left()));
  llvm::Value *right = tools()->wrapLoad(generator()->codegenNode(node->right()));
  if (left == NULL || right == NULL)
    return NULL;

  /*
    int -> to plain int, op, dsttype is atom -> make_int_atom
    float -> to plain float, op, dsttype is atom -> make_float_atom
    char -> to plain float, op, dsttype is atom -> make_char_atom
    bool -> to plain bool, op, dsttype is atom -> make_bool_atom
    atom -> call operator(), dsttype is plain -> make_plain
  */

  if (node->left()->type().isAnyInt() && node->right()->type().isAnyInt())
    return codegenOpIntInt(node, left, right);
  if (node->left()->type().isKeyword() && node->right()->type().isKeyword())
    return codegenOpKeywKeyw(node, left, right);

  tyerror(node->left()->type(), "unsupported type in binary operator");
  tyerror(node->right()->type(), "unsupported type in binary operator");
  return NULL;
}


llvm::Value*
CodegenBinaryNode::coerceIntOperand(const Type& dstType, const Type& isType,
                                    llvm::Value* value) const
{
  return builder().CreateIntCast(value, types()->getType(dstType), isType.isSigned());
}


llvm::Value*
CodegenBinaryNode::wrapInt(llvm::Value* value, const Type& type) const
{
  if (type.isPlainType())
    return coerceIntOperand(type, type, value);

  hr_assert(type.isAnyInt());
  if (type.isInt32())
    return tools()->makeIntAtom(value, CodegenTools::kAtomInt32);

  hr_invalid("TODO unhandled int type");

  return NULL;
}


llvm::Value*
CodegenBinaryNode::wrapBool(llvm::Value* value, const Type& type) const
{

  if (type.isPlainType())
    return value;

  hr_assert(type.isBool());
  return tools()->makeBoolAtom(value);
}


//------------------------------------------------------------------------------

bool
CodegenBinaryNode::isPlainInt(const Type& type) const
{
  return ( type.typeName() == String("lang|Int64")  ||
           type.typeName() == String("lang|Int32")  ||
           type.typeName() == String("lang|Int16")  ||
           type.typeName() == String("lang|Int8")   ||
           type.typeName() == String("lang|UInt64") ||
           type.typeName() == String("lang|UInt32") ||
           type.typeName() == String("lang|UInt16") ||
           type.typeName() == String("lang|UInt8") );
}


llvm::Value*
CodegenBinaryNode::codegenOpIntInt(const BinaryNode* node,
                                   llvm::Value* left,
                                   llvm::Value* right) const
{
  llvm::Value* coleft = NULL;
  llvm::Value* coright = NULL;

  if (isPlainInt(node->left()->type()))
    coleft = coerceIntOperand(node->left()->type(), node->left()->type(), left);
  else if (node->left()->type().isAnyInt())
    coleft = tools()->makeTypeCastAtomToPlain(left, node->left()->type());

  if (isPlainInt(node->left()->type()) && isPlainInt(node->right()->type()))
    coright = coerceIntOperand(node->left()->type(), node->right()->type(), right);
  else if (node->right()->type().isAnyInt())
    coright = tools()->makeTypeCastAtomToPlain(right, node->left()->type());

  switch (node->op()) {
  case kOpPlus:
    return wrapInt(builder().CreateAdd(coleft, coright, "addtmp"),
                   node->dstType());
  case kOpMinus:
    return wrapInt(builder().CreateSub(coleft, coright, "subtmp"),
                   node->dstType());
  case kOpMultiply:
    return wrapInt(builder().CreateMul(coleft, coright, "multmp"),
                   node->dstType());
  case kOpDivide:
    return wrapInt(builder().CreateSDiv(coleft, coright, "divtmp"),
                   node->dstType());
  case kOpMod:
    return wrapInt(builder().CreateSRem(coleft, coright, "modtmp"),
                   node->dstType());
  case kOpRem:
    return wrapInt(builder().CreateURem(coleft, coright, "remtmp"),
                   node->dstType());

  case kOpLess:
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLT(coleft, coright, "lttmp"),
                      node->dstType());
    else
      return wrapBool(builder().CreateICmpULT(coleft, coright, "lttmp"),
                      node->dstType());
  case kOpLessEqual:
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLE(coleft, coright, "letmp"),
                      node->dstType());
    else
      return wrapBool(builder().CreateICmpULE(coleft, coright, "letmp"),
                      node->dstType());
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqtmp"),
                    node->dstType());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(coleft, coright, "netmp"),
                    node->dstType());
  case kOpGreater:
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGT(coleft, coright, "gttmp"),
                      node->dstType());
    else
      return wrapBool(builder().CreateICmpUGT(coleft, coright, "gttmp"),
                      node->dstType());
  case kOpGreaterEqual:
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGE(coleft, coright, "getmp"),
                      node->dstType());
    else
      return wrapBool(builder().CreateICmpUGE(coleft, coright, "getmp"),
                      node->dstType());

  default:
    fprintf(stderr, "invalid binary operator: %d", node->op());
    return NULL;
  }

  return NULL;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::codegenOpKeywKeyw(const BinaryNode* node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  llvm::Value* plainLeft = tools()->makeTypeCastAtomToPlain(left,
                                                            Type::newKeyword());
  llvm::Value* plainRight = tools()->makeTypeCastAtomToPlain(right,
                                                             Type::newKeyword());

  llvm::Value* leftAsInt = tools()->createCastPtrToNativeInt(plainLeft);
  llvm::Value* rightAsInt = tools()->createCastPtrToNativeInt(plainRight);

  switch (node->op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(leftAsInt, rightAsInt, "eqkw"),
                    node->dstType());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(leftAsInt, rightAsInt, "nekw"),
                    node->dstType());

  default:
    fprintf(stderr, "invalid binary operator for keyword: %d", node->op());
    return NULL;
  }

  return NULL;
}


