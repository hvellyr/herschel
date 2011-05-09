/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"

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
#include "llvm/Support/raw_ostream.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"


//----------------------------------------------------------------------------

using namespace herschel;


llvm::Value*
CodeGenerator::coerceIntOperand(const Type& dstType, const Type& isType,
                                llvm::Value* value)
{
  return fBuilder.CreateIntCast(value, fTypes.getType(dstType), isType.isSigned());
}


llvm::Value*
CodeGenerator::wrapInt(llvm::Value* value, const Type& type)
{
  if (type.isPlainType())
    return coerceIntOperand(type, type, value);

  hr_assert(type.isAnyInt());
  return makeIntAtom(value);
}


llvm::Value*
CodeGenerator::wrapBool(llvm::Value* value, const Type& type)
{

  if (type.isPlainType())
    return value;

  hr_assert(type.isBool());
  return makeBoolAtom(value);
}


bool
CodeGenerator::isPlainInt(const Type& type) const
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
CodeGenerator::codegenOpIntInt(const BinaryNode* node,
                               llvm::Value* left,
                               llvm::Value* right)
{
  llvm::Value* coleft = NULL;
  llvm::Value* coright = NULL;

  if (isPlainInt(node->left()->type()))
    coleft = coerceIntOperand(node->left()->type(), node->left()->type(), left);
  else if (node->left()->type().isAnyInt())
    coleft = makeTypeCastAtomToPlain(left, node->left()->type());

  if (isPlainInt(node->left()->type()) && isPlainInt(node->right()->type()))
    coright = coerceIntOperand(node->left()->type(), node->right()->type(), right);
  else if (node->right()->type().isAnyInt())
    coright = makeTypeCastAtomToPlain(right, node->left()->type());

  switch (node->op()) {
  case kOpPlus:
    return wrapInt(fBuilder.CreateAdd(coleft, coright, "addtmp"),
                   node->dstType());
  case kOpMinus:
    return wrapInt(fBuilder.CreateSub(coleft, coright, "subtmp"),
                   node->dstType());
  case kOpMultiply:
    return wrapInt(fBuilder.CreateMul(coleft, coright, "multmp"),
                   node->dstType());
  case kOpDivide:
    return wrapInt(fBuilder.CreateSDiv(coleft, coright, "divtmp"),
                   node->dstType());
  case kOpMod:
    return wrapInt(fBuilder.CreateSRem(coleft, coright, "modtmp"),
                   node->dstType());
  case kOpRem:
    return wrapInt(fBuilder.CreateURem(coleft, coright, "remtmp"),
                   node->dstType());

  case kOpLess:
    if (node->left()->type().isSigned())
      return wrapBool(fBuilder.CreateICmpSLT(coleft, coright, "lttmp"),
                      node->dstType());
    else
      return wrapBool(fBuilder.CreateICmpULT(coleft, coright, "lttmp"),
                      node->dstType());
  case kOpLessEqual:
    if (node->left()->type().isSigned())
      return wrapBool(fBuilder.CreateICmpSLE(coleft, coright, "letmp"),
                      node->dstType());
    else
      return wrapBool(fBuilder.CreateICmpULE(coleft, coright, "letmp"),
                      node->dstType());
  case kOpEqual:
    return wrapBool(fBuilder.CreateICmpEQ(coleft, coright, "eqtmp"),
                    node->dstType());
  case kOpUnequal:
    return wrapBool(fBuilder.CreateICmpNE(coleft, coright, "netmp"),
                    node->dstType());
  case kOpGreater:
    if (node->left()->type().isSigned())
      return wrapBool(fBuilder.CreateICmpSGT(coleft, coright, "gttmp"),
                      node->dstType());
    else
      return wrapBool(fBuilder.CreateICmpUGT(coleft, coright, "gttmp"),
                      node->dstType());
  case kOpGreaterEqual:
    if (node->left()->type().isSigned())
      return wrapBool(fBuilder.CreateICmpSGE(coleft, coright, "getmp"),
                      node->dstType());
    else
      return wrapBool(fBuilder.CreateICmpUGE(coleft, coright, "getmp"),
                      node->dstType());

  default:
    fprintf(stderr, "invalid binary operator: %d", node->op());
    return NULL;
  }

  return NULL;
}
