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
    llvm::Value *left = tools()->wrapLoad(generator()->codegenNode(node->left()));
    llvm::Value *right = tools()->wrapLoad(generator()->codegenNode(node->right()));
    if (left == NULL || right == NULL)
      return NULL;
    return codegenOpIntInt(node, left, right);
  }

  else if (node->left()->type().isKeyword() && node->right()->type().isKeyword()) {
    llvm::Value *left = tools()->wrapLoad(generator()->codegenNode(node->left()));
    llvm::Value *right = tools()->wrapLoad(generator()->codegenNode(node->right()));
    if (left == NULL || right == NULL)
      return NULL;
    return codegenOpKeywKeyw(node, left, right);
  }

  else if (node->left()->type().isBool() && node->right()->type().isBool()) {
    llvm::Value *left = tools()->wrapLoad(generator()->codegenNode(node->left()));
    llvm::Value *right = tools()->wrapLoad(generator()->codegenNode(node->right()));
    if (left == NULL || right == NULL)
      return NULL;
    return codegenOpBoolBool(node, left, right);
  }

  else if (node->left()->type().isChar() && node->right()->type().isChar()) {
    llvm::Value *left = tools()->wrapLoad(generator()->codegenNode(node->left()));
    llvm::Value *right = tools()->wrapLoad(generator()->codegenNode(node->right()));
    if (left == NULL || right == NULL)
      return NULL;
    return codegenOpCharChar(node, left, right);
  }

  return codegenOpDucktype(node);
}


llvm::Value*
CodegenBinaryNode::coerceIntOperand(const Type& dstType, const Type& isType,
                                    llvm::Value* value) const
{
  return builder().CreateIntCast(value, types()->getType(dstType), isType.isSigned());
}


llvm::Value*
CodegenBinaryNode::wrapInt(llvm::Value* value, const Type& type, bool forceSigned) const
{
  if (type.isPlainType()) {
    if (forceSigned) {
      return coerceIntOperand(type, Type::newInt32(), value);
    }
    else
      return coerceIntOperand(type, type, value);
  }

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

llvm::Value*
CodegenBinaryNode::convertToPlainInt(const Type& dstType,
                                     const AptNode* right,
                                     llvm::Value* value) const
{
  switch (right->typeConv()) {
  case kNoConv:
    return coerceIntOperand(dstType, right->type(), value);
  case kAtom2PlainConv:
    return tools()->makeTypeCastAtomToPlain(value, dstType);
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return NULL;
}


llvm::Value*
CodegenBinaryNode::codegenOpIntInt(const BinaryNode* node,
                                   llvm::Value* left,
                                   llvm::Value* right) const
{
  llvm::Value* coleft = NULL;
  llvm::Value* coright = NULL;
  Type dstType;

  switch (node->op()) {
  case kOpPlus:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateAdd(coleft, coright, "addtmp"),
                   node->type());
  case kOpMinus:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateSub(coleft, coright, "subtmp"),
                   node->type());
  case kOpMultiply:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateMul(coleft, coright, "multmp"),
                   node->type());
  case kOpDivide:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateSDiv(coleft, coright, "divtmp"),
                   node->type());
  case kOpMod:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateSRem(coleft, coright, "modtmp"),
                   node->type());
  case kOpRem:
    coleft = convertToPlainInt(node->type(), node->left(), left);
    coright = convertToPlainInt(node->type(), node->right(), right);
    return wrapInt(builder().CreateURem(coleft, coright, "remtmp"),
                   node->type());

  case kOpLess:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLT(coleft, coright, "lttmp"),
                      node->type());
    else
      return wrapBool(builder().CreateICmpULT(coleft, coright, "lttmp"),
                      node->type());
  case kOpLessEqual:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSLE(coleft, coright, "letmp"),
                      node->type());
    else
      return wrapBool(builder().CreateICmpULE(coleft, coright, "letmp"),
                      node->type());
  case kOpEqual:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqtmp"),
                    node->type());
  case kOpUnequal:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    return wrapBool(builder().CreateICmpNE(coleft, coright, "netmp"),
                    node->type());
  case kOpGreater:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGT(coleft, coright, "gttmp"),
                      node->type());
    else
      return wrapBool(builder().CreateICmpUGT(coleft, coright, "gttmp"),
                      node->type());
  case kOpGreaterEqual:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    if (node->left()->type().isSigned())
      return wrapBool(builder().CreateICmpSGE(coleft, coright, "getmp"),
                      node->type());
    else
      return wrapBool(builder().CreateICmpUGE(coleft, coright, "getmp"),
                      node->type());

  case kOpCompare:
    dstType = maxIntType(node->left()->type(), node->right()->type());
    coleft = convertToPlainInt(dstType, node->left(), left);
    coright = convertToPlainInt(dstType, node->right(), right);
    return wrapInt(builder().CreateSub(coleft, coright, "cmptmp"),
                   node->type(), K(isSigned));
  default:
    fprintf(stderr, "invalid binary operator: %s",
            herschel::operatorName(node->op()));
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
                    node->type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(leftAsInt, rightAsInt, "nekw"),
                    node->type());

  default:
    fprintf(stderr, "invalid binary operator for keyword: %s\n",
            herschel::operatorName(node->op()));
    return NULL;
  }

  return NULL;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::coerceBoolOperand(const Type& dstType,
                                     llvm::Value* value) const
{
  return builder().CreateTruncOrBitCast(value,
                                        types()->getType(dstType));
}


llvm::Value*
CodegenBinaryNode::convertToPlainBool(const AptNode* dst,
                                      const AptNode* right,
                                      llvm::Value* value) const
{
  switch (right->typeConv()) {
  case kNoConv:
    return coerceBoolOperand(dst->type(), value);
  case kAtom2PlainConv:
    return tools()->makeTypeCastAtomToPlain(value, dst->type());
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return NULL;
}


llvm::Value*
CodegenBinaryNode::codegenOpBoolBool(const BinaryNode* node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  llvm::Value* coleft = convertToPlainBool(node->left(), node->left(), left);
  llvm::Value* coright = convertToPlainBool(node->left(), node->right(), right);

  switch (node->op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqbool"),
                    node->type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(coleft, coright, "nebool"),
                    node->type());

  case kOpLogicalAnd:
    return wrapBool(builder().CreateAnd(coleft, coright, "andbool"),
                    node->type());
  case kOpLogicalOr:
    return wrapBool(builder().CreateOr(coleft, coright, "orbool"),
                    node->type());

  default:
    fprintf(stderr, "invalid binary operator for bool: %s\n",
            herschel::operatorName(node->op()));
    return NULL;
  }

  return NULL;
}


//------------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::coerceCharOperand(const Type& dstType,
                                     llvm::Value* value) const
{
  return builder().CreateIntCast(value, types()->getType(dstType), !K(isSigned));
}


llvm::Value*
CodegenBinaryNode::convertToPlainChar(const AptNode* dst,
                                      const AptNode* right,
                                      llvm::Value* value) const
{
  switch (right->typeConv()) {
  case kNoConv:
    return coerceCharOperand(dst->type(), value);
  case kAtom2PlainConv:
    return tools()->makeTypeCastAtomToPlain(value, dst->type());
  case kPlain2AtomConv:
  case kTypeCheckConv:
    hr_invalid("");
  }

  return NULL;
}


llvm::Value*
CodegenBinaryNode::codegenOpCharChar(const BinaryNode* node,
                                     llvm::Value* left,
                                     llvm::Value* right) const
{
  llvm::Value* coleft = convertToPlainChar(node->left(), node->left(), left);
  llvm::Value* coright = convertToPlainChar(node->left(), node->right(), right);

  switch (node->op()) {
  case kOpEqual:
    return wrapBool(builder().CreateICmpEQ(coleft, coright, "eqchr"),
                    node->type());
  case kOpUnequal:
    return wrapBool(builder().CreateICmpNE(coleft, coright, "nechr"),
                    node->type());

  case kOpLess:
    return wrapBool(builder().CreateICmpULT(coleft, coright, "ltchr"),
                    node->type());
  case kOpLessEqual:
    return wrapBool(builder().CreateICmpULE(coleft, coright, "lechr"),
                    node->type());
  case kOpGreater:
    return wrapBool(builder().CreateICmpUGT(coleft, coright, "gtchr"),
                    node->type());
  case kOpGreaterEqual:
    return wrapBool(builder().CreateICmpUGE(coleft, coright, "gechr"),
                    node->type());
  case kOpCompare:
    return wrapInt(builder().CreateSub(coleft, coright, "cmpchr"),
                   node->type(), K(isSigned));

  default:
    fprintf(stderr, "invalid binary operator for char: %s\n",
            herschel::operatorName(node->op()));
    return NULL;
  }

  return NULL;
}


//-----------------------------------------------------------------------------

llvm::Value*
CodegenBinaryNode::codegenOpDucktype(const BinaryNode* node) const
{
  switch (node->op()) {
  case kOpPlus:
    return codegenOpDuckTypeBinary(node, Names::kLangAdd, Type::newAny());
  case kOpMinus:
    return codegenOpDuckTypeBinary(node, Names::kLangSubtract, Type::newAny());
  case kOpMultiply:
    return codegenOpDuckTypeBinary(node, Names::kLangMultiply, Type::newAny());
  case kOpDivide:
    return codegenOpDuckTypeBinary(node, Names::kLangDivide, Type::newAny());

  case kOpEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangEqualQ, Type::newBool());
  case kOpUnequal:
    return codegenOpDuckTypeBinary(node, Names::kLangUnequalQ, Type::newBool());
  case kOpLess:
    return codegenOpDuckTypeBinary(node, Names::kLangLessQ, Type::newBool());
  case kOpGreater:
    return codegenOpDuckTypeBinary(node, Names::kLangGreaterQ, Type::newBool());
  case kOpLessEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangLessEqualQ, Type::newBool());
  case kOpGreaterEqual:
    return codegenOpDuckTypeBinary(node, Names::kLangGreaterEqualQ, Type::newBool());
  case kOpCompare:
    return codegenOpDuckTypeBinary(node, Names::kLangCompare, Type::newInt32());

  default:
    fprintf(stderr, "binary operator not supported in ducktyping yet: %d", node->op());
    return NULL;
  }

  tyerror(node->left()->type(), "unsupported type in binary operator");
  tyerror(node->right()->type(), "unsupported type in binary operator");
  return NULL;
}



llvm::Value*
CodegenBinaryNode::codegenOpDuckTypeBinary(const BinaryNode* node,
                                           const String& funcnm,
                                           const Type& funcRetType) const
{
  String mangledFuncNm = herschel::mangleToC(funcnm);
  llvm::Function *calleeFunc = module()->getFunction(llvm::StringRef(mangledFuncNm));
  if (calleeFunc == NULL) {
    const AptNode* var = node->scope()->lookupVarOrFunc(funcnm,
                                                        K(showAmbiguousSymDef));
    if (const FuncDefNode* funcdef = dynamic_cast<const FuncDefNode*>(var)) {
      calleeFunc = CodegenFuncDef(generator()).emitExternFuncDef(funcdef);
      if (calleeFunc == NULL) {
        errorf(node->srcpos(), 0, "Unknown function referenced: %s",
               (const char*)StrHelper(funcnm));
        return NULL;
      }
    }
  }

  Ptr<AptNode> leftp = node->left();
  leftp->setTypeConv(leftp->type().isPlainType() ? kPlain2AtomConv : kNoConv);

  Ptr<AptNode> rightp = node->right();
  rightp->setTypeConv(rightp->type().isPlainType() ? kPlain2AtomConv : kNoConv);

  return CodegenApply(generator()).emitFunctionCall(node->srcpos(),
                                                    funcnm,
                                                    mangledFuncNm,
                                                    vector_of(leftp)(rightp),
                                                    Type::newAny(),
                                                    funcRetType,
                                                    kNoConv, // ???
                                                    !K(isInTailPos),
                                                    K(inlineRetv),
                                                    K(alwaysPassAtom));
}
