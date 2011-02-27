/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/


#include "parsertypes.h"
#include "token.h"

using namespace herschel;



OperatorType
herschel::tokenTypeToOperator(TokenType type)
{
  switch (type) {
  case kPlus:          return kOpPlus;
  case kMinus:         return kOpMinus;
  case kDivide:        return kOpDivide;
  case kMultiply:      return kOpMultiply;
  case kExponent:      return kOpExponent;
  case kFold:          return kOpFold;
  case kCompare:       return kOpCompare;
  case kEqual:         return kOpEqual;
  case kUnequal:       return kOpUnequal;
  case kLess:          return kOpLess;
  case kLessEqual:     return kOpLessEqual;
  case kGreater:       return kOpGreater;
  case kGreaterEqual:  return kOpGreaterEqual;
  case kAssign:        return kOpAssign;
  case kIn:            return kOpIn;
  case kMod:           return kOpMod;
  case kRem:           return kOpRem;
  case kIsa:           return kOpIsa;
  case kAs:            return kOpAs;
  case kBy:            return kOpBy;
  case kLogicalAnd:    return kOpLogicalAnd;
  case kLogicalOr:     return kOpLogicalOr;
  case kBitAnd:        return kOpBitAnd;
  case kBitOr:         return kOpBitOr;
  case kBitXor:        return kOpBitXor;
  case kShiftLeft:     return kOpShiftLeft;
  case kShiftRight:    return kOpShiftRight;
  case kRange:         return kOpRange;
  case kAppend:        return kOpAppend;
  case kRemove:        return kOpRemove;
  case kThenId:        return kOpThen;
  case kWhileId:       return kOpWhile;

  default:
    return kOpInvalid;
  }
}


TokenType
herschel::operatorToTokenType(OperatorType op)
{
  switch (op) {
  case kOpPlus:          return kPlus;
  case kOpMinus:         return kMinus;
  case kOpDivide:        return kDivide;
  case kOpMultiply:      return kMultiply;
  case kOpExponent:      return kExponent;
  case kOpFold:          return kFold;
  case kOpCompare:       return kCompare;
  case kOpEqual:         return kEqual;
  case kOpUnequal:       return kUnequal;
  case kOpLess:          return kLess;
  case kOpLessEqual:     return kLessEqual;
  case kOpGreater:       return kGreater;
  case kOpGreaterEqual:  return kGreaterEqual;
  case kOpAssign:        return kAssign;
  case kOpIn:            return kIn;
  case kOpMod:           return kMod;
  case kOpRem:           return kRem;
  case kOpIsa:           return kIsa;
  case kOpAs:            return kAs;
  case kOpBy:            return kBy;
  case kOpLogicalAnd:    return kLogicalAnd;
  case kOpLogicalOr:     return kLogicalOr;
  case kOpBitAnd:        return kBitAnd;
  case kOpBitOr:         return kBitOr;
  case kOpBitXor:        return kBitXor;
  case kOpShiftLeft:     return kShiftLeft;
  case kOpShiftRight:    return kShiftRight;
  case kOpRange:         return kRange;
  case kOpAppend:        return kAppend;
  case kOpRemove:        return kRemove;
  case kOpThen:          return kThenId;
  case kOpWhile:         return kWhileId;

  case kOpMapTo:
    return kInvalid;

  case kOpInvalid:
    assert(0);
  }

  return kInvalid;
}


ConfigVarRegistry::ConfigVarRegistry()
{}


ConfigVarRegistry::ConfigVarRegistry(ConfigVarRegistry* parent)
  : fParent(parent)
{}


bool
ConfigVarRegistry::lookup(const String& name, Token* out) const
{
  if (fParent != NULL && fParent->lookup(name, out))
    return true;
  return Registry<Token>::lookup(name, out);
}
