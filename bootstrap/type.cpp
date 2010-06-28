/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include <typeinfo>

#include "type.h"
#include "unittests.h"

using namespace heather;


//----------------------------------------------------------------------------

class GroupTypeImpl : public TypeImpl
{
public:
  GroupTypeImpl(const TypeVector& types)
    : fTypes(types)
  { }

  virtual bool isEqual(const TypeImpl* other) const
  {
    const GroupTypeImpl* o = dynamic_cast<const GroupTypeImpl*>(other);

    if (o != NULL && typeid(this) == typeid(other) && fTypes == o->fTypes) {
      for (size_t i = 0; i < fTypes.size(); ++i) {
        if (fTypes[i] != o->fTypes[i])
          return false;
      }
      return true;
    }
    return false;
  }


  virtual bool isCovariant(const TypeImpl* other) const
  {
    const GroupTypeImpl* o = dynamic_cast<const GroupTypeImpl*>(other);

    if (o != NULL && typeid(this) == typeid(other) && fTypes == o->fTypes) {
      for (size_t i = 0; i < fTypes.size(); ++i) {
        if (!fTypes[i].isCovariant(o->fTypes[i]))
          return false;
      }
      return true;
    }
    return false;
  }


  virtual bool isInvariant(const TypeImpl* other) const
  {
    const GroupTypeImpl* o = dynamic_cast<const GroupTypeImpl*>(other);

    if (o != NULL && fTypes == o->fTypes) {
      for (size_t i = 0; i < fTypes.size(); ++i) {
        if (!fTypes[i].isInvariant(o->fTypes[i]))
          return false;
      }
      return true;
    }
    return false;
  }

protected:
  TypeVector fTypes;
};


class UnionTypeImpl : public GroupTypeImpl
{
public:
  UnionTypeImpl(const TypeVector& types)
    : GroupTypeImpl(types)
  { }
};


class SeqTypeImpl : public GroupTypeImpl
{
public:
  SeqTypeImpl(const TypeVector& types)
    : GroupTypeImpl(types)
  { }
};


class FunctionTypeImpl : public TypeImpl
{
public:
  FunctionTypeImpl(const FunctionSignature& sign)
    : fSign(sign)
  { }


  virtual bool isEqual(const TypeImpl* other) const
  {
    const FunctionTypeImpl* o = dynamic_cast<const FunctionTypeImpl*>(other);
    return (o != NULL && fSign == o->fSign);
  }


  virtual bool isCovariant(const TypeImpl* other) const
  {
    const FunctionTypeImpl* o = dynamic_cast<const FunctionTypeImpl*>(other);
    return (o != NULL && fSign.isCovariant(o->fSign));
  }


  virtual bool isInvariant(const TypeImpl* other) const
  {
    const FunctionTypeImpl* o = dynamic_cast<const FunctionTypeImpl*>(other);
    return (o != NULL && fSign.isInvariant(o->fSign));
  }

private:
  FunctionSignature fSign;
};







Type::Type()
  : fKind(kType_Any)
{ }


Type::Type(const Type& other)
{
  *this = other;
}


Type::Type(TypeKind kind, TypeImpl* impl)
  : fKind(kind),
    fImpl(impl)
{ }


Type&
Type::operator=(const Type& other)
{
  fKind = other.fKind;
  fImpl = other.fImpl;
  return *this;
}


Type
Type::newAny()
{
  return Type(kType_Any, NULL);
}


Type
Type::newBool()
{
  return Type(kType_Bool, NULL);
}


Type
Type::newChar()
{
  return Type(kType_Char, NULL);
}


Type
Type::newInt()
{
  return Type(kType_Int, NULL);
}


Type
Type::newKeyword()
{
  return Type(kType_Keyword, NULL);
}


Type
Type::newLong()
{
  return Type(kType_Long, NULL);
}


Type
Type::newOctet()
{
  return Type(kType_Octet, NULL);
}


Type
Type::newRational()
{
  return Type(kType_Rational, NULL);
}


Type
Type::newReal()
{
  return Type(kType_Real, NULL);
}


Type
Type::newShort()
{
  return Type(kType_Short, NULL);
}


Type
Type::newString()
{
  return Type(kType_String, NULL);
}


Type
Type::newULong()
{
  return Type(kType_ULong, NULL);
}


Type
Type::newUShort()
{
  return Type(kType_UShort, NULL);
}


Type
Type::newUWord()
{
  return Type(kType_UWord, NULL);
}


Type
Type::newWord()
{
  return Type(kType_Word, NULL);
}


Type
Type::newEof()
{
  return Type(kType_Eof, NULL);
}


Type
Type::newNil()
{
  return Type(kType_Nil, NULL);
}


Type
Type::newUnspecified()
{
  return Type(kType_Unspecified, NULL);
}


Type
Type::newType(const String& name, const TypeVector& generics,
              const Type& inherit)
{
}


Type
Type::newType(const String& name, const TypeVector& generics,
              const Type& inherit,
              const FunctionSignatureVector& protocol)
{
}


Type
Type::newClass(const String& name, const TypeVector& generics,
               const Type& inherit)
{
}


Type
Type::newClass(const String& name, const TypeVector& generics,
               const Type& inherit,
               const FunctionSignature& defApplySign,
               const FunctionSignatureVector& protocol)
{
}


Type
Type::newAlias(const String& name, const TypeVector& generics,
               const Type& isa)
{
}


Type
Type::newMeasure(const String& name, const Type& baseType,
                 const String& defUnit)
{
}


Type
Type::newEnum(const String& name, const Type& baseType,
              const StringTokenMap& defUnit)
{
}


Type
Type::newFunction(const FunctionSignature& sign)
{
  return Type(kType_Function, new FunctionTypeImpl(sign));
}


Type
Type::newUnion(const TypeVector& types)
{
  return Type(kType_Union, new UnionTypeImpl(types));
}


Type
Type::newSeq(const TypeVector& types)
{
  return Type(kType_Sequence, new SeqTypeImpl(types));
}



bool
Type::operator==(const Type& other) const
{
  // TODO
}


bool
Type::operator!=(const Type& other) const
{
  return !(operator==(other));
}


bool
Type::isCovariant(const Type& other) const
{
  // TODO
}


bool
Type::isInvariant(const Type& other) const
{
  // TODO
}


bool
Type::isBase() const
{
  switch (fKind) {
  case kType_Bool:
  case kType_Char:
  case kType_Int:
  case kType_Keyword:
  case kType_Long:
  case kType_Octet:
  case kType_Rational:
  case kType_Real:
  case kType_Short:
  case kType_String:
  case kType_ULong:
  case kType_UShort:
  case kType_UWord:
  case kType_Word:
  case kType_Eof:
  case kType_Nil:
  case kType_Unspecified:
    return true;

  default:
    return false;
  }
}


bool
Type::isAny() const
{
  return fKind == kType_Any;
}


bool
Type::isInt() const
{
  return fKind == kType_Int;
}


bool
Type::isString() const
{
  return fKind == kType_String;
}


bool
Type::isReal() const
{
  return fKind == kType_Real;
}










//----------------------------------------------------------------------------

class LogicalConstraintImpl : public BaseTypeConstraintImpl
{
public:
  LogicalConstraintImpl(TypeConstOperator op,
                        const TypeConstraint& left,
                        const TypeConstraint& right)
    : fOp(op),
      fLeft(left),
      fRight(right)
  { }


  virtual bool isEqual(const BaseTypeConstraintImpl* other) const
  {
    const LogicalConstraintImpl* c =
      dynamic_cast<const LogicalConstraintImpl*>(other);

    return (c != NULL &&
            fOp == c->fOp &&
            fLeft == c->fLeft &&
            fRight == c->fRight);
  }


  virtual TypeConstOperator constOp() const
  {
    return fOp;
  }


  virtual BaseTypeConstraintImpl* unshare()
  {
    // TODO
    return this;
  }


  const TypeConstraint& left() const
  {
    return fLeft;
  }


  const TypeConstraint& right() const
  {
    return fRight;
  }

private:
  TypeConstOperator fOp;
  TypeConstraint    fLeft;
  TypeConstraint    fRight;
};


class ValueConstraintImpl : public BaseTypeConstraintImpl
{
public:
  ValueConstraintImpl(TypeConstOperator op, const Token& value)
    : fOp(op),
      fValue(value)
  { }


  virtual bool isEqual(const BaseTypeConstraintImpl* other) const
  {
    const ValueConstraintImpl* c =
      dynamic_cast<const ValueConstraintImpl*>(other);

    return (c != NULL &&
            fOp == c->fOp &&
            fValue == c->fValue);
  }


  virtual TypeConstOperator constOp() const
  {
    return fOp;
  }


  const Token& token() const
  {
    return fValue;
  }


  virtual BaseTypeConstraintImpl* unshare()
  {
    // TODO
    return this;
  }

private:
  TypeConstOperator fOp;
  Token fValue;
};


class TypeConstraintImpl : public BaseTypeConstraintImpl
{
public:
  TypeConstraintImpl(TypeConstOperator op, const Type& type)
    : fOp(op),
      fType(type)
  { }


  virtual bool isEqual(const BaseTypeConstraintImpl* other) const
  {
    const TypeConstraintImpl* c =
      dynamic_cast<const TypeConstraintImpl*>(other);

    return (c != NULL &&
            fType == c->fType);
  }


  virtual TypeConstOperator constOp() const
  {
    return fOp;
  }


  virtual BaseTypeConstraintImpl* unshare()
  {
    // TODO
    return this;
  }


  const Type& type() const
  {
    return fType;
  }


private:
  TypeConstOperator fOp;
  Type fType;
};



//----------------------------------------------------------------------------

TypeConstraint::TypeConstraint(BaseTypeConstraintImpl* impl)
  : fImpl(impl)
{ }


TypeConstraint::TypeConstraint(const TypeConstraint& other)
{
  *this = other;
}


TypeConstraint
TypeConstraint::newAnd(const TypeConstraint& left,
                       const TypeConstraint& right)
{
  return new LogicalConstraintImpl(kConstOp_and, left, right);
}


TypeConstraint
TypeConstraint::newOr(const TypeConstraint& left,
                      const TypeConstraint& right)
{
  return new LogicalConstraintImpl(kConstOp_or, left, right);
}


TypeConstraint
TypeConstraint::newValue(TypeConstOperator op, const Token& value)
{
  return new ValueConstraintImpl(op, value);
}


TypeConstraint
TypeConstraint::newType(TypeConstOperator op, const Type& type)
{
  return new TypeConstraintImpl(op, type);
}


TypeConstraint&
TypeConstraint::operator=(const TypeConstraint& other)
{
  fImpl = other.fImpl;
  return *this;
}


bool
TypeConstraint::operator==(const TypeConstraint& other) const
{
  if (constOp() == other.constOp())
    return fImpl->isEqual(other.fImpl);
  return false;
}


bool
TypeConstraint::operator!=(const TypeConstraint& other) const
{
  return !(operator==(other));
}


TypeConstOperator
TypeConstraint::constOp() const
{
  return fImpl->constOp();
}


bool
TypeConstraint::isValueConstraint() const
{
  switch (fImpl->constOp()) {
  case kConstOp_equal:
  case kConstOp_notEqual:
  case kConstOp_less:
  case kConstOp_lessEqual:
  case kConstOp_greater:
  case kConstOp_greaterEqual:
  case kConstOp_in:
    return true;

  case kConstOp_and:
  case kConstOp_or:
    return false;

  case kConstOp_isa:
    return false;
  }
  return false;
}


Token
TypeConstraint::constraintValue() const
{
  assert(isValueConstraint());
  return dynamic_cast<ValueConstraintImpl*>(fImpl.obj())->token();
}


bool
TypeConstraint::isLogicalConstraint() const
{
  switch (fImpl->constOp()) {
  case kConstOp_equal:
  case kConstOp_notEqual:
  case kConstOp_less:
  case kConstOp_lessEqual:
  case kConstOp_greater:
  case kConstOp_greaterEqual:
  case kConstOp_in:
    return false;

  case kConstOp_and:
  case kConstOp_or:
    return true;

  case kConstOp_isa:
    return false;
  }
  return false;
}


const TypeConstraint&
TypeConstraint::leftConstraint() const
{
  assert(isLogicalConstraint());
  return dynamic_cast<const LogicalConstraintImpl*>(fImpl.obj())->left();
}


const TypeConstraint&
TypeConstraint::rightConstraint() const
{
  assert(isLogicalConstraint());
  return dynamic_cast<const LogicalConstraintImpl*>(fImpl.obj())->right();
}


bool
TypeConstraint::isTypeConstraint() const
{
  switch (fImpl->constOp()) {
  case kConstOp_equal:
  case kConstOp_notEqual:
  case kConstOp_less:
  case kConstOp_lessEqual:
  case kConstOp_greater:
  case kConstOp_greaterEqual:
  case kConstOp_in:
    return false;

  case kConstOp_and:
  case kConstOp_or:
    return false;

  case kConstOp_isa:
    return true;
  }
  return false;
}


Type
TypeConstraint::typeConstraint() const
{
  assert(isTypeConstraint());
  return dynamic_cast<TypeConstraintImpl*>(fImpl.obj())->type();
}


//----------------------------------------------------------------------------

FunctionParameter::FunctionParameter(ParameterKind kind, bool isSpec,
                                     const String& key, const Type& type)
  : fKind(kind),
    fIsSpecialized(isSpec),
    fKey(key),
    fType(type)
{ }


FunctionParameter::FunctionParameter(const FunctionParameter& other)
{
  *this = other;
}


FunctionParameter
FunctionParameter::newPosParam(const Type& type)
{
  return FunctionParameter(kParamPos, false, String(), type);
}


FunctionParameter
FunctionParameter::newSpecParam(const Type& type)
{
  return FunctionParameter(kParamPos, true, String(), type);
}


FunctionParameter
FunctionParameter::newNamedParam(const String& key, const Type& type)
{
  return FunctionParameter(kParamNamed, false, key, type);
}


FunctionParameter
FunctionParameter::newRestParam(const Type& type)
{
  return FunctionParameter(kParamRest, false, String(), type);
}



FunctionParameter&
FunctionParameter::operator=(const FunctionParameter& other)
{
  fKind = other.fKind;
  fIsSpecialized = other.fIsSpecialized;
  fKey = other.fKey;
  fType = other.fType;

  return *this;
}


bool
FunctionParameter::operator==(const FunctionParameter& other) const
{
  return (fKind == other.fKind &&
          fIsSpecialized == other.fIsSpecialized &&
          fKey == other.fKey &&
          fType == other.fType);
}


bool
FunctionParameter::operator!=(const FunctionParameter& other) const
{
  return !(operator==(other));
}


bool
FunctionParameter::isCovariant(const FunctionParameter& other) const
{
  if (fKind != other.fKind)
    return false;
  if (fIsSpecialized != other.fIsSpecialized)
    return false;

  // the parameter key is not relevant for co-variance testing
  return fType.isCovariant(other.fType);
}


bool
FunctionParameter::isInvariant(const FunctionParameter& other) const
{
  if (fKind != other.fKind)
    return true;
  if (fIsSpecialized != other.fIsSpecialized)
    return true;

  // the parameter key is not relevant for co-variance testing
  return fType.isInvariant(other.fType);
}


bool
FunctionParameter::isContravariant(const FunctionParameter& other) const
{
  return !isCovariant(other) && !isInvariant(other);
}


FunctionParameter::ParameterKind
FunctionParameter::kind() const
{
  return fKind;
}


bool
FunctionParameter::isSpecialized() const
{
  return fIsSpecialized;
}


const String&
FunctionParameter::key() const
{
  return fKey;
}


const Type&
FunctionParameter::type() const
{
  return fType;
}


//----------------------------------------------------------------------------

FunctionSignature::FunctionSignature(bool isGeneric, const String& name,
                                     const Type& retType)
  : fIsGeneric(isGeneric),
    fName(name),
    fReturnType(retType)
{ }


FunctionSignature::FunctionSignature(bool isGeneric, const String& name,
                                     const Type& retType,
                                     const FunctionParamVector& parameters)
  : fIsGeneric(isGeneric),
    fName(name),
    fReturnType(retType),
    fParameters(parameters)
{ }


FunctionSignature::FunctionSignature(const FunctionSignature& other)
{
  *this = other;
}


FunctionSignature&
FunctionSignature::operator=(const FunctionSignature& other)
{
  fIsGeneric = other.fIsGeneric;
  fName = other.fName;
  fReturnType = other.fReturnType;
  fParameters = other.fParameters;

  return *this;
}


bool
FunctionSignature::operator==(const FunctionSignature& other) const
{
  if (fIsGeneric == other.fIsGeneric &&
      fName == other.fName &&
      fReturnType == other.fReturnType)
  {
    if (fParameters.size() == other.fParameters.size()) {
      for (size_t i = 0; i < fParameters.size(); ++i) {
        if (fParameters[i] != other.fParameters[i])
          return false;
      }
      return true;
    }
  }

  return false;
}


bool
FunctionSignature::operator!=(const FunctionSignature& other) const
{
  return !(operator==(other));
}


bool
FunctionSignature::isCovariant(const FunctionSignature& other) const
{
  // TODO: is fIsGeneric relevant for co-variance testing?

  if (fReturnType.isCovariant(other.fReturnType)) {
    if (fParameters.size() == other.fParameters.size()) {
      for (size_t i = 0; i < fParameters.size(); i++) {
        if (!fParameters[i].isCovariant(other.fParameters[i]))
          return false;
      }
      return true;
    }
  }

  return false;
}


bool
FunctionSignature::isInvariant(const FunctionSignature& other) const
{
  // TODO: is fIsGeneric relevant for co-variance testing?

  if (fReturnType.isInvariant(other.fReturnType))
    return true;
  if (fParameters.size() == other.fParameters.size()) {
    for (size_t i = 0; i < fParameters.size(); i++) {
      if (fParameters[i].isInvariant(other.fParameters[i]))
        return true;
    }
    return false;
  }
  return true;
}


bool
FunctionSignature::isContravariant(const FunctionSignature& other) const
{
  return !isCovariant(other) && !isInvariant(other);
}


bool
FunctionSignature::isGeneric() const
{
  return fIsGeneric;
}


const String&
FunctionSignature::methodName() const
{
  return fName;
}


const Type&
FunctionSignature::returnType() const
{
  return fReturnType;
}


const FunctionParamVector&
FunctionSignature::parameters() const
{
  return fParameters;
}



//============================================================================

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

class TypeUnitTest : public UnitTest
{
public:
  TypeUnitTest() : UnitTest("Type") {}

  virtual void run()
  {
  }
};
static TypeUnitTest typeUnitTest;


//----------------------------------------------------------------------------

class TypeConstraintUnitTest : public UnitTest
{
public:
  TypeConstraintUnitTest() : UnitTest("TypeConstraint") {}

  virtual void run()
  {
    SrcPos sp;
    {
      TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                                   Token(sp, kInt, 42));
      assert(t0.constOp() == kConstOp_equal);
    }

    {
      TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                                   Token(sp, kInt, 42));
      TypeConstraint t1 = TypeConstraint::newValue(kConstOp_equal,
                                                   Token(sp, kInt, 42));
      assert(t0.constOp() == kConstOp_equal);
      assert(t0 == t1);
    }

    {
      TypeConstraint t0 = TypeConstraint::newValue(kConstOp_notEqual,
                                                   Token(sp, kInt, 10));
      TypeConstraint t1 = TypeConstraint::newValue(kConstOp_notEqual,
                                                   Token(sp, kInt, 21));
      TypeConstraint t2 = TypeConstraint::newAnd(t0, t1);
      TypeConstraint t3 = TypeConstraint::newOr(t0, t2);

      assert(t2.constOp() == kConstOp_and);
      assert(t2.leftConstraint() == t0);
      assert(t2.rightConstraint() == t1);

      assert(t3.constOp() == kConstOp_or);
      assert(t3.leftConstraint() == t0);
      assert(t3.rightConstraint() == t2);
    }

    {
      TypeConstraint t0 = TypeConstraint::newType(kConstOp_isa,
                                                  Type::newInt());
      assert(t0.constOp() == kConstOp_isa);
      assert(t0.typeConstraint() == Type::newInt());
    }
  }
};
static TypeConstraintUnitTest typeConstraintUnitTest;


//----------------------------------------------------------------------------

class FunctionParameterUnitTest : public UnitTest
{
public:
  FunctionParameterUnitTest() : UnitTest("FunctionParameter") {}

  virtual void run()
  {
    {
      FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt());
      assert(p0.type().isInt());
      assert(!p0.isSpecialized());
      assert(p0.key().isEmpty());
      assert(p0.kind() == FunctionParameter::kParamPos);
    }

    {
      FunctionParameter p0 = FunctionParameter::newSpecParam(Type::newInt());
      assert(p0.type().isInt());
      assert(p0.isSpecialized());
      assert(p0.key().isEmpty());
      assert(p0.kind() == FunctionParameter::kParamPos);
    }

    {
      FunctionParameter p0 = FunctionParameter::newNamedParam(String("abc"),
                                                              Type::newInt());
      assert(p0.type().isInt());
      assert(!p0.isSpecialized());
      assert(p0.key() == String("abc"));
      assert(p0.kind() == FunctionParameter::kParamNamed);
    }

    {
      FunctionParameter p0 = FunctionParameter::newRestParam(Type::newInt());
      assert(p0.type().isInt());
      assert(!p0.isSpecialized());
      assert(p0.key().isEmpty());
      assert(p0.kind() == FunctionParameter::kParamRest);
    }

    {
      FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt());
      FunctionParameter p1 = FunctionParameter::newPosParam(Type::newInt());

      assert(p0 == p1);
      assert(p0.isCovariant(p1));
      assert(!p0.isContravariant(p1));
      assert(!p0.isInvariant(p1));
    }

    {
      FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt());
      FunctionParameter p1 = FunctionParameter::newPosParam(Type::newAny());

      assert(p0 != p1);

      assert(p0.isCovariant(p1));
      assert(!p0.isContravariant(p1));
      assert(!p0.isInvariant(p1));

      assert(!p1.isCovariant(p0));
      assert(p1.isContravariant(p0));
      assert(!p1.isInvariant(p0));
    }

    {
      FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt());
      FunctionParameter p1 = FunctionParameter::newPosParam(Type::newString());

      assert(p0 != p1);

      assert(!p0.isCovariant(p1));
      assert(!p0.isContravariant(p1));
      assert(p0.isInvariant(p1));

      assert(!p1.isCovariant(p0));
      assert(!p1.isContravariant(p0));
      assert(p1.isInvariant(p0));
    }
  }
};
static FunctionParameterUnitTest functionParameterUnitTest;


//----------------------------------------------------------------------------

class FunctionSignatureUnitTest : public UnitTest
{
public:
  FunctionSignatureUnitTest() : UnitTest("FunctionSignature") {}

  virtual void run()
  {
    FunctionSignature fs0 = FunctionSignature(false, String("abc"), Type::newInt());

    FunctionParamVector params1;
    params1.push_back(FunctionParameter::newSpecParam(Type::newString()));
    params1.push_back(FunctionParameter::newPosParam(Type::newInt()));
    params1.push_back(FunctionParameter::newNamedParam(String("xyz"), Type::newReal()));
    params1.push_back(FunctionParameter::newRestParam(Type::newAny()));

    FunctionSignature fs1 = FunctionSignature(true, String("man"), Type::newInt(),
                                              params1);

    assert(!fs0.isGeneric());
    assert(fs0.methodName() == String("abc"));
    assert(fs0.returnType() == Type::newInt());
    assert(fs0.parameters().empty());

    assert(fs1.isGeneric());
    assert(fs1.methodName() == String("man"));
    assert(fs1.returnType() == Type::newInt());
    assert(fs1.parameters().size() == 4);
    assert(fs1.parameters()[0].type().isString());
    assert(fs1.parameters()[0].kind() == FunctionParameter::kParamPos);
    assert(fs1.parameters()[0].isSpecialized());

    assert(fs1.parameters()[1].type().isInt());
    assert(fs1.parameters()[1].kind() == FunctionParameter::kParamPos);

    assert(fs1.parameters()[2].type().isReal());
    assert(fs1.parameters()[2].kind() == FunctionParameter::kParamNamed);

    assert(fs1.parameters()[3].type().isAny());
    assert(fs1.parameters()[3].kind() == FunctionParameter::kParamRest);
  }
};
static FunctionSignatureUnitTest functionSignatureUnitTest;


#endif  // #if defined(UNITTESTS)

