/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include <typeinfo>

#include "type.h"
#include "typectx.h"
#include "strbuf.h"
#include "unittests.h"

using namespace heather;


//----------------------------------------------------------------------------

namespace heather
{
  template<typename T>
  static bool
  isEqual(const T& vect0, const T& vect1)
  {
    if (vect0.size() == vect1.size()) {
      for (size_t i = 0; i < vect0.size(); i++) {
        if (vect0[i] != vect1[i])
          return false;
      }
    }
    return true;
  }


  template<typename T>
  static bool
  isCovariant(const T& vect0, const T& vect1)
  {
    if (vect0.size() == vect1.size()) {
      for (size_t i = 0; i < vect0.size(); i++) {
        if (!vect0[i].isCovariant(vect1[i]))
          return false;
      }
    }
    return true;
  }


  template<typename T>
  static bool
  isInvariant(const T& vect0, const T& vect1)
  {
    if (vect0.size() != vect1.size())
      return true;
    for (size_t i = 0; i < vect0.size(); i++) {
      if (vect0[i].isInvariant(vect1[i]))
        return true;
    }
    return false;
  }


  template<typename T>
  T vectorClone(const T& v)
  {
    T result;
    result.reserve(v.size());

    for (size_t i = 0; i < v.size(); i++) {
      result.push_back(v[i].clone());
    }

    return result;
  }


  template<typename T>
  void replaceGenerics(std::vector<T>& v, const TypeCtx& typeMap)
  {
    for (size_t i = 0; i < v.size(); i++) {
      T replacement = v[i].replaceGenerics(typeMap);
      if (replacement.isDef())
        v[i] = replacement;
    }
  }


  //--------------------------------------------------------------------------

  bool
  TypeImpl::isContravariant(const TypeImpl* other) const
  {
    return other->isCovariant(this);
  }


  bool
  TypeImpl::isInvariant(const TypeImpl* other) const
  {
  return !isCovariant(other) && !isContravariant(other);
  }


  //--------------------------------------------------------------------------

  class GroupTypeImpl : public TypeImpl
  {
  public:
    GroupTypeImpl(const TypeVector& types)
      : fTypes(types)
    { }


    virtual bool isEqual(const TypeImpl* other) const
    {
      const GroupTypeImpl* o = dynamic_cast<const GroupTypeImpl*>(other);

      return (o != NULL && typeid(this) == typeid(other) &&
              heather::isEqual(fTypes, o->fTypes));
    }


    virtual bool isCovariant(const TypeImpl* other) const
    {
      const GroupTypeImpl* o = dynamic_cast<const GroupTypeImpl*>(other);

      if (o != NULL && typeid(this) == typeid(other))
        return heather::isCovariant(fTypes, o->fTypes);
      return false;
    }


    const TypeVector& types() const
    {
      return fTypes;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      heather::replaceGenerics(fTypes, typeMap);
    }


  protected:
    TypeVector fTypes;
  };


  //--------------------------------------------------------------------------

  class UnionTypeImpl : public GroupTypeImpl
  {
  public:
    UnionTypeImpl(const TypeVector& types)
      : GroupTypeImpl(types)
    { }


    virtual UnionTypeImpl* clone() const
    {
      return new UnionTypeImpl(vectorClone(fTypes));
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:union>";
      for (size_t i = 0; i < fTypes.size(); i++)
        buf << fTypes[i].toString();
      buf << "</ty:union>";
      return buf.toString();
    }
  };


  //--------------------------------------------------------------------------

  class SeqTypeImpl : public GroupTypeImpl
  {
  public:
    SeqTypeImpl(const TypeVector& types)
      : GroupTypeImpl(types)
    { }

    virtual SeqTypeImpl* clone() const
    {
      return new SeqTypeImpl(vectorClone(fTypes));
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:seq>";
      for (size_t i = 0; i < fTypes.size(); i++)
        buf << fTypes[i].toString();
      buf << "</ty:seq>";
      return buf.toString();
    }
  };


  //--------------------------------------------------------------------------

  class FunctionTypeImpl : public TypeImpl
  {
  public:
    FunctionTypeImpl(const FunctionSignature& sign)
      : fSign(sign)
    { }


    virtual FunctionTypeImpl* clone() const
    {
      return new FunctionTypeImpl(fSign.clone());
    }


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


    const FunctionSignature& functionSignature() const
    {
      return fSign;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fSign.replaceGenerics(typeMap);
    }


    virtual String toString() const
    {
      // TODO
      return String();
    }

  private:
    FunctionSignature fSign;
  };


  //--------------------------------------------------------------------------

  class TypeTypeImpl : public TypeImpl
  {
  public:
    TypeTypeImpl(const String& name,
                 bool isInstantiatable,
                 const TypeVector& generics,
                 const Type& inherit,
                 const FunctionSignature& defApplySign,
                 const FunctionSignatureVector& protocol)
      : fName(name),
        fIsInstantiatable(isInstantiatable),
        fGenerics(generics),
        fInherit(inherit),
        fDefApplySign(defApplySign),
        fProtocol(protocol)
    { }


    virtual TypeTypeImpl* clone() const
    {
      return new TypeTypeImpl(fName, fIsInstantiatable,
                              vectorClone(fGenerics),
                              fInherit.clone(),
                              fDefApplySign.clone(),
                              vectorClone(fProtocol));
    }


    virtual bool
    isEqual(const TypeImpl* other) const
    {
      const TypeTypeImpl* o = dynamic_cast<const TypeTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              fIsInstantiatable == o->fIsInstantiatable &&
              fDefApplySign == o->fDefApplySign &&
              fInherit == o->fInherit &&
              heather::isEqual(fGenerics, o->fGenerics) &&
              heather::isEqual(fProtocol, o->fProtocol));
    }


    virtual bool
    isCovariant(const TypeImpl* other) const
    {
      const TypeTypeImpl* o = dynamic_cast<const TypeTypeImpl*>(other);

      if (o != NULL &&
          // fName == o->fName &&
          fGenerics.size() == o->fGenerics.size() &&
          fProtocol.size() >= o->fProtocol.size() &&
          fInherit.isCovariant(o->fInherit))
      {
        for (size_t i = 0; i < fGenerics.size(); i++) {
          if (!fGenerics[i].isCovariant(o->fGenerics[i]))
            return false;
        }

        for (size_t i = 0; i < fProtocol.size(); i++) {
          bool gotOne = false;
          for (size_t j = 0; j < o->fProtocol.size(); j++) {
            if (fProtocol[i].methodName() == o->fProtocol[j].methodName() &&
                fProtocol[i].isCovariant(o->fProtocol[j]))
            {
              gotOne = true;
              break;
            }
          }

          if (!gotOne)
            return false;
        }
        return true;
      }
      return false;
    }


    const String& name() const
    {
      return fName;
    }


    const Type& inherit() const
    {
      return fInherit;
    }


    const FunctionSignature& defaultApplySignature() const
    {
      return fDefApplySign;
    }


    const FunctionSignatureVector& protocol() const
    {
      return fProtocol;
    }


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      // TODO
    }


    virtual String toString() const
    {
      // TODO
      return String();
    }

  protected:
    String                  fName;
    bool                    fIsInstantiatable;
    TypeVector              fGenerics;
    Type                    fInherit;
    FunctionSignature       fDefApplySign;
    FunctionSignatureVector fProtocol;
  };


  //--------------------------------------------------------------------------

  class AliasTypeImpl : public TypeImpl
  {
  public:
    AliasTypeImpl(const String& name, const TypeVector& generics,
                  const Type& isa)
      : fName(name),
        fGenerics(generics),
        fType(isa)
    { }


    virtual AliasTypeImpl* clone() const
    {
      return new AliasTypeImpl(fName, vectorClone(fGenerics), fType.clone());
    }


    virtual bool isEqual(const TypeImpl* other) const
    {
      const AliasTypeImpl* o = dynamic_cast<const AliasTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              heather::isEqual(fGenerics, o->fGenerics) &&
              fType == o->fType);
    }


    virtual bool isCovariant(const TypeImpl* other) const
    {
      const AliasTypeImpl* o = dynamic_cast<const AliasTypeImpl*>(other);

      return (o != NULL &&
              fType.isCovariant(o->fType));
    }


    const String& name() const
    {
      return fName;
    }


    const Type& inherit() const
    {
      return fType;
    }


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      // TODO
    }


    virtual String toString() const
    {
      // TODO
      return String();
    }

  protected:
    String     fName;
    TypeVector fGenerics;
    Type       fType;
  };


  //--------------------------------------------------------------------------

  class TypeRefTypeImpl : public TypeImpl
  {
  public:
    TypeRefTypeImpl(const String& name,
                    const TypeVector& genericArgs,
                    const TypeConstVector& constraints)
      : fName(name),
        fGenerics(genericArgs),
        fConstraints(constraints)
    { }


    virtual TypeRefTypeImpl* clone() const
    {
      return new TypeRefTypeImpl(fName,
                                 vectorClone(fGenerics),
                                 vectorClone(fConstraints));
    }


    virtual bool isEqual(const TypeImpl* other) const
    {
      const TypeRefTypeImpl* o = dynamic_cast<const TypeRefTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              heather::isEqual(fGenerics, o->fGenerics) &&
              heather::isEqual(fConstraints, o->fConstraints));
    }


    virtual bool isCovariant(const TypeImpl* other) const
    {
      const TypeRefTypeImpl* o = dynamic_cast<const TypeRefTypeImpl*>(other);

      return (o != NULL &&
              heather::isCovariant(fGenerics, o->fGenerics) &&
              heather::isCovariant(fConstraints, o->fConstraints));
    }


    const String& name() const
    {
      return fName;
    }


    const TypeConstVector& constraints() const
    {
      return fConstraints;
    }


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      heather::replaceGenerics(fGenerics, typeMap);
      heather::replaceGenerics(fConstraints, typeMap);
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:ref nm='" << fName << "'>";
      if (!fGenerics.empty()) {
        buf << "<ty:gen>";
        for (size_t i = 0; i < fGenerics.size(); i++)
          buf << fGenerics[i].toString();
        buf << "</ty:gen>";
      }
      if (!fConstraints.empty()) {
        if (fConstraints.size() == 1)
          buf << fConstraints[0].toString();
        else {
          buf << "<ty:consts>";
          for (size_t i = 0; i < fConstraints.size(); i++)
            buf << fConstraints[i].toString();
          buf << "</ty:consts>";
        }
      }
      buf << "</ty:ref>";
      return buf.toString();
    }

  protected:
    String          fName;
    TypeVector      fGenerics;
    TypeConstVector fConstraints;
  };


  //--------------------------------------------------------------------------

  class ArrayTypeImpl : public TypeImpl
  {
  public:
    ArrayTypeImpl(const Type& base, int sizeIndicator)
      : fBase(base),
        fSizeIndicator(sizeIndicator)
    { }


    virtual ArrayTypeImpl* clone() const
    {
      return new ArrayTypeImpl(fBase.clone(), fSizeIndicator);
    }


    virtual bool
    isEqual(const TypeImpl* other) const
    {
      const ArrayTypeImpl* o = dynamic_cast<const ArrayTypeImpl*>(other);

      return (o != NULL &&
              fBase == o->fBase &&
              fSizeIndicator == o->fSizeIndicator);
    }


    virtual bool
    isCovariant(const TypeImpl* other) const
    {
      const ArrayTypeImpl* o = dynamic_cast<const ArrayTypeImpl*>(other);

      return (o != NULL &&
              fBase.isCovariant(o->fBase));
    }


    const Type&
    baseType() const
    {
      return fBase;
    }


    int
    sizeIndicator() const
    {
      return fSizeIndicator;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fBase = fBase.replaceGenerics(typeMap);
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:array ind='" << fromInt(fSizeIndicator) << "'>"
          << fBase.toString()
          << "</ty:array>";
      return buf.toString();
    }

  protected:
    Type fBase;
    int  fSizeIndicator;
  };
};                              // heather namespace


//----------------------------------------------------------------------------

Type::Type()
  : fKind(kType_Undefined)
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
Type::newTypeRef(const String& name, const TypeVector& genericArgs,
                 const TypeConstVector& constraints)
{
  return Type(kType_Ref, new TypeRefTypeImpl(name, genericArgs, constraints));
}


Type
Type::newTypeRef(const String& name)
{
  TypeVector dummyGenerics;
  TypeConstVector dummyConstraints;
  return Type(kType_Ref, 
              new TypeRefTypeImpl(name, dummyGenerics, dummyConstraints));
}


Type
Type::newArray(const Type& base, int sizeIndicator)
{
  return Type(kType_Array, new ArrayTypeImpl(base, sizeIndicator));
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
  FunctionSignatureVector dummyProtocol;
  FunctionSignature       dummyDefApplySign;
  return Type(kType_Type, new TypeTypeImpl(name, false, generics, inherit,
                                           dummyDefApplySign,
                                           dummyProtocol));
}


Type
Type::newType(const String& name, const TypeVector& generics,
              const Type& inherit,
              const FunctionSignatureVector& protocol)
{
  FunctionSignature dummyDefApplySign;
  return Type(kType_Type, new TypeTypeImpl(name, false, generics, inherit,
                                           dummyDefApplySign,
                                           protocol));
}


Type
Type::newClass(const String& name, const TypeVector& generics,
               const Type& inherit)
{
  FunctionSignatureVector dummyProtocol;
  FunctionSignature       dummyDefApplySign;
  return Type(kType_Class, new TypeTypeImpl(name, true, generics, inherit,
                                            dummyDefApplySign,
                                            dummyProtocol));
}


Type
Type::newClass(const String& name, const TypeVector& generics,
               const Type& inherit,
               const FunctionSignature& defApplySign,
               const FunctionSignatureVector& protocol)
{
  return Type(kType_Class, new TypeTypeImpl(name, true, generics, inherit,
                                            defApplySign,
                                            protocol));
}


Type
Type::newAlias(const String& name, const TypeVector& generics,
               const Type& isa)
{
  return Type(kType_Alias, new AliasTypeImpl(name, generics, isa));
}


Type
Type::newMeasure(const String& name, const Type& baseType,
                 const String& defUnit)
{
  // TODO
  return Type();
}


Type
Type::newEnum(const String& name, const Type& baseType,
              const StringTokenMap& defUnit)
{
  // TODO
  return Type();
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


Type
Type::clone() const
{
  return Type(fKind, (fImpl != NULL ? fImpl->clone() : NULL));
}


bool
Type::operator==(const Type& other) const
{
  if (isAny() || isBase())
    return fKind == other.fKind;

  assert(fImpl != NULL);
  return fImpl->isEqual(other.fImpl);
}


bool
Type::operator!=(const Type& other) const
{
  return !(operator==(other));
}


bool
Type::isCovariant(const Type& other) const
{
  if (isAny() || isBase()) {
    // TODO
    return true;
  }

  return fImpl->isCovariant(other.fImpl);
}


bool
Type::isContravariant(const Type& other) const
{
  return other.isCovariant(*this);
}


bool
Type::isInvariant(const Type& other) const
{
  return !isCovariant(other) && !isContravariant(other);
}


bool
Type::isDef() const
{
  return fKind != kType_Undefined;
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


bool
Type::isRef() const
{
  return fKind == kType_Ref;
}


String
Type::typeName() const
{
  switch (fKind) {
  case kType_Undefined:
    assert(0);

  case kType_Ref:
    return dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->name();

  case kType_Any:
    return String("Any");
  case kType_Bool:
    return String("Bool");
  case kType_Char:
    return String("Char");
  case kType_Int:
    return String("Int");
  case kType_Keyword:
    return String("Keyword");
  case kType_Long:
    return String("Long");
  case kType_Octet:
    return String("Octet");
  case kType_Rational:
    return String("Rational");
  case kType_Real:
    return String("Real");
  case kType_Short:
    return String("Short");
  case kType_String:
    return String("String");
  case kType_ULong:
    return String("ULong");
  case kType_UShort:
    return String("UShort");
  case kType_UWord:
    return String("UWord");
  case kType_Word:
    return String("Word");
  case kType_Eof:
    return String("Eof");
  case kType_Nil:
    return String("Nil");
  case kType_Unspecified:
    return String("Unspecified");

  case kType_Array:
    return arrayBaseType().typeName();
  case kType_Class:
  case kType_Type:
    return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->name();
  case kType_Alias:
    return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->name();

  case kType_Measure:
    // return dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->name();
  case kType_Enum:
    // return dynamic_cast<const EnumTypeImpl*>(fImpl.obj())->name();
  case kType_Union:
  case kType_Sequence:
  case kType_Function:
    return String();
  }

  return String();
}


bool
Type::isClass() const
{
  return fKind == kType_Class;
}


const Type&
Type::classInheritance() const
{
  assert(isClass());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->inherit();
}


const FunctionSignature&
Type::defaultApplySignature() const
{
  assert(isClass());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->defaultApplySignature();
}


const FunctionSignatureVector&
Type::classProtocol() const
{
  assert(isClass());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->protocol();
}


bool
Type::isType() const
{
  return fKind == kType_Type;
}


const Type&
Type::typeInheritance() const
{
  assert(isType());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->inherit();
}


const FunctionSignatureVector&
Type::typeProtocol() const
{
  assert(isType());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->protocol();
}


bool
Type::isAlias() const
{
  return fKind == kType_Alias;
}


const Type&
Type::aliasInheritance() const
{
  assert(isAlias());
  return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->inherit();
}


bool
Type::isFunction() const
{
  return fKind == kType_Function;
}


const FunctionSignature&
Type::functionSignature() const
{
  assert(isFunction());
  return dynamic_cast<const FunctionTypeImpl*>(fImpl.obj())->functionSignature();
}


bool
Type::isArray() const
{
  return fKind == kType_Array;
}


const Type&
Type::arrayBaseType() const
{
  assert(isArray());
  return dynamic_cast<const ArrayTypeImpl*>(fImpl.obj())->baseType();
}


Type
Type::arrayRootType() const
{
  Type ty = *this;
  while (ty.isArray())
    ty = ty.arrayBaseType();
  return ty;
}


Type
Type::rebase(const Type& newBaseType) const
{
  if (arrayBaseType().isArray())
    return Type::newArray(arrayBaseType().rebase(newBaseType),
                          arraySizeIndicator());

  return newBaseType;
}


int
Type::arraySizeIndicator() const
{
  assert(isArray());
  return dynamic_cast<const ArrayTypeImpl*>(fImpl.obj())->sizeIndicator();
}


bool
Type::isUnion() const
{
  return fKind == kType_Union;
}


const TypeVector&
Type::unionTypes() const
{
  assert(isUnion());
  return dynamic_cast<const UnionTypeImpl*>(fImpl.obj())->types();
}


bool
Type::isSequence() const
{
  return fKind == kType_Sequence;
}


const TypeVector&
Type::seqTypes() const
{
  assert(isUnion());
  return dynamic_cast<const SeqTypeImpl*>(fImpl.obj())->types();
}


bool
Type::isEnum() const
{
  return fKind == kType_Enum;
}


// const Type&
// Type::enumBaseType() const
// {
// }


// const TokenVector&
// Type::enumValues() const
// {
// }


bool
Type::isMeasure() const
{
  return fKind == kType_Measure;
}


// const Type&
// Type::measureBaseType() const
// {
// }


bool
Type::hasConstraints() const
{
  if (fKind == kType_Ref)
    return ( !dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())
             ->constraints().empty() );

  return false;
}


const TypeConstVector&
Type::constraints() const
{
  if (fKind == kType_Ref)
    return dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->constraints();

  static TypeConstVector dummy;
  return dummy;
}


bool
Type::hasGenerics() const
{
  return !generics().empty();
}


const TypeVector&
Type::generics() const
{
  static const TypeVector sEmptyTypeVector;
  switch (fKind) {
  case kType_Undefined:
  case kType_Any:
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
  case kType_Array:
  case kType_Function:
  case kType_Measure:
  case kType_Enum:
  case kType_Union:
  case kType_Sequence:
    return sEmptyTypeVector;

  case kType_Ref:
    return dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->generics();
  case kType_Class:
  case kType_Type:
    return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->generics();
  case kType_Alias:
    return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->generics();
  }

  return sEmptyTypeVector;
}


Type
Type::replaceGenerics(const TypeCtx& typeMap) const
{
  Type clonedTy;
  switch (fKind) {
  case kType_Ref:
    {
      Type replacement = typeMap.lookupType(typeName());
      if (replacement.isDef()) {
        if (replacement.hasConstraints()) {
          if (!constraints().empty())
            throw TypeConstraintsConflictException(
              *this,
              String("type parameter constraints conflict "
                     "with generics constraints"));
          clonedTy = replacement;
        }
        else if (hasConstraints()) {
          if (!replacement.isRef())
            throw TypeConstraintsConflictException(
              *this,
              String("Constraints for non trivial type reference"));
          clonedTy = Type::newTypeRef(replacement.typeName(),
                                     replacement.generics(),
                                     constraints());
        }
        else
          clonedTy = replacement;
      }
      else
        clonedTy = clone();

      clonedTy.fImpl->replaceGenerics(typeMap);
      return clonedTy;
    }

  case kType_Alias:
  case kType_Class:
  case kType_Type:
  case kType_Array:
  case kType_Union:
  case kType_Sequence:
    clonedTy = clone();
    clonedTy.fImpl->replaceGenerics(typeMap);
    return clonedTy;

  default:
    ;
  }
  return *this;
}


String
Type::toString() const
{
  switch (fKind) {
  case kType_Any:           return String("Any");
  case kType_Bool:          return String("Bool");
  case kType_Char:          return String("Char");
  case kType_Int:           return String("Int");
  case kType_Keyword:       return String("Keyword");
  case kType_Long:          return String("Long");
  case kType_Octet:         return String("Octet");
  case kType_Rational:      return String("Rational");
  case kType_Real:          return String("Real");
  case kType_Short:         return String("Short");
  case kType_String:        return String("String");
  case kType_ULong:         return String("ULong");
  case kType_UShort:        return String("UShort");
  case kType_UWord:         return String("UWord");
  case kType_Word:          return String("Word");
  case kType_Eof:           return String("Eof");
  case kType_Nil:           return String("Nil");
  case kType_Unspecified:   return String("Unspecified");

  case kType_Ref:
  case kType_Array:
  case kType_Function:
  case kType_Measure:
  case kType_Enum:
  case kType_Class:
  case kType_Type:
  case kType_Alias:
  case kType_Union:
  case kType_Sequence:
    return fImpl->toString();

  case kType_Undefined:
  default:
    ;
  }

  return String("--default--");
}


//----------------------------------------------------------------------------

namespace heather
{
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


    virtual BaseTypeConstraintImpl* clone() const
    {
      return new LogicalConstraintImpl(fOp, fLeft.clone(), fRight.clone());
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fLeft = fLeft.replaceGenerics(typeMap);
      fRight = fRight.replaceGenerics(typeMap);
    }


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


    const TypeConstraint& left() const
    {
      return fLeft;
    }


    const TypeConstraint& right() const
    {
      return fRight;
    }


    const char* optostr(TypeConstOperator op) const
    {
      switch (op) {
      case kConstOp_and:           return "and";
      case kConstOp_or:            return "or";
      default:
        assert(0);
      }
      return "??";
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:const k='" << optostr(fOp) << "'>"
          << fLeft.toString() << fRight.toString()
          << "</ty:const>";
      return buf.toString();
    }

  private:
    TypeConstOperator fOp;
    TypeConstraint    fLeft;
    TypeConstraint    fRight;
  };


  //--------------------------------------------------------------------------

  class ValueConstraintImpl : public BaseTypeConstraintImpl
  {
  public:
    ValueConstraintImpl(TypeConstOperator op, const Token& value)
      : fOp(op),
        fValue(value)
    { }


    virtual BaseTypeConstraintImpl* clone() const
    {
      return const_cast<ValueConstraintImpl*>(this);
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      // NOP
    }


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


    const char* optostr(TypeConstOperator op) const
    {
      switch (op) {
      case kConstOp_and:           assert(0);
      case kConstOp_or:            assert(0);
      case kConstOp_equal:         return "eq";
      case kConstOp_notEqual:      return "neq";
      case kConstOp_less:          return "lt";
      case kConstOp_lessEqual:     return "leq";
      case kConstOp_greater:       return "gt";
      case kConstOp_greaterEqual:  return "geq";
      case kConstOp_in:            return "in";
      case kConstOp_isa:           assert(0);
      }
      return "??";
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:const k='" << optostr(fOp) << "'>"
          << fValue.toString()
          << "</ty:const>";
      return buf.toString();
    }

  private:
    TypeConstOperator fOp;
    Token fValue;
  };


  //--------------------------------------------------------------------------

  class TypeConstraintImpl : public BaseTypeConstraintImpl
  {
  public:
    TypeConstraintImpl(TypeConstOperator op, const Type& type)
      : fOp(op),
        fType(type)
    { }


    virtual BaseTypeConstraintImpl* clone() const
    {
      return new TypeConstraintImpl(fOp, fType.clone());
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fType = fType.replaceGenerics(typeMap);
    }


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


    const Type& type() const
    {
      return fType;
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:const k='isa'>"
          << fType.toString()
          << "</ty:const>";
      return buf.toString();
    }

  private:
    TypeConstOperator fOp;
    Type fType;
  };

};                              // namespace heather


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


TypeConstraint
TypeConstraint::clone() const
{
  return TypeConstraint(fImpl->clone());
}


TypeConstraint
TypeConstraint::replaceGenerics(const TypeCtx& typeMap)
{
  fImpl->replaceGenerics(typeMap);
  return *this;
}


bool
TypeConstraint::isCovariant(const TypeConstraint& other) const
{
  // TODO
  if (constOp() == other.constOp()) {
    switch (constOp()) {
    case kConstOp_and:
    case kConstOp_or:
      // TODO
      return true;

    case kConstOp_equal:
    case kConstOp_notEqual:
    case kConstOp_less:
    case kConstOp_lessEqual:
    case kConstOp_greater:
    case kConstOp_greaterEqual:
    case kConstOp_in:
      // TODO
      return true;

    case kConstOp_isa:
      return ( dynamic_cast<const TypeConstraintImpl*>(fImpl.obj())->type()
               .isCovariant(dynamic_cast<const TypeConstraintImpl*>(
                              other.fImpl.obj())->type()));
    }
  }
  return true;
}


bool
TypeConstraint::isContravariant(const TypeConstraint& other) const
{
  return other.isCovariant(*this);
}


bool
TypeConstraint::isInvariant(const TypeConstraint& other) const
{
  return !isCovariant(other) && !isContravariant(other);
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


String
TypeConstraint::toString() const
{
  return fImpl->toString();
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


FunctionParameter
FunctionParameter::clone() const
{
  return FunctionParameter(fKind, fIsSpecialized,
                           fKey, fType.clone());
}


FunctionParameter
FunctionParameter::replaceGenerics(const TypeCtx& typeMap)
{
  fType = fType.replaceGenerics(typeMap);
  return *this;
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
FunctionParameter::isContravariant(const FunctionParameter& other) const
{
  return other.isCovariant(*this);
}


bool
FunctionParameter::isInvariant(const FunctionParameter& other) const
{
  return !isCovariant(other) && !isContravariant(other);
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

FunctionSignature::FunctionSignature()
  : fIsGeneric(false)
{ }


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


FunctionSignature
FunctionSignature::clone() const
{
  return FunctionSignature(fIsGeneric, fName,
                           fReturnType.clone(),
                           heather::vectorClone(fParameters));
}


bool
FunctionSignature::isCovariant(const FunctionSignature& other) const
{
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
FunctionSignature::isContravariant(const FunctionSignature& other) const
{
  return other.isCovariant(*this);
}


bool
FunctionSignature::isInvariant(const FunctionSignature& other) const
{
  return !isCovariant(other) && !isContravariant(other);
}


FunctionSignature
FunctionSignature::replaceGenerics(const TypeCtx& typeMap)
{
  fReturnType = fReturnType.replaceGenerics(typeMap);
  heather::replaceGenerics(fParameters, typeMap);
  return *this;
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

