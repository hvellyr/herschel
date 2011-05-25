/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include <typeinfo>

#include "errcodes.h"
#include "log.h"
#include "predefined.h"
#include "scope.h"
#include "strbuf.h"
#include "type.h"
#include "typectx.h"
#include "typeenum.h"
#include "rootscope.h"


using namespace herschel;


//----------------------------------------------------------------------------

namespace herschel
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


  template<typename T>
  bool isOpen(const std::vector<T>& v)
  {
    for (size_t i = 0; i < v.size(); i++) {
      if (v[i].isOpen())
        return true;
    }
    return false;
  }


  StringBuffer& operator<<(StringBuffer& other, const TypeVector& tyve)
  {
    for (size_t i = 0; i < tyve.size(); i++) {
      if (i > 0)
        other << ", ";
      other << tyve[i].typeId();
    }
    return other;
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
              herschel::isEqual(fTypes, o->fTypes));
    }


    bool isOpen() const
    {
      for (size_t i = 0; i < fTypes.size(); i++) {
        if (fTypes[i].isOpen())
          return true;
      }
      return false;
    }


    bool isOpenSelf() const
    {
      return false;
    }


    const TypeVector& types() const
    {
      return fTypes;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      herschel::replaceGenerics(fTypes, typeMap);
    }


    virtual bool containsType(const Type& type) const
    {
      if (type.isDef()) {
        for (size_t i = 0; i < fTypes.size(); i++) {
          if (fTypes[i].typeName() == type.typeName())
            return true;
        }
      }
      return false;
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


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:union" << ( !isValue ? " ref='t'" : "") << ">\n";
      for (size_t i = 0; i < fTypes.size(); i++)
        buf << fTypes[i].toString();
      buf << "</ty:union>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      if (right0.isUnion() && types().size() == right0.unionTypes().size()) {
        const TypeVector& ltypes = types();
        const TypeVector& rtypes = right0.unionTypes();

        for (size_t i = 0; i < ltypes.size(); ++i) {
          if (!ltypes[i].matchGenerics(localCtx, rtypes[i], scope, srcpos))
            return false;
        }
        return true;
      }
      return false;
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


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:seq" << ( !isValue ? " ref='t'" : "") << ">\n";
      for (size_t i = 0; i < fTypes.size(); i++)
        buf << fTypes[i].toString();
      buf << "</ty:seq>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      if (right0.isSequence() && types().size() == right0.seqTypes().size()) {
        const TypeVector& ltypes = types();
        const TypeVector& rtypes = right0.seqTypes();

        for (size_t i = 0; i < ltypes.size(); ++i) {
          if (!ltypes[i].matchGenerics(localCtx, rtypes[i], scope, srcpos))
            return false;
        }
        return true;
      }
      return false;
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


    bool isOpen() const
    {
      return fSign.isOpen();
    }


    bool isOpenSelf() const
    {
      return false;
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      if (right0.isFunction())
        return fSign.matchGenerics(localCtx, right0.functionSignature(),
                                   scope, srcpos);
      return false;
    }


    const FunctionSignature& functionSignature() const
    {
      return fSign;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fSign.replaceGenerics(typeMap);
    }


    virtual String toString(bool isValue) const
    {
      return fSign.toString();
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
                 const FunctionSignature& applySign)
      : fName(name),
        fIsInstantiatable(isInstantiatable),
        fGenerics(generics),
        fInherit(inherit),
        fApplySign(applySign)
    { }


    virtual TypeTypeImpl* clone() const
    {
      return new TypeTypeImpl(fName, fIsInstantiatable,
                              vectorClone(fGenerics),
                              fInherit.clone(),
                              fApplySign.clone());
    }


    virtual bool
    isEqual(const TypeImpl* other) const
    {
      const TypeTypeImpl* o = dynamic_cast<const TypeTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              fIsInstantiatable == o->fIsInstantiatable &&
              fInherit == o->fInherit &&
              herschel::isEqual(fGenerics, o->fGenerics));
    }


    bool isOpen() const
    {
      return ( fInherit.isOpen() || herschel::isOpen(fGenerics));
    }


    bool isOpenSelf() const
    {
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


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      for (size_t i = 0; i < fGenerics.size(); i++) {
        hr_assert(fGenerics[i].isRef());

        // fprintf(stderr, "REPLACE GENERIC: %s\n", (const char*)StrHelper(fGenerics[i].toString()));

        Type replacement = typeMap.lookupType(fGenerics[i].typeName());
        if (replacement.isDef()) {
          // fprintf(stderr, "FOUND STH: %s\n", (const char*)StrHelper(replacement.toString()));
          fGenerics[i] = replacement;
        }
      }
      fInherit = fInherit.replaceGenerics(typeMap);
    }


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:type nm='" << fName << "'"
          << (fIsInstantiatable ? " inst='t'" : "") << ">\n";
      if (fInherit.isDef())
        buf << "<ty:isa>\n" << fInherit.toString() << "</ty:isa>\n";

      if (!fGenerics.empty()) {
        buf << "<ty:gen>\n";
        for (size_t i = 0; i < fGenerics.size(); i++)
          buf << fGenerics[i].toString();
        buf << "</ty:gen>\n";
      }

      buf << "</ty:type>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      // fprintf(stderr, "RIGHT in class: %s\n", (const char*)StrHelper(right0.toString()));
      if (right0.isType() || right0.isClass()) {
        if (fName == right0.typeName() &&
            fGenerics.size() == right0.generics().size())
        {
          for (size_t i = 0; i < fGenerics.size(); ++i) {
            // fprintf(stderr, "CHECK GENERIC: %s\n", (const char*)StrHelper(fGenerics[i].toString()));
            if (!fGenerics[i].matchGenerics(localCtx, right0.generics()[i],
                                            scope, srcpos))
              return false;
          }
          return true;
        }
      }
      return false;
    }


    const FunctionSignature& applySignature() const
    {
      return fApplySign;
    }


  protected:
    String                  fName;
    bool                    fIsInstantiatable;
    TypeVector              fGenerics;
    Type                    fInherit;
    FunctionSignature       fApplySign;
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
              herschel::isEqual(fGenerics, o->fGenerics) &&
              fType == o->fType);
    }


    bool isOpen() const
    {
      return fType.isOpen() || herschel::isOpen(fGenerics);
    }


    bool isOpenSelf() const
    {
      return false;
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      hr_invalid("when does this happen?");
      return false;
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
      hr_invalid("");
    }


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:alias nm='" << fName << "'>\n";

      if (!fGenerics.empty()) {
        buf << "<ty:gen>\n";
        for (size_t i = 0; i < fGenerics.size(); i++)
          buf << fGenerics[i].toString();
        buf << "</ty:gen>\n";
      }

      if (fType.isDef())
        buf << "<ty:isa>\n" << fType.toString() << "</ty:isa>\n";

      buf << "</ty:alias>\n";
      return buf.toString();
    }

  protected:
    String     fName;
    TypeVector fGenerics;
    Type       fType;
  };


  //--------------------------------------------------------------------------

  class MeasureTypeImpl : public TypeImpl
  {
  public:
    MeasureTypeImpl(const String& name, const Type& baseType,
                    const String& defUnit)
      : fName(name),
        fBaseType(baseType),
        fDefUnit(defUnit)
    { }


    virtual MeasureTypeImpl* clone() const
    {
      return new MeasureTypeImpl(fName, fBaseType.clone(), fDefUnit);
    }


    virtual bool isEqual(const TypeImpl* other) const
    {
      const MeasureTypeImpl* o = dynamic_cast<const MeasureTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              fDefUnit == o->fDefUnit &&
              fBaseType == o->fBaseType);
    }


    bool isOpen() const
    {
      return fBaseType.isOpen();
    }


    bool isOpenSelf() const
    {
      return fBaseType.isOpenSelf();
    }


    const String& name() const
    {
      return fName;
    }


    const Type& inherit() const
    {
      return fBaseType;
    }


    const String& defUnit() const
    {
      return fDefUnit;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      fBaseType.replaceGenerics(typeMap);
    }


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:measure nm='" << fName << "' unit='"
          << fDefUnit << "'>\n";

      if (fBaseType.isDef())
        buf << "<ty:isa>\n" << fBaseType.toString() << "</ty:isa>\n";

      buf << "</ty:measure>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      if (right0.isMeasure())
        return fBaseType.matchGenerics(localCtx, right0.measureBaseType(),
                                       scope, srcpos);
      return false;
    }


  protected:
    String     fName;
    Type       fBaseType;
    String     fDefUnit;
  };


  //--------------------------------------------------------------------------

  class TypeRefTypeImpl : public TypeImpl
  {
  public:
    TypeRefTypeImpl(const String& name,
                    bool isOpen,
                    const TypeVector& genericArgs,
                    const TypeConstVector& constraints)
      : fName(name),
        fGenerics(genericArgs),
        fConstraints(constraints),
        fIsOpen(isOpen)
    { }


    virtual TypeRefTypeImpl* clone() const
    {
      return new TypeRefTypeImpl(fName,
                                 fIsOpen,
                                 vectorClone(fGenerics),
                                 vectorClone(fConstraints));
    }


    virtual bool isEqual(const TypeImpl* other) const
    {
      const TypeRefTypeImpl* o = dynamic_cast<const TypeRefTypeImpl*>(other);

      return (o != NULL &&
              fName == o->fName &&
              fIsOpen == o->fIsOpen &&
              herschel::isEqual(fGenerics, o->fGenerics) &&
              herschel::isEqual(fConstraints, o->fConstraints));
    }


    const String& name() const
    {
      return fName;
    }


    const TypeConstVector& constraints() const
    {
      return fConstraints;
    }


    bool isOpen() const
    {
      return fIsOpen || herschel::isOpen(fGenerics);
    }


    bool isOpenSelf() const
    {
      return fIsOpen;
    }


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    virtual void replaceGenerics(const TypeCtx& typeMap)
    {
      herschel::replaceGenerics(fGenerics, typeMap);
      herschel::replaceGenerics(fConstraints, typeMap);
    }


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:ref" << (fIsOpen ? " gen='t'" : "")
          << ( !isValue ? " ref='t'" : "")
          << " nm='" << fName << "'>\n";
      if (!fGenerics.empty()) {
        buf << "<ty:gen>\n";
        for (size_t i = 0; i < fGenerics.size(); i++)
          buf << fGenerics[i].toString();
        buf << "</ty:gen>\n";
      }
      if (!fConstraints.empty()) {
        if (fConstraints.size() == 1)
          buf << fConstraints[0].toString();
        else {
          buf << "<ty:consts>\n";
          for (size_t i = 0; i < fConstraints.size(); i++)
            buf << fConstraints[i].toString();
          buf << "</ty:consts>\n";
        }
      }
      buf << "</ty:ref>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      // fprintf(stderr, "LEFT in typeref:  %s\n", (const char*)StrHelper(toString(true)));
      // fprintf(stderr, "RIGHT in typeref: %s\n", (const char*)StrHelper(right0.toString()));
      if (right0.isRef() || right0.isType() || right0.isClass()) {
        // if the reference has generics, it itself cannot be generic.  A
        // 'T<'Y'> is not allowed.
        if (!fGenerics.empty()) {
          if (fGenerics.size() == right0.generics().size() &&
              fName == right0.typeName())
          {
            for (size_t i = 0; i < fGenerics.size(); ++i) {
              if (!fGenerics[i].matchGenerics(localCtx, right0.generics()[i],
                                              scope, srcpos)) {
                // fprintf(stderr, "<1>LEFT in typeref:  %s\n", (const char*)StrHelper(toString(true)));
                // fprintf(stderr, "<1>RIGHT in typeref: %s\n", (const char*)StrHelper(right0.toString()));
                return false;
              }
            }
            return true;
          }
          // fprintf(stderr, "<2>LEFT in typeref:  %s\n", (const char*)StrHelper(toString(true)));
          // fprintf(stderr, "<2>RIGHT in typeref: %s\n", (const char*)StrHelper(right0.toString()));
          return false;
        }

        if (localCtx.hasType(name())) {
          if (!isSameType(localCtx.lookupType(name()), right0, scope, srcpos))
          {
            errorf(srcpos, E_TypeMismatch, "type mismatch for generic parameter");
            return false;
          }
          return true;
        }
        else {
          // fprintf(stderr, "MAP %s to %s\n", (const char*)StrHelper(name()),
          //         (const char*)StrHelper(right0.toString()));
          localCtx.registerType(name(), right0);
          return true;
        }
      }
      else if (right0.isArray()) {
        // special case: Make lang/sliceable<K, E> match arrays, which are
        // otherwise not first class entities.
        if (name() == Names::kSliceableTypeName || name() == Names::kSliceableXTypeName) {
          if (fGenerics.size() == 2) {
            localCtx.registerType(fGenerics[0].typeName(), Type::newInt32());
            localCtx.registerType(fGenerics[1].typeName(), right0.arrayBaseType());

            return true;
          }
        }
      }

      // fprintf(stderr, "<3>LEFT in typeref:  %s\n", (const char*)StrHelper(toString(true)));
      // fprintf(stderr, "<3>RIGHT in typeref: %s\n", (const char*)StrHelper(right0.toString()));
      return false;
    }


  protected:
    String          fName;
    TypeVector      fGenerics;
    TypeConstVector fConstraints;
    bool            fIsOpen;
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


    bool isOpen() const
    {
      return fBase.isOpen();
    }


    bool isOpenSelf() const
    {
      return fBase.isOpenSelf();
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


    virtual String toString(bool isValue) const
    {
      StringBuffer buf;
      buf << "<ty:array ind='" << fromInt(fSizeIndicator) << "'"
          << ( !isValue ? " ref='t'" : "") << ">\n"
          << fBase.toString()
          << "</ty:array>\n";
      return buf.toString();
    }


    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const
    {
      // fprintf(stderr, "LEFT IS:  %s\n", (const char*)StrHelper(toString(true)));
      // fprintf(stderr, "RIGHT IS: %s\n", (const char*)StrHelper(right0.toString()));

      if (right0.isArray())
        return fBase.matchGenerics(localCtx, right0.arrayBaseType(),
                                   scope, srcpos);
      return false;
    }


  protected:
    Type fBase;
    int  fSizeIndicator;
  };
};                              // herschel namespace


//----------------------------------------------------------------------------

Type::Type()
  : fKind(kType_Undefined),
    fIsValue(true),
    fIsImaginary(false)
{ }


Type::Type(const Type& other)
{
  *this = other;
}


Type::Type(TypeKind kind, bool isValue, bool isImaginary, TypeImpl* impl)
  : fKind(kind),
    fIsValue(isValue),
    fIsImaginary(isImaginary),
    fImpl(impl)
{ }


Type&
Type::operator=(const Type& other)
{
  fKind = other.fKind;
  fIsValue = other.fIsValue;
  fIsImaginary = other.fIsImaginary;
  fImpl = other.fImpl;
  return *this;
}


Type
Type::newTypeRef(const String& name, const TypeVector& genericArgs,
                 const TypeConstVector& constraints, bool isValue)
{
  return Type(kType_Ref, isValue, !K(isImg),
              new TypeRefTypeImpl(name, !K(isOpen), genericArgs,
                                  constraints));
}


Type
Type::newTypeRef(const String& name, const TypeVector& genericArgs,
                 bool isValue)
{
  return Type(kType_Ref, isValue, !K(isImg),
              new TypeRefTypeImpl(name, !K(isOpen), genericArgs,
                                  TypeConstVector()));
}


Type
Type::newTypeRef(const String& name, bool isValue)
{
  TypeVector dummyGenerics;
  TypeConstVector dummyConstraints;
  return Type(kType_Ref, isValue, !K(isImg),
              new TypeRefTypeImpl(name, !K(isOpen),
                                  dummyGenerics, dummyConstraints));
}


Type
Type::newTypeRef(const char* name, bool isValue)
{
  return newTypeRef(String(name), isValue);
}


Type
Type::newTypeRef(const String& name, bool isOpen,
                 const TypeConstVector& constraints, bool isValue)
{
  TypeVector dummyGenerics;
  return Type(kType_Ref, isValue, !K(isImg),
              new TypeRefTypeImpl(name, isOpen, dummyGenerics,
                                  constraints));
}


Type
Type::newTypeRef(const String& name, bool isOpen, bool isValue)
{
  TypeVector dummyGenerics;
  TypeConstVector dummyConstraints;
  return Type(kType_Ref, isValue, !K(isImg),
              new TypeRefTypeImpl(name, isOpen, dummyGenerics,
                                  dummyConstraints));
}


Type
Type::newTypeRef(const String& name, const Type& old)
{
  hr_assert(old.isRef());

  return Type(kType_Ref, old.isValueType(), old.isImaginary(),
              new TypeRefTypeImpl(name,
                                  dynamic_cast<const TypeRefTypeImpl*>(old.fImpl.obj())->isOpenSelf(),
                                  old.generics(),
                                  old.constraints()));
}


Type
Type::newClassOf(const Type& type, bool isValue)
{
  return newTypeRef(Names::kClassTypeName, newTypeVector(type), isValue);
}


Type
Type::newArray(const Type& base, int sizeIndicator, bool isValue)
{
  return Type(kType_Array, isValue, !K(isImg),
              new ArrayTypeImpl(base, sizeIndicator));
}


Type
Type::newAny(bool isValue)
{
  return newTypeRef(Names::kAnyTypeName, isValue);
}


Type
Type::newInt32(bool isValue)
{
  return newTypeRef(Names::kInt32TypeName, isValue);
}


Type
Type::newUInt32(bool isValue)
{
  return newTypeRef(Names::kUInt32TypeName, isValue);
}


Type
Type::newRational(bool isValue)
{
  return newTypeRef(Names::kRationalTypeName, isValue);
}


Type
Type::newFloat32(bool isValue)
{
  return newTypeRef(Names::kFloat32TypeName, isValue);
}


Type
Type::newString(bool isValue)
{
  return newTypeRef(Names::kStringTypeName, isValue);
}


Type
Type::newBool(bool isValue)
{
  return newTypeRef(Names::kBoolTypeName, isValue);
}


Type
Type::newType(const String& name, const TypeVector& generics,
              const Type& inherit)
{
  return Type(kType_Type, K(isValue), !K(isImg),
              new TypeTypeImpl(name, !K(isInstantiable), generics, inherit,
                               FunctionSignature()));
}


Type
Type::newClass(const String& name, const TypeVector& generics,
               const Type& inherit, const FunctionSignature& applySign)
{
  return Type(kType_Class, K(isValue), !K(isImg),
              new TypeTypeImpl(name, K(isInstantiable),
                               generics, inherit, applySign));
}


Type
Type::newAlias(const String& name, const TypeVector& generics,
               const Type& isa)
{
  return Type(kType_Alias, K(isValue), !K(isImg),
              new AliasTypeImpl(name, generics, isa));
}


Type
Type::newMeasure(const String& name, const Type& baseType,
                 const String& defUnit)
{
  return Type(kType_Measure, K(isValue), !K(isImg),
              new MeasureTypeImpl(name, baseType, defUnit));
}


Type
Type::newFunction(const FunctionSignature& sign)
{
  return Type(kType_Function, K(isValue), !K(isImg),
              new FunctionTypeImpl(sign));
}


Type
Type::newUnion(const TypeVector& types, bool isValue)
{
  return Type(kType_Union, isValue, !K(isImg), new UnionTypeImpl(types));
}


Type
Type::newSeq(const TypeVector& types, bool isValue)
{
  return Type(kType_Sequence, isValue, !K(isImg), new SeqTypeImpl(types));
}


Type
Type::clone() const
{
  return Type(fKind, fIsValue, fIsImaginary,
              (fImpl != NULL ? fImpl->clone() : NULL));
}


TypeKind
Type::kind() const
{
  return fKind;
}


bool
Type::operator==(const Type& other) const
{
  if (fIsValue != other.fIsValue)
    return false;

  hr_assert(fImpl != NULL);
  return fImpl->isEqual(other.fImpl);
}


bool
Type::operator!=(const Type& other) const
{
  return !(operator==(other));
}


bool
Type::isDef() const
{
  return fKind != kType_Undefined;
}


bool
Type::isBaseType() const
{
  String nm;
  if (isRef() || isType() || isClass()) {
    nm = typeName();
  }

  if (!nm.isEmpty()) {
    return (nm == Names::kBoolTypeName ||
            nm == Names::kCharTypeName ||
            nm == Names::kEofTypeName ||
            nm == Names::kFloat32TypeName ||
            nm == Names::kFloat64TypeName ||
            nm == Names::kFloat128TypeName ||
            nm == Names::kKeywordTypeName ||
            nm == Names::kNilTypeName ||
            nm == Names::kRationalTypeName ||
            nm == Names::kStringTypeName ||
            nm == Names::kInt8TypeName ||
            nm == Names::kUInt8TypeName ||
            nm == Names::kInt16TypeName ||
            nm == Names::kUInt16TypeName ||
            nm == Names::kInt32TypeName ||
            nm == Names::kUInt32TypeName ||
            nm == Names::kInt64TypeName ||
            nm == Names::kUInt64TypeName);
  }

  return false;
}


bool
Type::isPlainType() const
{
  String nm;
  if (isRef() || isType() || isClass()) {
    nm = typeName();
  }

  if (!nm.isEmpty()) {
    return (nm == Names::kBoolTypeName ||
            nm == Names::kCharTypeName ||
            nm == Names::kFloat32TypeName ||
            nm == Names::kFloat64TypeName ||
            nm == Names::kFloat128TypeName ||
            nm == Names::kInt8TypeName ||
            nm == Names::kUInt8TypeName ||
            nm == Names::kInt16TypeName ||
            nm == Names::kUInt16TypeName ||
            nm == Names::kInt32TypeName ||
            nm == Names::kUInt32TypeName ||
            nm == Names::kInt64TypeName ||
            nm == Names::kUInt64TypeName ||
            nm == String("clang|int"));
  }

  return false;
}


bool
Type::isBuiltinType(const String& name) const
{
  return typeName() == name;
}


TypeEnumMaker*
Type::newBaseTypeEnumMaker() const
{
  if (fKind == kType_Ref) {
    String nm = typeName();
    if      (nm == Names::kBoolTypeName)     return new BoolTypeEnumMaker;
    else if (nm == Names::kCharTypeName)     return new CharTypeEnumMaker;
    else if (nm == Names::kFloat32TypeName)  return new Float32TypeEnumMaker;
    else if (nm == Names::kFloat64TypeName)  return new Float64TypeEnumMaker;
    else if (nm == Names::kFloat128TypeName) return new Float128TypeEnumMaker;
    else if (nm == Names::kEofTypeName)      return new EofTypeEnumMaker;
    else if (nm == Names::kKeywordTypeName)  return new KeywordTypeEnumMaker;
    else if (nm == Names::kNilTypeName)      return new NilTypeEnumMaker;
    else if (nm == Names::kRationalTypeName) return new RationalTypeEnumMaker;
    else if (nm == Names::kStringTypeName)   return new StringTypeEnumMaker;

    else if (nm == Names::kInt8TypeName)     return new Int8TypeEnumMaker;
    else if (nm == Names::kInt16TypeName)    return new Int16TypeEnumMaker;
    else if (nm == Names::kInt32TypeName)    return new Int32TypeEnumMaker;
    else if (nm == Names::kInt64TypeName)    return new Int64TypeEnumMaker;

    else if (nm == Names::kUInt8TypeName)    return new UInt8TypeEnumMaker;
    else if (nm == Names::kUInt16TypeName)   return new UInt16TypeEnumMaker;
    else if (nm == Names::kUInt32TypeName)   return new UInt32TypeEnumMaker;
    else if (nm == Names::kUInt64TypeName)   return new UInt64TypeEnumMaker;
  }

  return NULL;
}


bool
Type::isAny() const
{
  return isBuiltinType(Names::kAnyTypeName);
}


bool
Type::isSigned() const
{
  return ( isBuiltinType(Names::kNumberTypeName) ||
           isBuiltinType(Names::kComplexTypeName) ||
           isBuiltinType(Names::kRationalTypeName) ||
           isBuiltinType(Names::kInt8TypeName) ||
           isBuiltinType(Names::kInt16TypeName) ||
           isBuiltinType(Names::kInt32TypeName) ||
           isBuiltinType(Names::kInt64TypeName) ||
           isBuiltinType(Names::kFloat32TypeName) ||
           isBuiltinType(Names::kFloat64TypeName) ||
           isBuiltinType(Names::kFloat128TypeName) );
}


bool
Type::isAnyNumber() const
{
  return ( isBuiltinType(Names::kNumberTypeName) ||
           isBuiltinType(Names::kComplexTypeName) ||
           isBuiltinType(Names::kRationalTypeName) ||
           isBuiltinType(Names::kInt8TypeName) ||
           isBuiltinType(Names::kUInt8TypeName) ||
           isBuiltinType(Names::kInt16TypeName) ||
           isBuiltinType(Names::kUInt16TypeName) ||
           isBuiltinType(Names::kInt32TypeName) ||
           isBuiltinType(Names::kUInt32TypeName) ||
           isBuiltinType(Names::kInt64TypeName) ||
           isBuiltinType(Names::kUInt64TypeName) ||
           isBuiltinType(Names::kFloat32TypeName) ||
           isBuiltinType(Names::kFloat64TypeName) ||
           isBuiltinType(Names::kFloat128TypeName) );
}


bool
Type::isInt32() const
{
  return isBuiltinType(Names::kInt32TypeName);
}


bool
Type::isString() const
{
  return isBuiltinType(Names::kStringTypeName);
}


bool
Type::isKeyword() const
{
  return isBuiltinType(Names::kKeywordTypeName);
}


bool
Type::isFloat32() const
{
  return isBuiltinType(Names::kFloat32TypeName);
}


bool
Type::isNumber() const
{
  return isBuiltinType(Names::kNumberTypeName);
}


bool
Type::isComplex() const
{
  return ( isBuiltinType(Names::kComplexTypeName) || isImaginary() );
}


bool
Type::isRational() const
{
  return isBuiltinType(Names::kRationalTypeName);
}


bool
Type::isUInt32() const
{
  return isBuiltinType(Names::kUInt32TypeName);
}


bool
Type::isChar() const
{
  return isBuiltinType(Names::kCharTypeName);
}


bool
Type::isBool() const
{
  return isBuiltinType(Names::kBoolTypeName);
}


bool
Type::isAnyFloat() const
{
  return ( isBuiltinType(Names::kFloat32TypeName) ||
           isBuiltinType(Names::kFloat64TypeName) ||
           isBuiltinType(Names::kFloat128TypeName) );
}


bool
Type::isAnyInt() const
{
  return ( isAnySignedInt() || isAnyUInt() );
}


bool
Type::isAnySignedInt() const
{
  return ( isBuiltinType(Names::kInt8TypeName) ||
           isBuiltinType(Names::kInt16TypeName) ||
           isBuiltinType(Names::kInt32TypeName) ||
           isBuiltinType(Names::kInt64TypeName) );
}


bool
Type::isAnyUInt() const
{
  return ( isBuiltinType(Names::kUInt8TypeName) ||
           isBuiltinType(Names::kUInt16TypeName) ||
           isBuiltinType(Names::kUInt32TypeName) ||
           isBuiltinType(Names::kUInt64TypeName) );
}


bool
Type::isClassOf() const
{
  return isBuiltinType(Names::kClassTypeName);
}


bool
Type::isImaginary() const
{
  if (isAnyNumber()) {
    return fIsImaginary;
  }
  return false;
}


void
Type::setIsImaginary(bool value)
{
  if (isAnyNumber())
    fIsImaginary = value;
  else
    fIsImaginary = false;
}


bool
Type::isRef() const
{
  return fKind == kType_Ref;
}


bool
Type::isValueType() const
{
  return fIsValue;
}


Type&
Type::setIsValueType(bool value)
{
  fIsValue = value;
  return *this;
}


String
Type::typeName() const
{
  switch (fKind) {
  case kType_Undefined:
    hr_invalid("");

  case kType_Ref:
    return dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->name();

  case kType_Array:
    return arrayBaseType().typeName();
  case kType_Class:
  case kType_Type:
    return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->name();
  case kType_Alias:
    return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->name();

  case kType_Measure:
    return dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->name();
  case kType_Union:
  case kType_Sequence:
  case kType_Function:
    return String();
  }

  return String();
}


String
Type::typeId() const
{
  StringBuffer buffer;

  switch (fKind) {
  case kType_Undefined:
    hr_invalid("");
    break;

  case kType_Ref:
    {
      const TypeRefTypeImpl* tyimpl = dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj());
      if (tyimpl->isOpenSelf())
        buffer << "'";
      if (fIsImaginary)
        buffer << "i<";

      buffer << tyimpl->name();
      if (!tyimpl->generics().empty())
        buffer << "<" << tyimpl->generics() << ">";

      if (fIsImaginary)
        buffer << ">";

      return buffer.toString();
    }

  case kType_Array:
    buffer << arrayBaseType().typeId() << "[]";
    return buffer.toString();

  case kType_Class:
  case kType_Type:
    {
      const TypeTypeImpl* tyimpl = dynamic_cast<const TypeTypeImpl*>(fImpl.obj());
      if (fIsImaginary)
        buffer << "i<";
      buffer << tyimpl->name();
      if (!tyimpl->generics().empty())
        buffer << "<" << tyimpl->generics()  << ">";
      if (fIsImaginary)
        buffer << ">";
      return buffer.toString();
    }

  case kType_Alias:
    return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->name();

  case kType_Measure:
    buffer << dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->name()
           << dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->defUnit();
    return buffer.toString();

  case kType_Union:
    buffer << "&(" << dynamic_cast<const UnionTypeImpl*>(fImpl.obj())->types() << ")";
    return buffer.toString();

  case kType_Sequence:
    buffer << "(" << dynamic_cast<const SeqTypeImpl*>(fImpl.obj())->types() << ")";
    return buffer.toString();

  case kType_Function:
    return dynamic_cast<const FunctionTypeImpl*>(fImpl.obj())->functionSignature().typeId();
  }

  return String();
}


bool
Type::isClass() const
{
  return fKind == kType_Class;
}


bool
Type::isType() const
{
  return fKind == kType_Type;
}


const Type&
Type::typeInheritance() const
{
  hr_assert(isType() || isClass());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->inherit();
}


const FunctionSignature&
Type::applySignature() const
{
  hr_assert(isClass());
  return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->applySignature();
}


bool
Type::isAlias() const
{
  return fKind == kType_Alias;
}


const Type&
Type::aliasReplaces() const
{
  hr_assert(isAlias());
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
  hr_assert(isFunction());
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
  hr_assert(isArray());
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
                          arraySizeIndicator(),
                          arrayBaseType().isValueType());

  return newBaseType;
}


int
Type::arraySizeIndicator() const
{
  hr_assert(isArray());
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
  hr_assert(isUnion());
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
  hr_assert(isSequence());
  return dynamic_cast<const SeqTypeImpl*>(fImpl.obj())->types();
}


bool
Type::containsType(const Type& type) const
{
  if (isSequence())
    return dynamic_cast<const SeqTypeImpl*>(fImpl.obj())->containsType(type);
  else if (isUnion())
    return dynamic_cast<const UnionTypeImpl*>(fImpl.obj())->containsType(type);

  hr_invalid("no sequence or union type");
  return false;
}


bool
Type::isMeasure() const
{
  return fKind == kType_Measure;
}


const Type&
Type::measureBaseType() const
{
  hr_assert(isMeasure());
  return dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->inherit();
}


String
Type::measureUnit() const
{
  hr_assert(isMeasure());
  return dynamic_cast<const MeasureTypeImpl*>(fImpl.obj())->defUnit();
}


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


Type
Type::setConstraints(const TypeConstVector& newConstraints) const
{
  if (fKind == kType_Ref) {
    Type clonedTy = Type::newTypeRef(typeName(),
                                     generics(),
                                     newConstraints,
                                     isValueType());
    clonedTy.setIsImaginary(fIsImaginary);
    return clonedTy;
  }

  return *this;
}


bool
Type::isOpen() const
{
  return (fImpl != NULL && fImpl->isOpen());
}


bool
Type::isOpenSelf() const
{
  return fKind == kType_Ref && fImpl != NULL && fImpl->isOpenSelf();
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
  case kType_Ref:
    return dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->generics();
  case kType_Class:
  case kType_Type:
    return dynamic_cast<const TypeTypeImpl*>(fImpl.obj())->generics();
  case kType_Alias:
    return dynamic_cast<const AliasTypeImpl*>(fImpl.obj())->generics();

  case kType_Array:
  case kType_Function:
  case kType_Measure:
  case kType_Union:
  case kType_Sequence:
    return sEmptyTypeVector;
  }

  return sEmptyTypeVector;
}


Type
Type::replaceGenerics(const TypeCtx& typeMap) const
{
  Type clonedTy;
  switch (fKind) {
  case kType_Ref:
    if (dynamic_cast<const TypeRefTypeImpl*>(fImpl.obj())->isOpen()) {
      Type replacement = typeMap.lookupType(typeName());
      if (replacement.isDef()) {
        if (replacement.hasConstraints()) {
          if (!constraints().empty())
            throw TypeConstraintsConflictException(
              *this,
              String("type parameter constraints conflict "
                     "with generics constraints"));
          clonedTy = replacement;
          clonedTy.setIsImaginary(fIsImaginary);
        }
        else if (hasConstraints()) {
          if (!replacement.isRef())
            throw TypeConstraintsConflictException(
              *this,
              String("Constraints for non trivial type reference"));
          clonedTy = Type::newTypeRef(replacement.typeName(),
                                      replacement.generics(),
                                      constraints(),
                                      replacement.isValueType());
          clonedTy.setIsImaginary(fIsImaginary);
        }
        else {
          clonedTy = replacement;
          clonedTy.setIsImaginary(fIsImaginary);
        }
      }
      else
        clonedTy = clone();
    }
    else
      clonedTy = clone();

    clonedTy.fImpl->replaceGenerics(typeMap);
    return clonedTy;

  case kType_Alias:
  case kType_Class:
  case kType_Type:
  case kType_Array:
  case kType_Union:
  case kType_Sequence:
  case kType_Function:
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
  String retval;

  switch (fKind) {
  case kType_Ref:
  case kType_Array:
  case kType_Function:
  case kType_Measure:
  case kType_Class:
  case kType_Type:
  case kType_Alias:
  case kType_Union:
  case kType_Sequence:
    return fImpl->toString(fIsValue);

  case kType_Undefined:
  default:
    return String("--default--");
  }

  if (!fIsValue)
    return String("^") + retval;
  return retval;
}


bool
Type::matchGenerics(TypeCtx& localCtx, const Type& right0,
                    Scope* scope, const SrcPos& srcpos) const
{
  if (fImpl != NULL)
    return fImpl->matchGenerics(localCtx, right0, scope, srcpos);
  return false;
}


//----------------------------------------------------------------------------

namespace herschel
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
        hr_invalid("");
      }
      return "??";
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:const k='" << optostr(fOp) << "'>"
          << fLeft.toString() << fRight.toString()
          << "</ty:const>\n";
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
      case kConstOp_and:           hr_invalid("");
      case kConstOp_or:            hr_invalid("");
      case kConstOp_equal:         return "eq";
      case kConstOp_notEqual:      return "neq";
      case kConstOp_less:          return "lt";
      case kConstOp_lessEqual:     return "leq";
      case kConstOp_greater:       return "gt";
      case kConstOp_greaterEqual:  return "geq";
      case kConstOp_in:            return "in";
      case kConstOp_isa:           hr_invalid("");
      }
      return "??";
    }


    virtual String toString() const
    {
      StringBuffer buf;
      buf << "<ty:const k='" << optostr(fOp) << "'>"
          << fValue.toString()
          << "</ty:const>\n";
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
      buf << "<ty:const k='isa'>\n"
          << fType.toString()
          << "</ty:const>\n";
      return buf.toString();
    }

  private:
    TypeConstOperator fOp;
    Type fType;
  };

};                              // namespace herschel


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
  hr_assert(isValueConstraint());
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
  hr_assert(isLogicalConstraint());
  return dynamic_cast<const LogicalConstraintImpl*>(fImpl.obj())->left();
}


const TypeConstraint&
TypeConstraint::rightConstraint() const
{
  hr_assert(isLogicalConstraint());
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
  hr_assert(isTypeConstraint());
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
  return FunctionParameter(kParamPos, !K(isSpec), String(), type);
}


FunctionParameter
FunctionParameter::newSpecParam(const Type& type)
{
  return FunctionParameter(kParamPos, K(isSpec), String(), type);
}


FunctionParameter
FunctionParameter::newNamedParam(const String& key, const Type& type)
{
  return FunctionParameter(kParamNamed, !K(isSpec), key, type);
}


FunctionParameter
FunctionParameter::newRestParam(const Type& type)
{
  return FunctionParameter(kParamRest, !K(isSpec), String(), type);
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


String
FunctionParameter::toString() const
{
  StringBuffer buf;

  buf << "<ty:prm";

  switch (fKind) {
  case kParamPos:
    buf << " is='pos'";
    break;
  case kParamNamed:
    buf << " is='named' key='" << fKey << "'";
    break;
  case kParamRest:
    buf << " is='rest'";
    break;
  }

  buf << ( fIsSpecialized ? " spec='t'" : "" ) << ">\n";
  buf << fType.toString();
  buf << "</ty:prm>\n";
  return buf.toString();
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
                           herschel::vectorClone(fParameters));
}


FunctionSignature
FunctionSignature::replaceGenerics(const TypeCtx& typeMap)
{
  fReturnType = fReturnType.replaceGenerics(typeMap);
  herschel::replaceGenerics(fParameters, typeMap);
  return *this;
}


bool
FunctionSignature::isGeneric() const
{
  return fIsGeneric;
}


bool
FunctionSignature::isOpen() const
{
  if (fReturnType.isOpen())
    return true;

  for (size_t i = 0; i < fParameters.size(); ++i) {
    if (fParameters[i].type().isOpen())
      return true;
  }

  return false;
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


bool
FunctionSignature::matchGenerics(TypeCtx& localCtx,
                                 const FunctionSignature& right0,
                                 Scope* scope, const SrcPos& srcpos) const
{
  if (fParameters.size() == right0.parameters().size()) {
    if (!fReturnType.matchGenerics(localCtx, right0.returnType(),
                                   scope, srcpos))
      return false;
    for (size_t i = 0; i < fParameters.size(); ++i) {
      const FunctionParameter& lparam = fParameters[i];
      const FunctionParameter& rparam = right0.parameters()[i];

      if (lparam.kind() != rparam.kind())
        return false;
      if (!lparam.type().matchGenerics(localCtx, rparam.type(), scope, srcpos))
        return false;
    }
    return true;
  }
  return false;
}


String
FunctionSignature::toString() const
{
  StringBuffer buf;
  buf << "<ty:fun nm='" << fName << "'"
      << (fIsGeneric ? " gen='t'" : "") << ">\n";

  if (!fParameters.empty()) {
    buf << "<ty:prms>\n";
    for (size_t i = 0; i < fParameters.size(); i++)
      buf << fParameters[i].toString();
    buf << "</ty:prms>\n";
  }

  buf << "<ty:ret>\n" << fReturnType.toString() << "</ty:ret>\n";
  buf << "</ty:fun>\n";
  return buf.toString();
}


bool
FunctionSignature::hasPositionalParam() const
{
  for (size_t i = 0; i < fParameters.size(); i++) {
    if (fParameters[i].kind() == FunctionParameter::kParamPos)
      return true;
  }

  return false;
}


namespace herschel
{
  StringBuffer&
  operator<<(StringBuffer& other, const FunctionParamVector& params)
  {
    for (size_t i = 0; i < params.size(); i++) {
      if (i > 0)
        other << ", ";
      other << params[i].type().typeId();
    }
    return other;
  }
}

String
FunctionSignature::typeId() const
{
  StringBuffer buf;
  if (fName.isEmpty())
    buf << "lambda";
  else
    buf << fName;
  buf << "(" << fParameters << ")";
  buf << ":" << fReturnType.typeId();
  return buf.toString();
}


//------------------------------------------------------------------------------

TypeUnit::TypeUnit()
{ }


TypeUnit::TypeUnit(const String& name, const String& derivedFrom,
                   const Type& effectiveType)
  : fName(name),
    fDerivedFrom(derivedFrom),
    fEffType(effectiveType)
{ }


TypeUnit::TypeUnit(const TypeUnit& other)
{
  *this = other;
}


bool
TypeUnit::isDef() const
{
  return !fName.isEmpty();
}


const String&
TypeUnit::name() const
{
  return fName;
}


const String&
TypeUnit::derivedFromName() const
{
  return fDerivedFrom;
}


const Type&
TypeUnit::effType() const
{
  return fEffType;
}


TypeUnit&
TypeUnit::operator=(const TypeUnit& other)
{
  fName = other.fName;
  fDerivedFrom = other.fDerivedFrom;
  fEffType = other.fEffType;
  return *this;
}


//----------------------------------------------------------------------------

namespace herschel
{
  Type
  resolveType(const Type& type, Scope* scope)
  {
    Type ty = ( type.isDef() && type.isRef()
                ? scope->lookupType(type.typeName(), K(showAmbiguousSymDef))
                : type );
    if (ty.isDef() && ty.isOpen()) {
      if (type.isDef())
        return scope->normalizeType(ty, type);
    }
    return ty;
  }


  bool
  isSameType(const TypeVector& vect0, const TypeVector& vect1, Scope* scope,
             const SrcPos& srcpos, bool reportErrors)
  {
    if (vect0.size() == vect1.size()) {
      for (size_t i = 0; i < vect0.size(); i++) {
        if (!isSameType(vect0[i], vect1[i], scope, srcpos, reportErrors))
          return false;
      }
      return true;
    }
    return false;
  }


  bool
  isSameType(const FunctionSignature& leftsig,
             const FunctionSignature& rightsig,
             Scope* scope, const SrcPos& srcpos, bool reportErrors)
  {
    if (!isSameType(leftsig.returnType(), rightsig.returnType(),
                    scope, srcpos, reportErrors))
      return false;
    if (leftsig.parameters().size() != rightsig.parameters().size())
      return false;

    for (size_t i = 0; i < leftsig.parameters().size(); i++) {
      const FunctionParameter& leftprm = leftsig.parameters()[i];
      const FunctionParameter& rightprm = rightsig.parameters()[i];

      if (leftprm.kind() != rightprm.kind() ||
          !isSameType(leftprm.type(), rightprm.type(), scope, srcpos,
                      reportErrors))
        return false;
    }
    return true;
  }


  bool
  isSameType(const Type& left0, const Type& right0, Scope* scope,
             const SrcPos& srcpos, bool reportErrors)
  {
    if (!left0.isDef() || !right0.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    // tyerror(left0, "LEFT IS");
    // tyerror(right0, "RIGHT IS");
    if (left0.isOpenSelf() && right0.isOpenSelf())
      // TODO: handle complex generic types like 'T[]
      return left0.typeName() == right0.typeName();

    Type left;
    Type right;

    if (left0.isOpenSelf()) {
      right = resolveType(right0, scope);
      if (!right.isDef()) {
        if (reportErrors)
          errorf(srcpos, E_UndefinedType,
                 "Undefined type: '%s' (%s:%d)",
                 (const char*)StrHelper(right0.typeId()), __FILE__, __LINE__);
        return false;
      }
      // if only one of both types is open is can not be the same type.
      return false;
    }
    else if (right0.isOpenSelf()) {
      left = resolveType(left0, scope);
      if (!left.isDef()) {
        if (reportErrors)
          errorf(srcpos, E_UndefinedType,
                 "Undefined type: '%s' (%s:%d)",
                 (const char*)StrHelper(left0.typeId()), __FILE__, __LINE__);
        return false;
      }
      // if only one of both types is open is can not be the same type.
      return false;
    }
    else {
      left = resolveType(left0, scope);
      right = resolveType(right0, scope);
    }

    // tyerror(left, "LEFT IS");
    // tyerror(right, "RIGHT IS");
    if (!left.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType,
               "Undefined type: '%s' (%s:%d)",
               (const char*)StrHelper(left0.typeId()), __FILE__, __LINE__);
      return false;
    }
    if (!right.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType,
               "Undefined type: '%s' (%s:%d)",
               (const char*)StrHelper(right0.typeId()), __FILE__, __LINE__);
      return false;
    }

    if (left.isAny()) {
      if (right.isAny())
        return true;
      return false;
    }

#if 0
    // TODO: check constraints
    if (left.hasConstraints()) {
      if (right.hasConstraints()) {
        // TODO
      }
      return false;
    }
#endif

    if (left.isArray()) {
      if (right.isArray())
        return isSameType(left.arrayBaseType(), right.arrayBaseType(), scope,
                          srcpos, reportErrors);
      return false;
    }
    else if (left.isUnion()) {
      if (right.isUnion())
        return isSameType(left.unionTypes(), right.unionTypes(), scope,
                          srcpos, reportErrors);
      return false;
    }
    else if (left.isSequence()) {
      if (right.isSequence())
        return isSameType(left.seqTypes(), right.seqTypes(), scope,
                          srcpos, reportErrors);
      return false;
    }
    else if (left.isMeasure()) {
      if (right.isMeasure())
        if (left.typeName() == right.typeName())
          return true;
      return false;
    }
    else if (left.isFunction()) {
      if (right.isFunction()) {
        return isSameType(left.functionSignature(), right.functionSignature(),
                          scope, srcpos, reportErrors);
      }
      return false;
    }
    else if (left.isType() || left.isClass()) {
      if (left.kind() == right.kind()) {
        if (left.typeName() != right.typeName())
          return false;
        if (!isSameType(left.generics(), right.generics(), scope, srcpos,
                        reportErrors))
          return false;
        return true;
      }
      return false;
    }

    fprintf(stderr, "LEFT: %s\n", (const char*)StrHelper(left.toString()));
    fprintf(stderr, "RIGHT: %s\n", (const char*)StrHelper(right.toString()));
    hr_invalid("unhandled type?");
    return false;
  }


  //! Indicates whether left0 is a subtype of right0.  This is tested by checking
  //! whether right0 is in left0's inheritance list.
  bool
  inheritsFrom(const Type& left0, const Type& right0, Scope* scope,
               const SrcPos& srcpos, bool reportErrors)
  {
    if (!left0.isDef() || !right0.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    Type left = resolveType(left0, scope);
    Type right = resolveType(right0, scope);

    if (!left.isDef() || !right.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    Type inheritance;
    if (left.isType() || left.isClass()) {
      inheritance = left.typeInheritance();
    }
    else if (left.isMeasure()) {
      inheritance = left.measureBaseType();
    }
    else
      return false;

    if (!inheritance.isDef()) {
      return false;
    }
    else if (inheritance.isRef()) {
      inheritance = scope->lookupType(inheritance.typeName(),
                                      K(showAmbiguousSymDef));
    }

    if (!inheritance.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    if (inheritance.isType() || inheritance.isClass()) {
      if (isSameType(inheritance, right, scope, srcpos, reportErrors))
        return true;
      return inheritsFrom(inheritance, right, scope, srcpos, reportErrors);
    }

    if (inheritance.isSequence()) {
      const TypeVector& seq = inheritance.seqTypes();
      for (size_t i = 0; i < seq.size(); ++i) {
        if (isSameType(seq[i], right, scope, srcpos, reportErrors))
          return true;
        if (inheritsFrom(seq[i], right, scope, srcpos, reportErrors))
          return true;
      }
      return false;
    }

    hr_invalid("unexpected type kind");
    return false;
  }


  //----------------------------------------------------------------------------

  bool
  isCovariant(const TypeVector& vect0, const TypeVector& vect1, Scope* scope,
              const SrcPos& srcpos, bool reportErrors)
  {
    if (vect0.size() == vect1.size()) {
      for (size_t i = 0; i < vect0.size(); i++) {
        if (!isCovariant(vect0[i], vect1[i], scope, srcpos, reportErrors))
          return false;
      }
      return true;
    }
    return false;
  }


  bool
  isCovariantToEveryTypeInSeq(const Type& type, const TypeVector& vect0,
                              Scope* scope,
                              const SrcPos& srcpos, bool reportErrors)
  {
    for (size_t i = 0; i < vect0.size(); i++) {
      if (!isCovariant(type, vect0[i], scope, srcpos, reportErrors))
        return false;
    }
    return true;
  }


  bool
  isCoOrInvariantToEveryTypeInUnion(const Type& type, const TypeVector& vect0,
                                    Scope* scope,
                                    const SrcPos& srcpos, bool reportErrors)
  {
    bool hadOneCovariantType = false;
    for (size_t i = 0; i < vect0.size(); i++) {
      if (isContravariant(type, vect0[i], scope, srcpos, reportErrors) &&
          !isSameType(type, vect0[i], scope, srcpos, reportErrors))
        return false;
      if (!hadOneCovariantType &&
          isCovariant(type, vect0[i], scope, srcpos, reportErrors))
        hadOneCovariantType = true;
    }
    return hadOneCovariantType;
  }


  bool
  isCovariantForAllTypesInUnion(const TypeVector& vect0, const TypeVector& vect1,
                                Scope* scope,
                                const SrcPos& srcpos, bool reportErrors)
  {
    for (size_t i = 0; i < vect0.size(); i++) {
      if (!isCoOrInvariantToEveryTypeInUnion(vect0[i], vect1, scope,
                                             srcpos, reportErrors))
        return false;
    }
    return true;
  }


  bool
  isCovariant(const FunctionSignature& leftsig,
              const FunctionSignature& rightsig,
              Scope* scope, const SrcPos& srcpos, bool reportErrors)
  {
    if (!isCovariant(leftsig.returnType(), rightsig.returnType(),
                     scope, srcpos, reportErrors))
      return false;

    if (leftsig.parameters().size() != rightsig.parameters().size())
      return false;

    TypeCtx localCtx;

    for (size_t i = 0; i < leftsig.parameters().size(); i++) {
      const FunctionParameter& leftprm = leftsig.parameters()[i];
      const FunctionParameter& rightprm = rightsig.parameters()[i];

      if (leftprm.kind() == rightprm.kind()) {
        if (leftprm.isSpecialized() && rightprm.isSpecialized()) {
          if (!isCovariant(leftprm.type(), rightprm.type(), scope, srcpos,
                           reportErrors))
            return false;
        }
        else if (leftprm.isSpecialized()) {
          // parameters are not symmetrical specialized
          return false;
        }
        else {
          if (!isContravariant(leftprm.type(), rightprm.type(), scope, srcpos,
                               reportErrors) &&
              // special case: a function taking lang|Any types accepts
              // everything.
              !containsAny(rightprm.type(), srcpos, reportErrors))
            return false;
        }

        if (rightprm.type().isOpenSelf()) {
          String genName = rightprm.type().typeName();

          Type knownType = localCtx.lookupType(genName);
          if (knownType.isDef()) {
            if (!isContravariant(leftprm.type(), knownType, scope, srcpos,
                                 reportErrors))
              return false;
          }

          localCtx.registerType(genName, leftprm.type());
        }
      }
      else
        return false;
    }
    return true;
  }


  bool
  containsAny(const Type& left, const SrcPos& srcpos, bool reportErrors)
  {
    if (!left.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    if (left.isAny())
      return true;
    if (left.isUnion()) {
      const TypeVector& vect = left.unionTypes();
      for (size_t i = 0; i < vect.size(); i++) {
        if (containsAny(vect[i], srcpos, reportErrors))
          return true;
      }
    }

    return false;
  }


  bool
  isCovariant(const Type& left0, const Type& right0, Scope* scope,
              const SrcPos& srcpos, bool reportErrors)
  {
    // fprintf(stderr, "CONTRA-X: %s %s\n", (const char*)StrHelper(left0.toString()),
    //        (const char*)StrHelper(right0.toString()));

    if (!left0.isDef() || !right0.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    if (left0.isOpen() && right0.isOpen())
      // TODO: handle complex generic types like 'T[]
      return isSameType(left0, right0, scope, srcpos, reportErrors);

    Type right;
    Type left;
    if (left0.isOpenSelf()) {
      right = resolveType(right0, scope);
      if (!right.isDef()) {
        if (reportErrors)
          errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
        return false;
      }
      if (isCovariant(right, Type::newAny(), scope, srcpos, reportErrors)) {
        // a generic open type is covariant to Any.  This needs special treatment
        // in the compiler though
        return true;
      }
    }
    else if (right0.isOpenSelf()) {
      left = resolveType(left0, scope);

      if (!left.isDef()) {
        if (reportErrors)
          errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
        return false;
      }

      if (isCovariant(left, Type::newAny(), scope, srcpos, reportErrors)) {
        // a generic open type is covariant to Any.  This needs special treatment
        // in the compiler though
        return true;
      }
    }
    else {
      right = resolveType(right0, scope);
      left = resolveType(left0, scope);
    }

    if (!right.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }
    if (!left.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    if (right.isAny()) {
      // everything is covariant to lang|Any
      return true;
    }

#if 0
    // TODO: check constraints
    if (left.hasConstraints()) {
      if (right.hasConstraints()) {
        // TODO
      }
      return false;
    }
#endif

    if (left.isArray()) {
      if (right.isType() && (right.typeName() == Names::kSliceableTypeName ||
                             right.typeName() == Names::kSliceableXTypeName) &&
          right.generics().size() == 2 &&
          isSameType(right.generics()[0], Type::newInt32(), scope,
                     srcpos, reportErrors) &&
          isSameType(left.arrayBaseType(), right.generics()[1],
                     scope, srcpos, reportErrors))
        return true;
      return isSameType(left, right, scope, srcpos, reportErrors);
    }
    else if (left.isUnion()) {
      if (right.isUnion()) {
        if (isSameType(left, right, scope, srcpos, reportErrors))
          return true;

        return isCovariantForAllTypesInUnion(left.unionTypes(),
                                             right.unionTypes(),
                                             scope, srcpos,
                                             reportErrors);
      }
      return false;
    }
    else if (left.isSequence()) {
      if (right.isSequence()) {
        if (isSameType(left, right, scope, srcpos, reportErrors))
          return true;
        return isCovariant(left.seqTypes(), right.seqTypes(), scope,
                           srcpos, reportErrors);
      }
      return false;
    }
    else if (left.isMeasure()) {
      return ( isSameType(left, right, scope, srcpos, reportErrors) ||
               isCovariant(left.measureBaseType(), right, scope, srcpos,
                           reportErrors) );
    }
    else if (left.isFunction()) {
      if (isSameType(left, right, scope, srcpos, reportErrors))
        return true;
      if (right.isFunction())
        return isCovariant(left.functionSignature(), right.functionSignature(),
                           scope, srcpos, reportErrors);
      else
        return false;
    }
    else if (left.isType() || left.isClass()) {
      if (isSameType(left, right, scope, srcpos, reportErrors))
        return true;

      if (right.isType() || right.isClass()) {
        if (!inheritsFrom(left, right, scope, srcpos, reportErrors))
          return false;
        return isSameType(left.generics(), right.generics(), scope, srcpos,
                          reportErrors);
      }
      else if (right.isSequence()) {
        return isCovariantToEveryTypeInSeq(left, right.seqTypes(), scope,
                                           srcpos, reportErrors);
      }
      else if (right.isUnion()) {
        return isCoOrInvariantToEveryTypeInUnion(left, right.unionTypes(),
                                                 scope,
                                                 srcpos, reportErrors);
      }

      return false;
    }
    else if (left.isAny()) {
      return false;
    }

    tyerror(left, "LEFT");
    tyerror(right, "RIGHT");
    hr_invalid("unhandled type?");
    return false;
  }


  bool
  isContravariant(const Type& left, const Type& right, Scope* scope,
                  const SrcPos& srcpos, bool reportErrors)
  {
    return isCovariant(right, left,
                       scope, srcpos, reportErrors);
  }


  bool
  isInvariant(const Type& left, const Type& right, Scope* scope,
              const SrcPos& srcpos, bool reportErrors)
  {
    return ( !isCovariant(left, right, scope, srcpos, reportErrors) &&
             !isContravariant(left, right, scope, srcpos, reportErrors) );
  }
};                              // namespace herschel


//----------------------------------------------------------------------------

namespace herschel
{
  TypeVector
  newTypeVector()
  {
    return TypeVector();
  }

  TypeVector
  newTypeVector(const Type& ty1)
  {
    TypeVector vector;
    vector.push_back(ty1);
    return vector;
  }


  TypeVector
  newTypeVector(const Type& ty1, const Type& ty2)
  {
    TypeVector vector;
    vector.push_back(ty1);
    vector.push_back(ty2);
    return vector;
  }


  TypeVector
  newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3)
  {
    TypeVector vector;
    vector.push_back(ty1);
    vector.push_back(ty2);
    vector.push_back(ty3);
    return vector;
  }


  TypeVector
  newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3,
                const Type& ty4)
  {
    TypeVector vector;
    vector.push_back(ty1);
    vector.push_back(ty2);
    vector.push_back(ty3);
    vector.push_back(ty4);
    return vector;
  }


  TypeVector
  newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3,
                const Type& ty4, const Type& ty5)
  {
    TypeVector vector;
    vector.push_back(ty1);
    vector.push_back(ty2);
    vector.push_back(ty3);
    vector.push_back(ty4);
    vector.push_back(ty5);
    return vector;
  }


  TypeConstVector
  newTypeConstVector()
  {
    return TypeConstVector();
  }


  Type
  newRangeType(const Type& generic)
  {
    return Type::newType(Names::kRangeTypeName, newTypeVector(generic), Type());
  }


  void
  tyerror(const Type& type, const char* msg)
  {
    fprintf(stderr, "%s: %s\n", msg, (const char*)StrHelper(type.typeId()));
  }


  int
  floatTypeBitsize(const Type& ty)
  {
    if (ty.isBuiltinType(Names::kFloat32TypeName))
      return 32;
    else if (ty.isBuiltinType(Names::kFloat64TypeName))
      return 64;
    else if (ty.isBuiltinType(Names::kFloat128TypeName))
      return 128;

    hr_invalid("unhandled floating type");
    return 0;
  }


  Type
  maxFloatType(const Type& leftty, const Type& rightty)
  {
    if (floatTypeBitsize(leftty) < floatTypeBitsize(rightty))
      return rightty;
    else
      return leftty;
  }


  int
  intTypeBitsize(const Type& ty)
  {
    if (ty.isBuiltinType(Names::kInt8TypeName) ||
        ty.isBuiltinType(Names::kUInt8TypeName))
      return 8;
    else if (ty.isBuiltinType(Names::kInt16TypeName) ||
             ty.isBuiltinType(Names::kUInt16TypeName))
      return 16;
    else if (ty.isBuiltinType(Names::kInt32TypeName) ||
             ty.isBuiltinType(Names::kUInt32TypeName))
      return 32;
    else if (ty.isBuiltinType(Names::kInt64TypeName) ||
             ty.isBuiltinType(Names::kUInt64TypeName))
      return 64;

    hr_invalid("unhandled int type");
    return 0;
  }


  Type
  maxIntType(const Type& leftty, const Type& rightty)
  {
    int righttysize = intTypeBitsize(rightty);
    if (intTypeBitsize(leftty) < righttysize) {
      if (leftty.isAnyUInt()) {
        switch (righttysize) {
        case 8:
          return Type::newTypeRef(Names::kUInt8TypeName, K(isValue));
        case 16:
          return Type::newTypeRef(Names::kUInt16TypeName, K(isValue));
        case 32:
          return Type::newTypeRef(Names::kUInt32TypeName, K(isValue));
        case 64:
          return Type::newTypeRef(Names::kUInt64TypeName, K(isValue));
        default:
          hr_invalid("unhandled int type size");
        }
      }
      return rightty;
    }
    else
      return leftty;
  }


  Type
  degeneralizeType(const SrcPos& srcpos, const Type& type,
                   const TypeVector& srcGenerics)
  {
    if (type.isDef()) {
      if (type.hasGenerics()) {
        if (type.generics().size() != srcGenerics.size()) {
          errorf(srcpos, E_GenericsMismatch,
                 "Type instance generic number mismatch");
          return Type();
        }
        if (!srcGenerics.empty() && !type.isOpen()) {
          errorf(srcpos, E_GenericsMismatch,
                 "Type instance generic number mismatch");
          return Type();
        }

        TypeCtx localCtx;
        for (size_t i = 0; i < type.generics().size(); i++) {
          Type gen = type.generics()[i];
          hr_assert(gen.isRef());

          String genName = gen.typeName();
          localCtx.registerType(genName, srcGenerics[i]);
        }

        // TODO: shouldn't this be Class<some-type> ?
        return type.replaceGenerics(localCtx);
      }
      else {
        if (!srcGenerics.empty()) {
          errorf(srcpos, E_GenericsMismatch,
                 "Type instance generic number mismatch");
          return Type();
        }

        // TODO: shouldn't this be Class<some-type> ?
        return type;
      }
    }

    return Type();
  }
};


//============================================================================

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>
#include <iostream>

namespace herschel
{
  std::ostream& operator<<(std::ostream& os, const TypeConstraint& constraint)
  {
    os << constraint.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const Type& type)
  {
    os << type.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const FunctionParameter& prm)
  {
    os << prm.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const FunctionSignature& sign)
  {
    os << sign.toString();
    return os;
  }
};


static Scope* testScopeSetup()
{
  Ptr<Scope> scope = herschel::type::newRootScope(K(forUnitTests));

  TypeVector generics;

  // Test class tree:
  //
  // Obj <- Base     <- Medium  <- Top
  //     ^           <- Special <- Ultra
  //     |               |
  //     |               v
  //     \- Abstract <- Xyz

  // scope->registerType(SrcPos(), Names::kAnyTypeName, Type::newAny(true));

  scope->registerType(SrcPos(), String("Obj"),
                      Type::newType(String("Obj"), generics, Type()));
  scope->registerType(SrcPos(), String("Base"),
                      Type::newType(String("Base"),
                                    generics,
                                    Type::newTypeRef("Obj")));

  scope->registerType(SrcPos(), String("Medium"),
                      Type::newType(String("Medium"),
                                    generics,
                                    Type::newTypeRef("Base")));
  scope->registerType(SrcPos(), String("Top"),
                      Type::newType(String("Top"),
                                    generics,
                                    Type::newTypeRef("Medium")));

  scope->registerType(SrcPos(), String("Abstract"),
                      Type::newType(String("Abstract"),
                                    generics,
                                    Type::newTypeRef("Obj")));
  scope->registerType(SrcPos(), String("Xyz"),
                      Type::newType(String("Xyz"),
                                    generics,
                                    Type::newTypeRef("Abstract")));

  TypeVector isa;
  isa.push_back(Type::newTypeRef("Base"));
  isa.push_back(Type::newTypeRef("Xyz"));
  scope->registerType(SrcPos(), String("Special"),
                      Type::newType(String("Special"),
                                    generics,
                                    Type::newSeq(isa, K(isValue))));
  scope->registerType(SrcPos(), String("Ultra"),
                      Type::newType(String("Ultra"),
                                    generics,
                                    Type::newTypeRef("Special")));

  return scope.release();
}


SUITE(Type_IsSameType)
{
  TEST(basicTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isSameType(Type::newTypeRef("Base"),
                               Type::newTypeRef("Base"),
                               scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isSameType(Type::newTypeRef("Xyz"),
                               Type::newTypeRef("Xyz"),
                               scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isSameType(Type::newTypeRef("Base"),
                                Type::newTypeRef("Medium"),
                                scope, SrcPos(), !K(reportError)));

    CHECK(!herschel::isSameType(Type::newTypeRef("Base"),
                                Type::newTypeRef("Hello"),
                                scope, SrcPos(), !K(reportError)));
  }


  TEST(arrayTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newArray(Type::newTypeRef("Base"), 17, !K(isValue)),
            scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newArray(Type::newTypeRef("Xyz"), 17, !K(isValue)),
            scope, SrcPos(), !K(reportErrors)));

    CHECK(herschel::isSameType(
            Type::newArray(Type::newAny(K(isValue)), 5, K(isValue)),
            Type::newArray(Type::newAny(K(isValue)), 17, !K(isValue)),
            scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newTypeRef("Base"),
            scope, SrcPos(), !K(reportErrors)));
  }


  TEST(anyTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isSameType(Type::newAny(K(isValue)),
                               Type::newAny(K(isValue)),
                               scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(Type::newAny(K(isValue)),
                                Type::newTypeRef("Medium"),
                                scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(Type::newTypeRef("Xyz"),
                                Type::newAny(K(isValue)),
                                scope, SrcPos(), K(reportErrors)));
  }


  TEST(unionTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    TypeVector union0;
    union0.push_back(Type::newTypeRef("Xyz"));
    union0.push_back(Type::newTypeRef("Medium"));

    TypeVector union1;
    union1.push_back(Type::newTypeRef("Medium"));
    union1.push_back(Type::newTypeRef("Xyz"));

    CHECK(herschel::isSameType(Type::newUnion(union0, K(isValue)),
                               Type::newUnion(union0, K(isValue)),
                               scope, SrcPos(), K(reportErrors)));
    CHECK(!herschel::isSameType(Type::newUnion(union0, K(isValue)),
                                Type::newUnion(union1, K(isValue)),
                                scope, SrcPos(), K(reportErrors)));

    TypeVector union2;
    union2.push_back(Type::newTypeRef("Medium"));
    union2.push_back(Type::newTypeRef("Ultra"));
    union2.push_back(Type::newTypeRef("Abstract"));

    CHECK(!herschel::isSameType(Type::newUnion(union0, K(isValue)),
                                Type::newUnion(union2, K(isValue)),
                                scope, SrcPos(), K(reportErrors)));
  }


  TEST(seqTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    TypeVector seq0;
    seq0.push_back(Type::newTypeRef("Xyz"));
    seq0.push_back(Type::newTypeRef("Medium"));

    TypeVector seq1;
    seq1.push_back(Type::newTypeRef("Medium"));
    seq1.push_back(Type::newTypeRef("Xyz"));

    CHECK(herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                               Type::newSeq(seq0, K(isValue)),
                               scope, SrcPos(), K(reportErrors)));
    CHECK(!herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                                Type::newSeq(seq1, K(isValue)),
                                scope, SrcPos(), K(reportErrors)));

    TypeVector seq2;
    seq2.push_back(Type::newTypeRef("Medium"));
    seq2.push_back(Type::newTypeRef("Ultra"));
    seq2.push_back(Type::newTypeRef("Abstract"));

    CHECK(!herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                                Type::newSeq(seq2, K(isValue)),
                                scope, SrcPos(), K(reportErrors)));
  }


  TEST(measureTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                Type::newTypeRef("Xyz"),
                                                String("mk")),
                               Type::newMeasure(String("Maiko"),
                                                Type::newTypeRef("Xyz"),
                                                String("mk")),
                               scope, SrcPos(), !K(reportError)));

    CHECK(!herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("mk")),
                                Type::newTypeRef(String("Xyz"), K(isValue)),
                                scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("mk")),
                                Type::newMeasure(String("Tomoko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("to")),
                                scope, SrcPos(), !K(reportErrors)));
  }


  TEST(functionTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    FunctionParamVector params0;
    CHECK(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               scope, SrcPos(), !K(reportErrors)));

    CHECK(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("foo"),
                                                    Type::newTypeRef("Xyz"),
                                                    params0)),
                                Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("bar"),
                                                    Type::newTypeRef("Abstract"),
                                                    params0)),
                                scope, SrcPos(), !K(reportErrors)));

    FunctionParamVector params1;
    params1.push_back(FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                                        String(), Type::newTypeRef("Medium")));
    CHECK(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               scope, SrcPos(), !K(reportErrors)));

    CHECK(!herschel::isSameType(Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("foo"),
                                                    Type::newTypeRef("Xyz"),
                                                    params1)),
                                Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("bar"),
                                                    Type::newTypeRef("Xyz"),
                                                    params0)),
                                scope, SrcPos(), !K(reportErrors)));

    params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                        String("na"),
                                        Type::newTypeRef("Xyz")));
    params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                        String("nu"),
                                        Type::newTypeRef("Abstract")));
    params1.push_back(FunctionParameter(FunctionParameter::kParamRest, !K(isSpec),
                                        String("rest"),
                                        Type::newAny(K(isValue))));

    CHECK(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               scope, SrcPos(), !K(reportErrors)));
  }

  // TODO check generic types
  // TODO check combinations of tests (arrays of generics, arrays of unions,
  // sequences of generics and function types, etc.)
}


//----------------------------------------------------------------------------

SUITE(Type_Inheritance)
{
  // Test class tree:
  //
  // Obj <- Base     <- Medium  <- Top
  //     ^           <- Special <- Ultra
  //     |               |
  //     |               v
  //     \- Abstract <- Xyz


  TEST(basicTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    // a type A does not inherit itself
    CHECK(!herschel::inheritsFrom(Type::newTypeRef("Base"),
                                  Type::newTypeRef("Base"),
                                  scope, SrcPos(), !K(reportError)));
    CHECK(herschel::inheritsFrom(Type::newTypeRef("Ultra"),
                                 Type::newTypeRef("Obj"),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(herschel::inheritsFrom(Type::newTypeRef("Special"),
                                 Type::newTypeRef("Base"),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(herschel::inheritsFrom(Type::newTypeRef("Special"),
                                 Type::newTypeRef("Abstract"),
                                 scope, SrcPos(), !K(reportError)));

    CHECK(!herschel::inheritsFrom(Type::newTypeRef("Top"),
                                  Type::newTypeRef("Abstract"),
                                  scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::inheritsFrom(Type::newTypeRef("Xyz"),
                                  Type::newTypeRef("Base"),
                                  scope, SrcPos(), !K(reportError)));
  }

  // TODO check generic types

  TEST(measureTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  scope, SrcPos(), !K(reportError)));

    CHECK(herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                  Type::newTypeRef("Ultra"),
                                                  String("mk")),
                                 Type::newTypeRef(String("Abstract"), K(isValue)),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  Type::newTypeRef(String("Base"), K(isValue)),
                                  scope, SrcPos(), !K(reportError)));

    CHECK(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Ultra"),
                                                   String("mk")),
                                  Type::newMeasure(String("Tomoko"),
                                                   Type::newTypeRef("Abstract"),
                                                   String("to")),
                                  scope, SrcPos(), !K(reportError)));
  }
}


SUITE(Type_Covariance)
{
  // Test class tree:
  //
  // Obj <- Base     <- Medium  <- Top
  //     ^           <- Special <- Ultra
  //     |               |
  //     |               v
  //     \- Abstract <- Xyz


  TEST(basicTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isCovariant(Type::newTypeRef("Base"),
                                Type::newTypeRef("Base"),
                                scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isCovariant(Type::newTypeRef("Xyz"),
                                Type::newTypeRef("Xyz"),
                                scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isCovariant(Type::newTypeRef("Medium"),
                                Type::newTypeRef("Base"),
                                scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isCovariant(Type::newTypeRef("Base"),
                                 Type::newTypeRef("Medium"),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isContravariant(Type::newTypeRef("Base"),
                                    Type::newTypeRef("Medium"),
                                    scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isContravariant(Type::newTypeRef("Medium"),
                                     Type::newTypeRef("Base"),
                                     scope, SrcPos(), !K(reportError)));

    CHECK(!herschel::isCovariant(Type::newTypeRef("Top"),
                                 Type::newTypeRef("Xyz"),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isContravariant(Type::newTypeRef("Top"),
                                     Type::newTypeRef("Xyz"),
                                     scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isInvariant(Type::newTypeRef("Top"),
                                Type::newTypeRef("Xyz"),
                                scope, SrcPos(), !K(reportError)));
  }


  TEST(MultipleInheritance_basicTypes)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isCovariant(Type::newTypeRef("Ultra"),
                                Type::newTypeRef("Base"),
                                scope, SrcPos(), !K(reportError)));
    CHECK(herschel::isCovariant(Type::newTypeRef("Ultra"),
                                Type::newTypeRef("Abstract"),
                                scope, SrcPos(), !K(reportError)));
  }

  TEST(SliceableArrays)
  {
    Ptr<Scope> scope = testScopeSetup();

    CHECK(herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Ultra"), K(isValue)),
                                               0, K(isValue)),
                                Type::newType(Names::kSliceableTypeName,
                                              newTypeVector(Type::newInt32(),
                                                            Type::newTypeRef("Ultra")),
                                              Type()),
                                scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Special"), K(isValue)),
                                                0, K(isValue)),
                                 Type::newType(Names::kSliceableTypeName,
                                               newTypeVector(Type::newInt32(),
                                                             Type::newTypeRef("Ultra")),
                                               Type()),
                                 scope, SrcPos(), !K(reportError)));
    CHECK(!herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Ultra"), K(isValue)),
                                                0, K(isValue)),
                                 Type::newType(Names::kSliceableTypeName,
                                               newTypeVector(Type::newInt32(),
                                                             Type::newTypeRef("Special")),
                                               Type()),
                                 scope, SrcPos(), !K(reportError)));
  }
}


//----------------------------------------------------------------------------
SUITE(TypeConstraint)
{
  TEST(construction)
  {
    SrcPos sp;
    TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                                 Token(sp, kInt, 42));
    CHECK_EQUAL(t0.constOp(), kConstOp_equal);
  }


  TEST(equalConstraint)
  {
    SrcPos sp;
    TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                                 Token(sp, kInt, 42));
    TypeConstraint t1 = TypeConstraint::newValue(kConstOp_equal,
                                                 Token(sp, kInt, 42));
    CHECK_EQUAL(t0.constOp(), kConstOp_equal);
    CHECK_EQUAL(t0, t1);
  }


  TEST(andConstraint)
  {
    SrcPos sp;
    TypeConstraint t0 = TypeConstraint::newValue(kConstOp_notEqual,
                                                 Token(sp, kInt, 10));
    TypeConstraint t1 = TypeConstraint::newValue(kConstOp_notEqual,
                                                 Token(sp, kInt, 21));
    TypeConstraint t2 = TypeConstraint::newAnd(t0, t1);
    TypeConstraint t3 = TypeConstraint::newOr(t0, t2);

    CHECK_EQUAL(t2.constOp(), kConstOp_and);
    CHECK_EQUAL(t2.leftConstraint(), t0);
    CHECK_EQUAL(t2.rightConstraint(), t1);

    CHECK_EQUAL(t3.constOp(), kConstOp_or);
    CHECK_EQUAL(t3.leftConstraint(), t0);
    CHECK_EQUAL(t3.rightConstraint(), t2);
  }


  TEST(isaConstraint)
  {
    SrcPos sp;
    TypeConstraint t0 = TypeConstraint::newType(kConstOp_isa,
                                                Type::newInt32());
    CHECK_EQUAL(t0.constOp(), kConstOp_isa);
    CHECK_EQUAL(t0.typeConstraint(), Type::newInt32());
  }
}


//----------------------------------------------------------------------------

SUITE(FunctionParameter)
{
  TEST(posParamCtor)
  {
    FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt32());
    CHECK(p0.type().isInt32());
    CHECK(!p0.isSpecialized());
    CHECK(p0.key().isEmpty());
    CHECK_EQUAL(p0.kind(), FunctionParameter::kParamPos);
  }

  TEST(specParamCtor)
  {
    FunctionParameter p0 = FunctionParameter::newSpecParam(Type::newInt32());
    CHECK(p0.type().isInt32());
    CHECK(p0.isSpecialized());
    CHECK(p0.key().isEmpty());
    CHECK_EQUAL(p0.kind(), FunctionParameter::kParamPos);
  }

  TEST(namedParamCtor)
  {
    FunctionParameter p0 = FunctionParameter::newNamedParam(String("abc"),
                                                            Type::newInt32());
    CHECK(p0.type().isInt32());
    CHECK(!p0.isSpecialized());
    CHECK_EQUAL(p0.key(), String("abc"));
    CHECK_EQUAL(p0.kind(), FunctionParameter::kParamNamed);
  }

  TEST(restParamCtor)
  {
    FunctionParameter p0 = FunctionParameter::newRestParam(Type::newInt32());
    CHECK(p0.type().isInt32());
    CHECK(!p0.isSpecialized());
    CHECK(p0.key().isEmpty());
    CHECK_EQUAL(p0.kind(), FunctionParameter::kParamRest);
  }
}


//----------------------------------------------------------------------------

TEST(FunctionSignature)
{
  FunctionSignature fs0 = FunctionSignature(!K(isGeneric),
                                            String("abc"), Type::newInt32());

  FunctionParamVector params1;
  params1.push_back(FunctionParameter::newSpecParam(Type::newString()));
  params1.push_back(FunctionParameter::newPosParam(Type::newInt32()));
  params1.push_back(FunctionParameter::newNamedParam(String("xyz"), Type::newFloat32()));
  params1.push_back(FunctionParameter::newRestParam(Type::newAny()));

  FunctionSignature fs1 = FunctionSignature(K(isGeneric),
                                            String("man"), Type::newInt32(),
                                            params1);

  CHECK(!fs0.isGeneric());
  CHECK_EQUAL(fs0.methodName(), String("abc"));
  CHECK_EQUAL(fs0.returnType(), Type::newInt32());
  CHECK(fs0.parameters().empty());

  CHECK(fs1.isGeneric());
  CHECK_EQUAL(fs1.methodName(), String("man"));
  CHECK_EQUAL(fs1.returnType(), Type::newInt32());
  CHECK_EQUAL(fs1.parameters().size(), (size_t)4);
  CHECK(fs1.parameters()[0].type().isString());
  CHECK_EQUAL(fs1.parameters()[0].kind(), FunctionParameter::kParamPos);
  CHECK(fs1.parameters()[0].isSpecialized());

  CHECK(fs1.parameters()[1].type().isInt32());
  CHECK_EQUAL(fs1.parameters()[1].kind(), FunctionParameter::kParamPos);

  CHECK(fs1.parameters()[2].type().isFloat32());
  CHECK_EQUAL(fs1.parameters()[2].kind(), FunctionParameter::kParamNamed);

  CHECK(fs1.parameters()[3].type().isAny());
  CHECK_EQUAL(fs1.parameters()[3].kind(), FunctionParameter::kParamRest);
}


TEST(FunctionSignIsOpen)
{
  FunctionSignature fs0 = FunctionSignature(!K(isGeneric),
                                            String("abc"), Type::newInt32());
  CHECK(!fs0.isOpen());

  FunctionParamVector params1;
  params1.push_back(FunctionParameter::newSpecParam(Type::newString()));
  params1.push_back(FunctionParameter::newPosParam(Type::newTypeRef(String("x"),
                                                                    K(isOpen), !K(isValue))));

  FunctionSignature fs1 = FunctionSignature(K(isGeneric), String("man"),
                                            Type::newTypeRef(String("y"),
                                                             K(isOpen), !K(isValue)),
                                            params1);
  CHECK(fs1.isOpen());
}


#endif  // #if defined(UNITTESTS)

