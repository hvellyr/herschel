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
#include "rootscope.h"
#include "scope.h"
#include "strbuf.h"
#include "type.h"
#include "typectx.h"
#include "typeenum.h"
#include "typeprops-bool.h"
#include "typeprops-char.h"
#include "typeprops-float.h"
#include "typeprops-int.h"
#include "typeprops-keyword.h"
#include "typeprops-string.h"
#include "utils.h"


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
  std::vector<T> vectorClone(const std::vector<T>& v)
  {
    std::vector<T> result;

    std::transform(v.begin(), v.end(),
                   std::back_inserter(result),
                   [](const T& e){ return e.clone(); });

    return result;
  }


  template<typename T>
  void replaceGenerics(std::vector<T>& v, const TypeCtx& typeMap)
  {
    std::for_each(v.begin(), v.end(),
                  [&](T& t) {
                    T replacement = t.replaceGenerics(typeMap);
                    if (replacement.isDef())
                      t = replacement;
                  });
  }


  template<typename T>
  bool isOpen(const std::vector<T>& v)
  {
    return std::any_of(v.begin(), v.end(),
                       [](const T& t) {
                         return t.isOpen();
                       });
  }


  StringBuffer& operator<<(StringBuffer& other, const TypeVector& tyve)
  {
    if (!tyve.empty()) {
      other << tyve.front().typeId();

      std::for_each(std::next(tyve.begin()), tyve.end(),
                    [&](const Type& t) {
                      other << ", ";
                      other << t.typeId();
                    });
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


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const GroupTypeImpl*>(other);

      return (o && typeid(this) == typeid(other) &&
              herschel::isEqual(fTypes, o->fTypes));
    }


    bool isOpen() const override
    {
      return std::any_of(fTypes.begin(), fTypes.end(),
                         [](const Type& t) {
                           return t.isOpen();
                         });
    }


    bool isOpenSelf() const override
    {
      return false;
    }


    const TypeVector& types() const
    {
      return fTypes;
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      herschel::replaceGenerics(fTypes, typeMap);
    }


    bool containsType(const Type& type) const
    {
      return type.isDef() &&
        std::any_of(fTypes.begin(), fTypes.end(),
                    [&](const Type& t) {
                      return t.typeName() == type.typeName();
                    });
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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<UnionTypeImpl>(vectorClone(fTypes));
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:union" << ( !isValue ? " ref='t'" : "") << ">\n";
      for (const auto& t : fTypes)
        buf << t.toString();
      buf << "</ty:union>\n";
      return buf.toString();
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope, const SrcPos& srcpos) const override
    {
      if (right0.isUnion() && types().size() == right0.unionTypes().size()) {
        const auto& ltypes = types();
        const auto& rtypes = right0.unionTypes();

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

    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<SeqTypeImpl>(vectorClone(fTypes));
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:seq" << ( !isValue ? " ref='t'" : "") << ">\n";
      for (const auto& t : fTypes)
        buf << t.toString();
      buf << "</ty:seq>\n";
      return buf.toString();
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
    {
      if (right0.isSequence() && types().size() == right0.seqTypes().size()) {
        const auto& ltypes = types();
        const auto& rtypes = right0.seqTypes();

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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<FunctionTypeImpl>(fSign.clone());
    }


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const FunctionTypeImpl*>(other);
      return (o && fSign == o->fSign);
    }


    bool isOpen() const override
    {
      return fSign.isOpen();
    }


    bool isOpenSelf() const override
    {
      return false;
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
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


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      fSign.replaceGenerics(typeMap);
    }


    String toString(bool isValue) const override
    {
      return fSign.toString();
    }

  private:
    FunctionSignature fSign;
  };


  //--------------------------------------------------------------------------

  namespace
  {
    bool matchGenericsForType(TypeCtx& localCtx,
                              const String& typeName, const TypeVector& generics,
                              const Type& ty,
                              const Scope& scope, const SrcPos& srcpos)
    {
      if (generics.size() == ty.generics().size() &&
          typeName == ty.typeName())
      {
        for (size_t i = 0; i < generics.size(); ++i) {
          if (!generics[i].matchGenerics(localCtx, ty.generics()[i],
                                         scope, srcpos)) {
            return false;
          }
        }
        return true;
      }

      return false;
    }


    bool registerGenerics(TypeCtx& localCtx, const Type& ty,
                          const Scope& scope)
    {
      if (ty.hasGenerics())
      {
        Type typespec = scope.lookupType(ty.typeName(),
                                         K(showAmbiguousSymDef));
        if (typespec.isDef() &&
            typespec.generics().size() == ty.generics().size())
        {
          const TypeVector& genprms = typespec.generics();
          const TypeVector& genargs = ty.generics();

          for (size_t i = 0; i < genargs.size(); ++i) {
            //hr_assert(genprms[i].isOpenSelf());

            if (!genargs[i].isOpenSelf()) {
              if (localCtx.hasType(genprms[i].typeName())) {
                // TODO
              }
              else {
                localCtx.registerType(genprms[i].typeName(), genargs[i]);
              }
            }
          }
          return true;
        }
      }

      return true; //false;
    }


    bool matchGenericsImpl(TypeCtx& localCtx,
                           const String& typeName, const TypeVector& generics,
                           const Type& right0,
                           const Scope& scope, const SrcPos& srcpos)
    {
      if (right0.isRef() || right0.isType() || right0.isClass()) {
        // if the reference has generics, it itself cannot be generic.  A
        // 'T<'Y'> is not allowed.
        if (!generics.empty()) {
          if (!registerGenerics(localCtx, right0, scope))
            return false;

          if (matchGenericsForType(localCtx, typeName, generics, right0,
                                   scope, srcpos))
            return true;

          Type right = scope.lookupType(right0.typeName(),
                                        K(showAmbiguousSymDef));

          Type inheritance;
          if (right.isType() || right.isClass()) {
            inheritance = right.typeInheritance();
          }
          else if (right.isMeasure()) {
            inheritance = right.measureBaseType();
          }
          else
            return false;

          if (!inheritance.isDef()) {
            return false;
          }

          if (inheritance.isType() || inheritance.isClass() || inheritance.isRef()) {
            if (matchGenericsImpl(localCtx, typeName, generics, inheritance,
                                  scope, srcpos))
              return true;
          }
          else if (inheritance.isSequence()) {
            const TypeVector& seq = inheritance.seqTypes();
            for (const auto& s : seq) {
              if (matchGenericsImpl(localCtx, typeName, generics, s, scope, srcpos))
                return true;
            }
          }
          return false;
        }

        if (localCtx.hasType(typeName)) {
          if (!isContravariant(localCtx.lookupType(typeName), right0, scope, srcpos) &&
              !isSameType(localCtx.lookupType(typeName), right0, scope, srcpos) )
          {
            errorf(srcpos, E_TypeMismatch, "type mismatch for generic parameter");
            return false;
          }
          return true;
        }
        else {
          localCtx.registerType(typeName, right0);
          return true;
        }
      }
      else if (right0.isArray()) {
        // special case: Make lang/sliceable<K, E> match arrays, which are
        // otherwise not first class entities.
        if (typeName == Names::kSliceableTypeName || typeName == Names::kSliceableXTypeName) {
          if (generics.size() == 2) {
            localCtx.registerType(generics[0].typeName(), Type::makeUInt32());
            localCtx.registerType(generics[1].typeName(), right0.arrayBaseType());

            return true;
          }
        }
      }

      return false;
    }
  } // end anon namespace


  //--------------------------------------------------------------------------

  class TypeTypeImpl : public TypeImpl
  {
  public:
    TypeTypeImpl(const String& name,
                 bool isInstantiatable,
                 const TypeVector& generics,
                 const Type& inherit,
                 const FunctionSignature& applySign,
                 const TypeSlotList& slots)
      : fName(name),
        fIsInstantiatable(isInstantiatable),
        fGenerics(generics),
        fInherit(inherit),
        fApplySign(applySign),
        fSlots(slots)
    { }


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<TypeTypeImpl>(fName, fIsInstantiatable,
                                            vectorClone(fGenerics),
                                            fInherit.clone(),
                                            fApplySign.clone(),
                                            vectorClone(fSlots));
    }


    bool isEqual(const TypeImpl* other) const override
    {
      const TypeTypeImpl* o = dynamic_cast<const TypeTypeImpl*>(other);

      return (o &&
              fName == o->fName &&
              fIsInstantiatable == o->fIsInstantiatable &&
              fInherit == o->fInherit &&
              herschel::isEqual(fGenerics, o->fGenerics));
    }


    bool isOpen() const override
    {
      return (fInherit.isOpen() || herschel::isOpen(fGenerics));
    }


    bool isOpenSelf() const override
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


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      for (auto& gen : fGenerics) {
        hr_assert(gen.isRef());

        Type replacement = typeMap.lookupType(gen.typeName());
        if (replacement.isDef())
          gen = replacement;
      }
      fInherit = fInherit.replaceGenerics(typeMap);

      for (auto& s : fSlots)
        s.replaceGenerics(typeMap);
    }


    const TypeSlotList& slots() const
    {
      return fSlots;
    }


    String toString(bool isValue) const override
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

#if 0
      if (!fSlots.empty()) {
        buf << "<ty:slots>\n";
        for (size_t i = 0; i < fSlots.size(); i++)
          buf << fSlots[i].toString();
        buf << "</ty:slots>\n";
      }
#endif

      buf << "</ty:type>\n";
      return buf.toString();
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
    {
      return matchGenericsImpl(localCtx, fName, fGenerics, right0, scope, srcpos);
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
    TypeSlotList            fSlots;
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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<AliasTypeImpl>(fName,
                                             vectorClone(fGenerics),
                                             fType.clone());
    }


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const AliasTypeImpl*>(other);

      return (o &&
              fName == o->fName &&
              herschel::isEqual(fGenerics, o->fGenerics) &&
              fType == o->fType);
    }


    bool isOpen() const override
    {
      return fType.isOpen() || herschel::isOpen(fGenerics);
    }


    bool isOpenSelf() const override
    {
      return false;
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
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


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      // TODO
      hr_invalid("");
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:alias nm='" << fName << "'>\n";

      if (!fGenerics.empty()) {
        buf << "<ty:gen>\n";
        for (const auto& g : fGenerics)
          buf << g.toString();
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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<MeasureTypeImpl>(fName, fBaseType.clone(),
                                               fDefUnit);
    }


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const MeasureTypeImpl*>(other);

      return (o &&
              fName == o->fName &&
              fDefUnit == o->fDefUnit &&
              fBaseType == o->fBaseType);
    }


    bool isOpen() const override
    {
      return fBaseType.isOpen();
    }


    bool isOpenSelf() const override
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


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      fBaseType.replaceGenerics(typeMap);
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:measure nm='" << fName << "' unit='"
          << fDefUnit << "'>\n";

      if (fBaseType.isDef())
        buf << "<ty:isa>\n" << fBaseType.toString() << "</ty:isa>\n";

      buf << "</ty:measure>\n";
      return buf.toString();
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<TypeRefTypeImpl>(fName,
                                               fIsOpen,
                                               vectorClone(fGenerics),
                                               vectorClone(fConstraints));
    }


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const TypeRefTypeImpl*>(other);

      return (o &&
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


    bool isOpen() const override
    {
      return fIsOpen || herschel::isOpen(fGenerics);
    }


    bool isOpenSelf() const override
    {
      return fIsOpen;
    }


    const TypeVector& generics() const
    {
      return fGenerics;
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      herschel::replaceGenerics(fGenerics, typeMap);
      herschel::replaceGenerics(fConstraints, typeMap);
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:ref" << (fIsOpen ? " gen='t'" : "")
          << ( !isValue ? " ref='t'" : "")
          << " nm='" << fName << "'>\n";
      if (!fGenerics.empty()) {
        buf << "<ty:gen>\n";
        for (const auto& g : fGenerics)
          buf << g.toString();
        buf << "</ty:gen>\n";
      }
      if (!fConstraints.empty()) {
        if (fConstraints.size() == 1)
          buf << fConstraints[0].toString();
        else {
          buf << "<ty:consts>\n";
          for (const auto& c : fConstraints)
            buf << c.toString();
          buf << "</ty:consts>\n";
        }
      }
      buf << "</ty:ref>\n";
      return buf.toString();
    }


     bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                        const Scope& scope,
                        const SrcPos& srcpos) const override
    {
      return matchGenericsImpl(localCtx, fName, fGenerics, right0, scope, srcpos);
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


    std::shared_ptr<TypeImpl> clone() const override
    {
      return std::make_shared<ArrayTypeImpl>(fBase.clone(), fSizeIndicator);
    }


    bool isEqual(const TypeImpl* other) const override
    {
      auto o = dynamic_cast<const ArrayTypeImpl*>(other);

      return (o &&
              fBase == o->fBase &&
              fSizeIndicator == o->fSizeIndicator);
    }


    bool isOpen() const override
    {
      return fBase.isOpen();
    }


    bool isOpenSelf() const override
    {
      return fBase.isOpenSelf();
    }


    const Type& baseType() const
    {
      return fBase;
    }


    int sizeIndicator() const
    {
      return fSizeIndicator;
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      fBase = fBase.replaceGenerics(typeMap);
    }


    String toString(bool isValue) const override
    {
      StringBuffer buf;
      buf << "<ty:array ind='" << fromInt(fSizeIndicator) << "'"
          << ( !isValue ? " ref='t'" : "") << ">\n"
          << fBase.toString()
          << "</ty:array>\n";
      return buf.toString();
    }


    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       const Scope& scope,
                       const SrcPos& srcpos) const override
    {
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


Type::Type(TypeKind kind, bool isValue, bool isImaginary,
           std::shared_ptr<TypeImpl> impl)
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
Type::makeTypeRef(const String& name, const TypeVector& genericArgs,
                 const TypeConstVector& constraints, bool isValue)
{
  return Type(kType_Ref, isValue, !K(isImg),
              std::make_shared<TypeRefTypeImpl>(
                name, !K(isOpen), genericArgs, constraints));
}


Type
Type::makeTypeRef(const String& name, const TypeVector& genericArgs,
                 bool isValue)
{
  return Type(kType_Ref, isValue, !K(isImg),
              std::make_shared<TypeRefTypeImpl>(
                name, !K(isOpen), genericArgs, TypeConstVector()));
}


Type
Type::makeTypeRef(const String& name, bool isValue)
{
  TypeVector dummyGenerics;
  TypeConstVector dummyConstraints;
  return Type(kType_Ref, isValue, !K(isImg),
              std::make_shared<TypeRefTypeImpl>(
                name, !K(isOpen), dummyGenerics, dummyConstraints));
}


Type
Type::makeTypeRef(zstring name, bool isValue)
{
  return makeTypeRef(String(name), isValue);
}


Type
Type::makeTypeRef(const String& name, bool isOpen,
                 const TypeConstVector& constraints, bool isValue)
{
  TypeVector dummyGenerics;
  return Type(kType_Ref, isValue, !K(isImg),
              std::make_shared<TypeRefTypeImpl>(
                name, isOpen, dummyGenerics, constraints));
}


Type
Type::makeTypeRef(const String& name, bool isOpen, bool isValue)
{
  TypeVector dummyGenerics;
  TypeConstVector dummyConstraints;
  return Type(kType_Ref, isValue, !K(isImg),
              std::make_shared<TypeRefTypeImpl>(
                name, isOpen, dummyGenerics, dummyConstraints));
}


Type
Type::makeTypeRef(const String& name, const Type& old)
{
  hr_assert(old.isRef());

  return Type(kType_Ref, old.isValueType(), old.isImaginary(),
              std::make_shared<TypeRefTypeImpl>(
                name,
                std::dynamic_pointer_cast<TypeRefTypeImpl>(old.fImpl)->isOpenSelf(),
                old.generics(),
                old.constraints()));
}


Type
Type::makeClassOf(const Type& type, bool isValue)
{
  return makeTypeRef(Names::kClassTypeName, makeVector(type), isValue);
}


Type
Type::makeArray(const Type& base, int sizeIndicator, bool isValue)
{
  return Type(kType_Array, isValue, !K(isImg),
              std::make_shared<ArrayTypeImpl>(base, sizeIndicator));
}


Type
Type::makeAny(bool isValue)
{
  return makeTypeRef(Names::kAnyTypeName, isValue);
}


Type
Type::makeInt32(bool isValue)
{
  return makeInt(32, isValue);
}


Type
Type::makeUInt32(bool isValue)
{
  return makeUInt(32, isValue);
}


Type
Type::makeInt(int bitwidth, bool isValue)
{
  switch (bitwidth) {
  case  8: return makeTypeRef(Names::kInt8TypeName, isValue);
  case 16: return makeTypeRef(Names::kInt16TypeName, isValue);
  case 32: return makeTypeRef(Names::kInt32TypeName, isValue);
  case 64: return makeTypeRef(Names::kInt64TypeName, isValue);
  }

  hr_invalid("");
  return Type();
}


Type
Type::makeUInt(int bitwidth, bool isValue)
{
  switch (bitwidth) {
  case  8: return makeTypeRef(Names::kUInt8TypeName, isValue);
  case 16: return makeTypeRef(Names::kUInt16TypeName, isValue);
  case 32: return makeTypeRef(Names::kUInt32TypeName, isValue);
  case 64: return makeTypeRef(Names::kUInt64TypeName, isValue);
  }

  hr_invalid("");
  return Type();
}


Type
Type::makeRational(bool isValue)
{
  return makeTypeRef(Names::kRationalTypeName, isValue);
}


Type
Type::makeFloat32(bool isValue)
{
  return makeTypeRef(Names::kFloat32TypeName, isValue);
}


Type
Type::makeString(bool isValue)
{
  return makeTypeRef(Names::kStringTypeName, isValue);
}


Type
Type::makeBool(bool isValue)
{
  return makeTypeRef(Names::kBoolTypeName, isValue);
}


Type
Type::makeKeyword(bool isValue)
{
  return makeTypeRef(Names::kKeywordTypeName, isValue);
}


Type
Type::makeChar(bool isValue)
{
  return makeTypeRef(Names::kCharTypeName, isValue);
}


Type
Type::makeType(const String& name, const TypeVector& generics,
              const Type& inherit)
{
  return Type(kType_Type, K(isValue), !K(isImg),
              std::make_shared<TypeTypeImpl>(
                name, !K(isInstantiable), generics, inherit,
                FunctionSignature(), TypeSlotList()));
}


Type
Type::makeClass(const String& name, const TypeVector& generics,
               const Type& inherit, const FunctionSignature& applySign,
               const TypeSlotList& slots)
{
  return Type(kType_Class, K(isValue), !K(isImg),
              std::make_shared<TypeTypeImpl>(
                name, K(isInstantiable), generics, inherit, applySign,
                slots));
}


Type
Type::makeAlias(const String& name, const TypeVector& generics,
               const Type& isa)
{
  return Type(kType_Alias, K(isValue), !K(isImg),
              std::make_shared<AliasTypeImpl>(name, generics, isa));
}


Type
Type::makeMeasure(const String& name, const Type& baseType,
                 const String& defUnit)
{
  return Type(kType_Measure, K(isValue), !K(isImg),
              std::make_shared<MeasureTypeImpl>(name, baseType, defUnit));
}


Type
Type::makeFunction(const FunctionSignature& sign)
{
  return Type(kType_Function, K(isValue), !K(isImg),
              std::make_shared<FunctionTypeImpl>(sign));
}


Type
Type::makeUnion(const TypeVector& types, bool isValue)
{
  return Type(kType_Union, isValue, !K(isImg),
              std::make_shared<UnionTypeImpl>(types));
}


Type
Type::makeSeq(const TypeVector& types, bool isValue)
{
  return Type(kType_Sequence, isValue, !K(isImg),
              std::make_shared<SeqTypeImpl>(types));
}


Type
Type::clone() const
{
  return Type(fKind, fIsValue, fIsImaginary,
              (fImpl != nullptr ? fImpl->clone() : nullptr));
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

  hr_assert(fImpl);
  return fImpl->isEqual(other.fImpl.get());
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
    if (nm == Names::kEofTypeName ||
        nm == Names::kNilTypeName ||
        nm == Names::kRationalTypeName ||
        nm == Names::kStringTypeName)
      return true;

    const auto& prop = typeProperty(!K(mustExist));
    if (prop.isValid())
      return prop.isBaseType();
  }

  return false;
}


bool
Type::isPlainType() const
{
  if (isArray())
    return false;

  const auto& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.isPlainType();

  return false;
}


bool
Type::isBuiltinType(const String& name) const
{
  return typeName() == name;
}


std::unique_ptr<TypeEnumMaker>
Type::makeBaseTypeEnumMaker() const
{
  if (fKind == kType_Ref) {
    auto nm = typeName();
    if (nm == Names::kEofTypeName)
      return std::unique_ptr<TypeEnumMaker>(new EofTypeEnumMaker);
    else if (nm == Names::kNilTypeName)
      return std::unique_ptr<TypeEnumMaker>(new NilTypeEnumMaker);
    else if (nm == Names::kRationalTypeName)
      return std::unique_ptr<TypeEnumMaker>(new RationalTypeEnumMaker);
    else if (nm == Names::kStringTypeName)
      return std::unique_ptr<TypeEnumMaker>(new StringTypeEnumMaker);

    const auto& prop = typeProperty();
    if (prop.isValid())
      return prop.makeBaseTypeEnumMaker();
  }

  return nullptr;
}


const TypeProperty&
Type::typeProperty(bool mustExist) const
{
  static const InvalidTypeProperty   invalidProperty;
  static const Int8TypeProperty      int8Property;
  static const UInt8TypeProperty     uint8Property;
  static const Int16TypeProperty     int16Property;
  static const UInt16TypeProperty    uint16Property;
  static const Int32TypeProperty     int32Property;
  static const UInt32TypeProperty    uint32Property;
  static const Int64TypeProperty     int64Property;
  static const UInt64TypeProperty    uint64Property;
  static const BoolTypeProperty      boolProperty;
  static const Float32TypeProperty   float32Property;
  static const Float64TypeProperty   float64Property;
  static const Float128TypeProperty  float128Property;
  static const CharTypeProperty      charProperty;
  static const ClangCharTypeProperty clangCharProperty;
  static const ClangIntTypeProperty  clangIntProperty;
  static const KeywordTypeProperty   keywordProperty;
  static const StringTypeProperty    stringProperty;

  auto nm = typeName();
  if (nm == Names::kInt32TypeName)
    return int32Property;
  else if (nm == Names::kUInt32TypeName)
    return uint32Property;
  else if (nm == Names::kInt16TypeName)
    return int16Property;
  else if (nm == Names::kUInt16TypeName)
    return uint16Property;
  else if (nm == Names::kInt8TypeName)
    return int8Property;
  else if (nm == Names::kUInt8TypeName)
    return uint8Property;
  else if (nm == Names::kInt64TypeName)
    return int64Property;
  else if (nm == Names::kUInt64TypeName)
    return uint64Property;
  else if (nm == Names::kFloat32TypeName)
    return float32Property;
  else if (nm == Names::kFloat64TypeName)
    return float64Property;
  else if (nm == Names::kFloat128TypeName)
    return float128Property;

  else if (nm == Names::kBoolTypeName)
    return boolProperty;
  else if (nm == Names::kCharTypeName)
    return charProperty;

  else if (nm == Names::kKeywordTypeName)
    return keywordProperty;
  else if (nm == Names::kStringTypeName)
    return stringProperty;

  else if (nm == Names::kClangCharTypeName)
    return clangCharProperty;
  else if (nm == Names::kClangIntTypeName)
    return clangIntProperty;

  if (mustExist) {
    hr_invalid((zstring)StrHelper(String("unhandled type: ") + typeId()));
  }
  return invalidProperty;
}


bool
Type::isAny() const
{
  return isBuiltinType(Names::kAnyTypeName);
}


bool
Type::isClangAtom() const
{
  return isBuiltinType(Names::kClangAtomTypeName);
}


bool
Type::isSigned() const
{
  if ( isBuiltinType(Names::kNumberTypeName) ||
       isBuiltinType(Names::kComplexTypeName) ||
       isBuiltinType(Names::kRationalTypeName) ||
       isBuiltinType(Names::kIntegerTypeName))
    return true;

  const TypeProperty& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.isSigned();
  return false;
}


bool
Type::isAnyNumber() const
{
  if ( isBuiltinType(Names::kNumberTypeName) ||
       isBuiltinType(Names::kComplexTypeName) ||
       isBuiltinType(Names::kRationalTypeName) ||
       isBuiltinType(Names::kIntegerTypeName))
    return true;

  const auto& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.isAnyNumber();
  return false;
}


bool
Type::isInteger() const
{
  if ( isBuiltinType(Names::kIntegerTypeName))
    return true;
  return false;
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
  const auto& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.isAnyFloat();
  return false;
}


bool
Type::isAnyInt() const
{
  return isAnySignedInt() || isAnyUInt();
}


bool
Type::isAnySignedInt() const
{
  const auto& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return prop.isSigned() && prop.isAnyInt();
  return false;
}


bool
Type::isAnyUInt() const
{
  // if ( isBuiltinType(Names::kIntegerTypeName))
  //   return true;

  const auto& prop = typeProperty(!K(mustExist));
  if (prop.isValid())
    return !prop.isSigned() && prop.isAnyInt();
  return false;
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
    return std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl)->name();

  case kType_Array:
    return arrayBaseType().typeName();
  case kType_Class:
  case kType_Type:
    return std::dynamic_pointer_cast<TypeTypeImpl>(fImpl)->name();
  case kType_Alias:
    return std::dynamic_pointer_cast<AliasTypeImpl>(fImpl)->name();

  case kType_Measure:
    return std::dynamic_pointer_cast<MeasureTypeImpl>(fImpl)->name();
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
    return String("<undefined>");

  case kType_Ref:
    {
      auto tyimpl = std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl);
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
      auto tyimpl = std::dynamic_pointer_cast<TypeTypeImpl>(fImpl);
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
    return std::dynamic_pointer_cast<AliasTypeImpl>(fImpl)->name();

  case kType_Measure:
    buffer << std::dynamic_pointer_cast<MeasureTypeImpl>(fImpl)->name()
           << std::dynamic_pointer_cast<MeasureTypeImpl>(fImpl)->defUnit();
    return buffer.toString();

  case kType_Union:
    buffer << "&(" << std::dynamic_pointer_cast<UnionTypeImpl>(fImpl)->types() << ")";
    return buffer.toString();

  case kType_Sequence:
    buffer << "(" << std::dynamic_pointer_cast<SeqTypeImpl>(fImpl)->types() << ")";
    return buffer.toString();

  case kType_Function:
    return std::dynamic_pointer_cast<FunctionTypeImpl>(fImpl)->functionSignature().typeId();
  }

  return String();
}


bool
Type::isClass() const
{
  return fKind == kType_Class;
}


const TypeSlotList&
Type::slots() const
{
  hr_assert(isClass());
  return std::dynamic_pointer_cast<TypeTypeImpl>(fImpl)->slots();
}


Type
Type::slotType(const String& slotName, const Scope& scope) const
{
  hr_assert(isClass());
  auto tyimpl = std::dynamic_pointer_cast<TypeTypeImpl>(fImpl);

  for (const auto& slot : tyimpl->slots()) {
    if (slot.name() == slotName)
      return ( slot.type().isDef()
               ? slot.type()
               : Type::makeAny() );
  }

  auto inherits = typeInheritance();
  if (inherits.isSequence()) {
    const auto& inheritedTypes = inherits.seqTypes();
    for (const auto& ty : inheritedTypes) {
      auto normalizedType = herschel::resolveType(ty, scope);

      if (normalizedType.isClass()) {
        auto sty = normalizedType.slotType(slotName, scope);
        if (sty.isDef())
          return sty;
      }
    }
  }
  return Type();
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
  return std::dynamic_pointer_cast<TypeTypeImpl>(fImpl)->inherit();
}


const FunctionSignature&
Type::applySignature() const
{
  hr_assert(isClass());
  return std::dynamic_pointer_cast<TypeTypeImpl>(fImpl)->applySignature();
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
  return std::dynamic_pointer_cast<AliasTypeImpl>(fImpl)->inherit();
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
  return std::dynamic_pointer_cast<FunctionTypeImpl>(fImpl)->functionSignature();
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
  return std::dynamic_pointer_cast<ArrayTypeImpl>(fImpl)->baseType();
}


Type
Type::arrayRootType() const
{
  auto ty = *this;
  while (ty.isArray())
    ty = ty.arrayBaseType();
  return ty;
}


Type
Type::rebase(const Type& newBaseType) const
{
  if (arrayBaseType().isArray())
    return Type::makeArray(arrayBaseType().rebase(newBaseType),
                          arraySizeIndicator(),
                          arrayBaseType().isValueType());

  return newBaseType;
}


int
Type::arraySizeIndicator() const
{
  hr_assert(isArray());
  return std::dynamic_pointer_cast<ArrayTypeImpl>(fImpl)->sizeIndicator();
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
  return std::dynamic_pointer_cast<UnionTypeImpl>(fImpl)->types();
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
  return std::dynamic_pointer_cast<SeqTypeImpl>(fImpl)->types();
}


bool
Type::containsType(const Type& type) const
{
  if (isSequence())
    return std::dynamic_pointer_cast<SeqTypeImpl>(fImpl)->containsType(type);
  else if (isUnion())
    return std::dynamic_pointer_cast<UnionTypeImpl>(fImpl)->containsType(type);

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
  return std::dynamic_pointer_cast<MeasureTypeImpl>(fImpl)->inherit();
}


String
Type::measureUnit() const
{
  hr_assert(isMeasure());
  return std::dynamic_pointer_cast<MeasureTypeImpl>(fImpl)->defUnit();
}


bool
Type::hasConstraints() const
{
  if (fKind == kType_Ref)
    return ( !std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl)
             ->constraints().empty() );

  return false;
}


const TypeConstVector&
Type::constraints() const
{
  if (fKind == kType_Ref)
    return std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl)->constraints();

  static TypeConstVector dummy;
  return dummy;
}


Type
Type::setConstraints(const TypeConstVector& newConstraints) const
{
  if (fKind == kType_Ref) {
    auto clonedTy = Type::makeTypeRef(typeName(),
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
  return (fImpl && fImpl->isOpen());
}


bool
Type::isOpenSelf() const
{
  return fKind == kType_Ref && fImpl && fImpl->isOpenSelf();
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
    return std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl)->generics();
  case kType_Class:
  case kType_Type:
    return std::dynamic_pointer_cast<TypeTypeImpl>(fImpl)->generics();
  case kType_Alias:
    return std::dynamic_pointer_cast<AliasTypeImpl>(fImpl)->generics();

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
    if (std::dynamic_pointer_cast<TypeRefTypeImpl>(fImpl)->isOpen()) {
      auto replacement = typeMap.lookupType(typeName());
      if (replacement.isDef()) {
        if (replacement.hasConstraints()) {
          // if (!constraints().empty())
          //   throw TypeConstraintsConflictException(
          //     *this,
          //     String("type parameter constraints conflict "
          //            "with generics constraints"));
          TypeConstVector allConstraints;
          allConstraints.insert(allConstraints.begin(), constraints().begin(), constraints().end());
          allConstraints.insert(allConstraints.end(), replacement.constraints().begin(),
                                replacement.constraints().end());

          clonedTy = Type::makeTypeRef(replacement.typeName(),
                                      replacement.generics(),
                                      allConstraints,
                                      replacement.isValueType());
          //clonedTy = replacement;
          clonedTy.setIsImaginary(fIsImaginary);
        }
        else if (hasConstraints()) {
          if (replacement.isRef() || replacement.isClass() || replacement.isType()) {
            clonedTy = Type::makeTypeRef(replacement.typeName(),
                                        replacement.generics(),
                                        constraints(),
                                        replacement.isValueType());
            clonedTy.setIsImaginary(fIsImaginary);
          }
          else
            throw TypeConstraintsConflictException(
              *this,
              String("Constraints for non trivial type reference"));
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
                    const Scope& scope, const SrcPos& srcpos) const
{
  if (fImpl)
    return fImpl->matchGenerics(localCtx, right0, scope, srcpos);
  return false;
}


//----------------------------------------------------------------------------

TypeSlot::TypeSlot(const String& name, const Type& type, unsigned int flags)
  : fName(name),
    fType(type),
    fFlags(flags)
{
}


TypeSlot::TypeSlot(const TypeSlot& other)
  : fName(other.fName),
    fType(other.fType),
    fFlags(other.fFlags)
{
}


TypeSlot
TypeSlot::clone() const
{
  return TypeSlot(fName, fType.clone(), fFlags);
}


//! Assign operator
TypeSlot&
TypeSlot::operator=(const TypeSlot& other)
{
  fName = other.fName;
  fType = other.fType;
  fFlags = other.fFlags;

  return *this;
}


//! Compare operator.
bool
TypeSlot::operator==(const TypeSlot& other) const
{
  return fName == other.fName && fType == other.fType && fFlags == other.fFlags;
}


//! Compare operator
bool
TypeSlot::operator!=(const TypeSlot& other) const
{
  return !(operator==(other));
}


void
TypeSlot::replaceGenerics(const TypeCtx& typeMap)
{
  fType = fType.replaceGenerics(typeMap);
}


namespace herschel
{
  static String flagsToStr(unsigned int flags)
  {
    StringBuffer buf;
    if ((flags & kTransientSlot) != 0)
      buf << "transient ";
    if ((flags & kReadonlySlot) != 0)
      buf << "readonly ";

    if ((flags & kPublicSlot) != 0)
      buf << "public ";
    else if ((flags & kOuterSlot) != 0)
      buf << "outer ";
    else if ((flags & kInnerSlot) != 0)
      buf << "inner ";

    if ((flags & kAutoSlot) != 0)
      buf << "auto ";

    return buf.toString();
  }
};


String
TypeSlot::toString() const
{
  StringBuffer buf;
  buf << "<ty:slot k='" << flagsToStr(fFlags) << "' "
      << "nm='" << fName << "'>"
      << fType.toString()
      << "</ty:slot>\n";
  return buf.toString();
}


String
TypeSlot::name() const
{
  return fName;
}


Type
TypeSlot::type() const
{
  return fType;
}


unsigned int
TypeSlot::flags() const
{
  return fFlags;
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


    std::shared_ptr<BaseTypeConstraintImpl> clone() const override
    {
      return std::make_shared<LogicalConstraintImpl>(fOp, fLeft.clone(),
                                                     fRight.clone());
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      fLeft = fLeft.replaceGenerics(typeMap);
      fRight = fRight.replaceGenerics(typeMap);
    }


    bool isEqual(const BaseTypeConstraintImpl* other) const override
    {
      auto c = dynamic_cast<const LogicalConstraintImpl*>(other);

      return (c &&
              fOp == c->fOp &&
              fLeft == c->fLeft &&
              fRight == c->fRight);
    }


    TypeConstOperator constOp() const override
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


    zstring optostr(TypeConstOperator op) const
    {
      switch (op) {
      case kConstOp_and:           return "and";
      case kConstOp_or:            return "or";
      default:
        hr_invalid("");
      }
      return "??";
    }


    String toString() const override
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


    std::shared_ptr<BaseTypeConstraintImpl> clone() const override
    {
      return std::make_shared<ValueConstraintImpl>(fOp, fValue);
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      // NOP
    }


    bool isEqual(const BaseTypeConstraintImpl* other) const override
    {
      auto c = dynamic_cast<const ValueConstraintImpl*>(other);

      return (c &&
              fOp == c->fOp &&
              fValue == c->fValue);
    }


    TypeConstOperator constOp() const override
    {
      return fOp;
    }


    const Token& token() const
    {
      return fValue;
    }


    zstring optostr(TypeConstOperator op) const
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


    String toString() const override
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


    std::shared_ptr<BaseTypeConstraintImpl> clone() const override
    {
      return std::make_shared<TypeConstraintImpl>(fOp, fType.clone());
    }


    void replaceGenerics(const TypeCtx& typeMap) override
    {
      fType = fType.replaceGenerics(typeMap);
    }


    bool isEqual(const BaseTypeConstraintImpl* other) const override
    {
      auto c = dynamic_cast<const TypeConstraintImpl*>(other);

      return (c && fType == c->fType);
    }


    TypeConstOperator constOp() const override
    {
      return fOp;
    }


    const Type& type() const
    {
      return fType;
    }


    String toString() const override
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

TypeConstraint::TypeConstraint(std::shared_ptr<BaseTypeConstraintImpl> impl)
  : fImpl(std::move(impl))
{ }


TypeConstraint::TypeConstraint(const TypeConstraint& other)
{
  *this = other;
}


TypeConstraint
TypeConstraint::makeAnd(const TypeConstraint& left,
                       const TypeConstraint& right)
{
  return TypeConstraint(
    std::make_shared<LogicalConstraintImpl>(kConstOp_and, left, right));
}


TypeConstraint
TypeConstraint::makeOr(const TypeConstraint& left,
                      const TypeConstraint& right)
{
  return TypeConstraint(
    std::make_shared<LogicalConstraintImpl>(kConstOp_or, left, right));
}


TypeConstraint
TypeConstraint::makeValue(TypeConstOperator op, const Token& value)
{
  return TypeConstraint(
    std::make_shared<ValueConstraintImpl>(op, value));
}


TypeConstraint
TypeConstraint::makeType(TypeConstOperator op, const Type& type)
{
  return TypeConstraint(std::make_shared<TypeConstraintImpl>(op, type));
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
    return fImpl->isEqual(other.fImpl.get());
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
  return std::dynamic_pointer_cast<ValueConstraintImpl>(fImpl)->token();
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
  return std::dynamic_pointer_cast<LogicalConstraintImpl>(fImpl)->left();
}


const TypeConstraint&
TypeConstraint::rightConstraint() const
{
  hr_assert(isLogicalConstraint());
  return std::dynamic_pointer_cast<LogicalConstraintImpl>(fImpl)->right();
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
  return std::dynamic_pointer_cast<TypeConstraintImpl>(fImpl)->type();
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
FunctionParameter::makePosParam(const Type& type)
{
  return FunctionParameter(kParamPos, !K(isSpec), String(), type);
}


FunctionParameter
FunctionParameter::makeSpecParam(const Type& type)
{
  return FunctionParameter(kParamPos, K(isSpec), String(), type);
}


FunctionParameter
FunctionParameter::makeNamedParam(const String& key, const Type& type)
{
  return FunctionParameter(kParamNamed, !K(isSpec), key, type);
}


FunctionParameter
FunctionParameter::makeRestParam(const Type& type)
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

  return std::any_of(fParameters.begin(), fParameters.end(),
                     [](const FunctionParameter& p) {
                       return p.type().isOpen();
                     });
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
                                 const Scope& scope, const SrcPos& srcpos) const
{
  if (fParameters.size() == right0.parameters().size()) {
    if (!fReturnType.matchGenerics(localCtx, right0.returnType(),
                                   scope, srcpos))
      return false;
    for (size_t i = 0; i < fParameters.size(); ++i) {
      const auto& lparam = fParameters[i];
      const auto& rparam = right0.parameters()[i];

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
    for (const auto& p : fParameters)
      buf << p.toString();
    buf << "</ty:prms>\n";
  }

  buf << "<ty:ret>\n" << fReturnType.toString() << "</ty:ret>\n";
  buf << "</ty:fun>\n";
  return buf.toString();
}


bool
FunctionSignature::hasPositionalParam() const
{
  return std::any_of(fParameters.begin(), fParameters.end(),
                     [](const FunctionParameter& p) {
                       return p.kind() == FunctionParameter::kParamPos;
                     });
}


namespace herschel
{
  StringBuffer&
  operator<<(StringBuffer& other, const FunctionParamVector& params)
  {
    if (!params.empty()) {
      other << params.front().type().typeId();
      std::for_each(std::next(params.begin()), params.end(),
                    [&](const FunctionParameter& p) {
                      other << ", ";
                      other << p.type().typeId();
                    });
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
  resolveType(const Type& type, const Scope& scope)
  {
    Type ty = ( type.isDef() && type.isRef()
                ? scope.lookupType(type.typeName(), K(showAmbiguousSymDef))
                : type );
    if (ty.isDef() && ty.isOpen()) {
      if (type.isDef())
        return scope.normalizeType(ty, type);
    }
    return ty;
  }


  bool
  isSameType(const TypeVector& vect0, const TypeVector& vect1,
             const Scope& scope, const SrcPos& srcpos, bool reportErrors)
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
             const Scope& scope, const SrcPos& srcpos, bool reportErrors)
  {
    if (!isSameType(leftsig.returnType(), rightsig.returnType(),
                    scope, srcpos, reportErrors))
      return false;
    if (leftsig.parameters().size() != rightsig.parameters().size())
      return false;

    for (size_t i = 0; i < leftsig.parameters().size(); i++) {
      const auto& leftprm = leftsig.parameters()[i];
      const auto& rightprm = rightsig.parameters()[i];

      if (leftprm.kind() != rightprm.kind() ||
          !isSameType(leftprm.type(), rightprm.type(), scope, srcpos,
                      reportErrors))
        return false;
    }
    return true;
  }


  bool
  isSameType(const Type& left0, const Type& right0, const Scope& scope,
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
                 (zstring)StrHelper(right0.typeId()), __FILE__, __LINE__);
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
                 (zstring)StrHelper(left0.typeId()), __FILE__, __LINE__);
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
               (zstring)StrHelper(left0.typeId()), __FILE__, __LINE__);
      return false;
    }
    if (!right.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType,
               "Undefined type: '%s' (%s:%d)",
               (zstring)StrHelper(right0.typeId()), __FILE__, __LINE__);
      return false;
    }

    if (left.isAny() || left.isClangAtom()) {
      if (right.isAny() || right.isClangAtom())
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

    fprintf(stderr, "LEFT: %s\n", (zstring)StrHelper(left.toString()));
    fprintf(stderr, "RIGHT: %s\n", (zstring)StrHelper(right.toString()));
    hr_invalid("unhandled type?");
    return false;
  }


  //! Indicates whether left0 is a subtype of right0.  This is tested by checking
  //! whether right0 is in left0's inheritance list.
  bool
  inheritsFrom(const Type& left0, const Type& right0, const Scope& scope,
               const SrcPos& srcpos, bool reportErrors)
  {
    if (!left0.isDef() || !right0.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    auto left = resolveType(left0, scope);
    auto right = resolveType(right0, scope);

    if (!left.isDef() || !right.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    Type inheritance;
    if (left.isType() || left.isClass())
      inheritance = left.typeInheritance();
    else if (left.isMeasure())
      inheritance = left.measureBaseType();
    else
      return false;

    if (!inheritance.isDef())
      return false;
    else if (inheritance.isRef())
      inheritance = scope.lookupType(inheritance);

    if (!inheritance.isDef()) {
      if (reportErrors)
        errorf(srcpos, E_UndefinedType, "Undefined type (%s:%d)", __FILE__, __LINE__);
      return false;
    }

    if (inheritance.isType() || inheritance.isClass()) {
      if (isSameType(inheritance, right, scope, srcpos, reportErrors))
        return true;

      if (right.isOpen()) {
        TypeCtx localCtx;
        if (inheritance.matchGenerics(localCtx, right, scope, srcpos))
          return true;
      }
      return inheritsFrom(inheritance, right, scope, srcpos, reportErrors);
    }

    if (inheritance.isSequence()) {
      return std::any_of(inheritance.seqTypes().begin(),
                         inheritance.seqTypes().end(),
                         [&](const Type& t) {
                           return isSameType(t, right, scope, srcpos, reportErrors)
                             || inheritsFrom(t, right, scope, srcpos, reportErrors);
                         });
    }

    hr_invalid("unexpected type kind");
    return false;
  }


  //----------------------------------------------------------------------------

  bool
  isCovariant(const TypeVector& vect0, const TypeVector& vect1,
              const Scope& scope, const SrcPos& srcpos, bool reportErrors)
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
                              const Scope& scope,
                              const SrcPos& srcpos, bool reportErrors)
  {
    for (const auto& ty : vect0) {
      if (!isCovariant(type, ty, scope, srcpos, reportErrors))
        return false;
    }
    return true;
  }


  bool
  isCoOrInvariantToEveryTypeInUnion(const Type& type, const TypeVector& vect0,
                                    const Scope& scope,
                                    const SrcPos& srcpos, bool reportErrors)
  {
    auto hadOneCovariantType = false;
    for (const auto& ty : vect0) {
      if (isContravariant(type, ty, scope, srcpos, reportErrors) &&
          !isSameType(type, ty, scope, srcpos, reportErrors))
        return false;
      if (!hadOneCovariantType &&
          isCovariant(type, ty, scope, srcpos, reportErrors))
        hadOneCovariantType = true;
    }
    return hadOneCovariantType;
  }


  bool
  isCovariantForAllTypesInUnion(const TypeVector& vect0, const TypeVector& vect1,
                                const Scope& scope,
                                const SrcPos& srcpos, bool reportErrors)
  {
    for (const auto& ty : vect0) {
      if (!isCoOrInvariantToEveryTypeInUnion(ty, vect1, scope,
                                             srcpos, reportErrors))
        return false;
    }
    return true;
  }


  bool
  isCovariant(const FunctionSignature& leftsig,
              const FunctionSignature& rightsig,
              const Scope& scope, const SrcPos& srcpos, bool reportErrors)
  {
    if (!isCovariant(leftsig.returnType(), rightsig.returnType(),
                     scope, srcpos, reportErrors))
    {
      //tyerror(leftsig.returnType(), "leftsig returntype");
      //tyerror(rightsig.returnType(), "rightsig returntype");
      return false;
    }

    if (leftsig.parameters().size() != rightsig.parameters().size())
      return false;

    TypeCtx localCtx;

    for (size_t i = 0; i < leftsig.parameters().size(); i++) {
      const auto& leftprm = leftsig.parameters()[i];
      const auto& rightprm = rightsig.parameters()[i];

      if (leftprm.kind() == rightprm.kind()) {
        if (leftprm.isSpecialized() && rightprm.isSpecialized()) {
          if (!isCovariant(leftprm.type(), rightprm.type(), scope, srcpos,
                           reportErrors))
          {
            // tyerror(leftprm.type(), "leftprm type");
            // tyerror(rightprm.type(), "rightprm type");
            return false;
          }
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
          {
            return false;
          }
        }

        if (rightprm.type().isOpenSelf()) {
          auto genName = rightprm.type().typeName();

          auto knownType = localCtx.lookupType(genName);
          if (knownType.isDef()) {
            if (!isContravariant(leftprm.type(), knownType, scope, srcpos,
                                 reportErrors))
            {
              return false;
            }
          }

          if (!localCtx.hasType(genName)) {
            localCtx.registerType(genName, leftprm.type());
          }
          else {
            // TODO
          }
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

    if (left.isAny() || left.isClangAtom())
      return true;
    if (left.isUnion()) {
      const auto& vect = left.unionTypes();
      for (const auto& ty : vect) {
        if (containsAny(ty, srcpos, reportErrors))
          return true;
      }
    }

    return false;
  }


  bool
  isCovariant(const Type& left0, const Type& right0, const Scope& scope,
              const SrcPos& srcpos, bool reportErrors)
  {
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
      if (isCovariant(right, Type::makeAny(), scope, srcpos, reportErrors)) {
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

      if (isCovariant(left, Type::makeAny(), scope, srcpos, reportErrors)) {
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

    if (right.isAny() || right.isClangAtom()) {
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
      auto rightTypeName = right.typeName();
      if (right.isType() && (rightTypeName == Names::kSliceableTypeName ||
                             rightTypeName == Names::kSliceableXTypeName) &&
          right.generics().size() == 2 &&
          isSameType(right.generics()[0], Type::makeUInt32(), scope,
                     srcpos, reportErrors) &&
          isSameType(left.arrayBaseType(), right.generics()[1],
                     scope, srcpos, reportErrors))
      {
        return true;
      }
      else if (right.isArray() && right.arrayBaseType().isOpenSelf())
      {
        // a generic open type is covariant to Any.  This needs special treatment
        // in the compiler though
        return isCovariant(left.arrayBaseType(), Type::makeAny(), scope, srcpos, reportErrors);
      }
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
      if (left.isOpen()) {
        TypeCtx localCtx;
        if (left.matchGenerics(localCtx, right, scope, srcpos))
          return true;
      }
      if (right.isOpen()) {
        TypeCtx localCtx;
        if (right.matchGenerics(localCtx, left, scope, srcpos))
          return true;
      }
      if (isSameType(left, right, scope, srcpos, reportErrors))
        return true;

      if (right.isType() || right.isClass()) {
        if (!inheritsFrom(left, right, scope, srcpos, reportErrors))
          return false;
        if (left.hasGenerics() && right.hasGenerics())
          return isSameType(left.generics(), right.generics(), scope, srcpos,
                            reportErrors);
        return true;
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
    else if (left.isAny() || left.isClangAtom()) {
      return false;
    }

    tyerror(left, "LEFT");
    tyerror(right, "RIGHT");
    hr_invalid("unhandled type?");
    return false;
  }


  bool
  isContravariant(const Type& left, const Type& right, const Scope& scope,
                  const SrcPos& srcpos, bool reportErrors)
  {
    return isCovariant(right, left,
                       scope, srcpos, reportErrors);
  }


  bool
  isInvariant(const Type& left, const Type& right, const Scope& scope,
              const SrcPos& srcpos, bool reportErrors)
  {
    return ( !isCovariant(left, right, scope, srcpos, reportErrors) &&
             !isContravariant(left, right, scope, srcpos, reportErrors) );
  }
};                              // namespace herschel


//----------------------------------------------------------------------------

namespace herschel
{
  Type
  makeRangeType(const Type& generic)
  {
    return Type::makeType(Names::kRangeTypeName, makeVector(generic), Type());
  }


  void
  tyerror(const Type& type, zstring msg)
  {
    fprintf(stderr, "%s: %s\n", msg, (zstring)StrHelper(type.typeId()));
  }


  int
  floatTypeBitsize(const Type& ty)
  {
    return ty.typeProperty().typeBitsize();
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
    return ty.typeProperty().typeBitsize();
  }


  Type
  maxIntType(const Type& leftty, const Type& rightty)
  {
    auto righttysize = intTypeBitsize(rightty);
    if (intTypeBitsize(leftty) < righttysize) {
      if (leftty.isAnyUInt()) {
        switch (righttysize) {
        case 8:
          return Type::makeTypeRef(Names::kUInt8TypeName, K(isValue));
        case 16:
          return Type::makeTypeRef(Names::kUInt16TypeName, K(isValue));
        case 32:
          return Type::makeTypeRef(Names::kUInt32TypeName, K(isValue));
        case 64:
          return Type::makeTypeRef(Names::kUInt64TypeName, K(isValue));
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
          auto gen = type.generics()[i];
          hr_assert(gen.isRef());

          auto genName = gen.typeName();
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


  String
  arrayTypeName(const String& baseName)
  {
    return baseName + "[]";
  }


  String
  arrayTypeName(zstring baseName)
  {
    return arrayTypeName(String(baseName));
  }
};
