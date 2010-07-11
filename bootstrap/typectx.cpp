/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "typectx.h"
#include "strbuf.h"


using namespace heather;

static Type sInvalidType;


TypeCtx::TypeCtx()
{ }


TypeCtx::TypeCtx(TypeCtx* parent)
 : fParent(parent)
{
}


TypeCtx*
TypeCtx::parent() const
{
  return fParent;
}


void
TypeCtx::registerType(const String& name, const Type& type)
{
  assert(fMap.find(name) == fMap.end());
  // assert(!type.isRef());
  assert(!type.isArray());
  assert(type.isDef());

  fMap.insert(std::make_pair(name, type));
}


bool
TypeCtx::hasType(const String& name) const
{
  return fMap.find(name) != fMap.end();
}


const Type&
TypeCtx::lookupType(const String& name) const
{
  const Type& type = lookupTypeLocal(name);
  if (type.isDef())
    return type;

  if (fParent != NULL)
    return fParent->lookupType(name);

  return type;
}


Type
TypeCtx::normalizeType(const Type& type, const Type& refType) const
{
  Type baseType;
  Type returnType = type;

  if (type.isArray()) {
    baseType = type.arrayRootType();
  }
  else if (type.isRef()) {
    baseType = type;
  }
  else if (type.isAlias()) {
    baseType = type.aliasReplaces();
    returnType = baseType;
  }

  assert(baseType.isDef());

  if (!type.generics().size() == refType.generics().size())
    throw TypeRefMatchException(refType,
                                ( StringBuffer()
                                  << "Requires "
                                  << fromInt(type.generics().size())
                                  << " type arguments, found "
                                  << fromInt(refType.generics().size())).toString());

  if (type.hasGenerics()) {
    TypeCtx localCtx;
    for (size_t i = 0; i < type.generics().size(); i++) {
      Type gen = type.generics()[i];
      assert(gen.isRef());

      String genName = gen.typeName();
      localCtx.registerType(genName, refType.generics()[i]);
    }

    returnType = baseType.replaceGenerics(localCtx);
  }

  if (refType.isArray())
    return refType.rebase(returnType);

  return returnType;
}


Type
TypeCtx::lookupType(const Type& type) const
{
  Type baseType;
  if (type.isArray()) {
    baseType = type.arrayRootType();
  }
  else if (type.isRef()) {
    baseType = type;
  }

  if (baseType.isDef()) {
    Type fullType = lookupTypeLocal(baseType.typeName());
    if (fullType.isDef()) {
      return normalizeType(fullType, type);
    }

    if (fParent != NULL)
      return fParent->lookupType(type);
  }

  return type;
}


const Type&
TypeCtx::lookupTypeLocal(const String& name) const
{
  TypeMap::const_iterator it = fMap.find(name);
  if (it != fMap.end())
    return it->second;

  return sInvalidType;
}
