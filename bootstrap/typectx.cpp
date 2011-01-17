/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "typectx.h"
#include "strbuf.h"


using namespace herschel;

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
//  assert(!type.isArray());
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


const Type&
TypeCtx::lookupTypeLocal(const String& name) const
{
  TypeMap::const_iterator it = fMap.find(name);
  if (it != fMap.end())
    return it->second;

  return sInvalidType;
}


void
TypeCtx::dumpDebug()
{
  for (TypeMap::const_iterator it = fMap.begin(), e = fMap.end();
       it != e; ++it)
  {
    fprintf(stderr, "TYPECTX: %s -> %s\n",
            (const char*)StrHelper(it->first),
            (const char*)StrHelper(it->second.toString()));
  }
}
