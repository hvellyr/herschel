/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "typectx.hpp"

#include "strbuf.hpp"


using namespace herschel;

static Type sInvalidType;

void TypeCtx::registerType(const String& name, const Type& type)
{
  hr_assert(fMap.find(name) == fMap.end());
  // hr_assert(!type.isRef());
  // hr_assert(!type.isArray());
  hr_assert(type.isDef());

  fMap.insert(std::make_pair(name, type));
}


bool TypeCtx::hasType(const String& name) const
{
  return fMap.find(name) != fMap.end();
}


const Type& TypeCtx::lookupType(const String& name) const
{
  const Type& type = lookupTypeLocal(name);
  if (type.isDef())
    return type;

  return type;
}


const Type& TypeCtx::lookupTypeLocal(const String& name) const
{
  TypeMap::const_iterator it = fMap.find(name);
  if (it != fMap.end())
    return it->second;

  return sInvalidType;
}


void TypeCtx::dumpDebug()
{
  for (TypeMap::const_iterator it = fMap.begin(), e = fMap.end(); it != e; ++it) {
    fprintf(stderr, "TYPECTX: %s -> %s\n", (zstring)StrHelper(it->first),
            (zstring)StrHelper(it->second.toString()));
  }
}
