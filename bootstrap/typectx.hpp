/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include <map>

#include "exception.hpp"
#include "str.hpp"
#include "type.hpp"


namespace herschel {
class TypeRefMatchException : public Exception {
public:
  TypeRefMatchException(const Type& type, const String& str)
      : Exception(str)
  {
  }
};


class TypeConstraintsConflictException : public Exception {
public:
  TypeConstraintsConflictException(const Type& type, const String& str)
      : Exception(str)
  {
  }
};


class TypeCtx {
public:
  void registerType(const String& name, const Type& type);
  bool hasType(const String& name) const;
  const Type& lookupType(const String& name) const;

  void dumpDebug();

private:
  const Type& lookupTypeLocal(const String& name) const;

  //-------- data members

  using TypeMap = std::map<String, Type>;
  TypeMap fMap;
};

}  // namespace herschel
