/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <map>

#include "refcountable.h"
#include "str.h"

namespace herschel
{
  //--------------------------------------------------------------------------

  //! A typesafe registry.
  //!
  //! A registry is a heap (and refcounted) dictionary mapping string values
  //! to a templated type.  Values are added (set) using \c registerValue()
  //! and looked up with \c lookup().
  template<typename T>
  class Registry : public RefCountable
  {
  public:
    using ValueType = T;
    using ValueMapType = typename std::map<String, ValueType>;
    using ValueMapTypeIterator = typename std::map<String, ValueType>::iterator;
    using ValueMapTypeConstIterator = typename std::map<String, ValueType>::const_iterator;

    //! Bind a new value \p value to key \p name.  If \p name was bound before
    //! the previous bound value is silently dropped.
    virtual void registerValue(const String& name, ValueType value)
    {
      ValueMapTypeIterator it = fMap.find(name);
      if (it == fMap.end())
        fMap.insert(std::make_pair(name, value));
      else
        it->second = value;
    }

    //! Look up the value for \p name.  If \p name is bound returns \c true
    //! and pass the value out in \p out.
    virtual bool lookup(const String& name, ValueType* out) const
    {
      ValueMapTypeConstIterator it = fMap.find(name);
      if (it != fMap.end()) {
        *out = it->second;
        return true;
      }
      return false;
    }

  private:
    ValueMapType fMap;
  };

}
