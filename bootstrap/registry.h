/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_registry_h
#define bootstrap_registry_h

#include <map>

#include "refcountable.h"
#include "str.h"

namespace heather
{
  //--------------------------------------------------------------------------

  template<typename T>
  class Registry : public RefCountable
  {
  public:
    typedef T ValueType;
    typedef typename std::map<String, ValueType> ValueMapType;
    typedef typename std::map<String, ValueType>::iterator ValueMapTypeIterator;
    typedef typename std::map<String, ValueType>::const_iterator ValueMapTypeConstIterator;

    virtual void registerValue(const String& name, ValueType value)
    {
      ValueMapTypeIterator it = fMap.find(name);
      if (it == fMap.end())
        fMap.insert(std::make_pair(name, value));
      else
        it->second = value;
    }

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

};


#endif  // bootstrap_registry_h
