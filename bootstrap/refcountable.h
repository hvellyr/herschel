/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_refcountable_h
#define bootstrap_refcountable_h

#include "common.h"
#include "require.h"

//----------------------------------------------------------------------------

namespace herschel
{

//----------------------------------------------------------------------------
  class RefCountable
  {
  public:
    RefCountable()
      : fRefCount(0)
    {
    }

    virtual ~RefCountable()
    {
    }

    int refCount() const
    {
      return fRefCount;
    }


    virtual void incRef()
    {
      fRefCount++;
    }


    virtual void decRefWithoutDelete()
    {
      hr_assert(fRefCount > 0);
      fRefCount--;
    }


    virtual void decRef()
    {
      hr_assert(fRefCount > 0);
      fRefCount--;

      if (fRefCount == 0) {
        delete this;
      }
    }

  protected:
    int fRefCount;
  };

};                              // namespace

#endif                          // bootstrap_refcountable_h
