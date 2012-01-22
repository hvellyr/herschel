/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_refcountable_h
#define bootstrap_refcountable_h

#include "common.h"
#include "require.h"

//----------------------------------------------------------------------------

namespace herschel
{
  //! Base class for reference counted objects.
  //!
  //! Classes derived from RefCountable can automatically be maintained by \c
  //! Ptr.  When created a reference counted object has a reference count of
  //! \c 0.  Increasing and decreasing the reference count with \c incRef()
  //! and \c decRef() automatically prevents the object from deletion or
  //! deletes it when the reference count reaches \c 0.
  //!
  //! Cf. \c herschel::Ptr
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

    //! Returns the object's reference count.
    int refCount() const
    {
      return fRefCount;
    }

    //! Increase the object's reference count.
    virtual void incRef()
    {
      fRefCount++;
    }

    //! Decreases the object's reference count.  The reference count \e must
    //! be larger \c 0.  Even if the reference count falls to \c 0, the object
    //! is not deleted.
    //!
    //! This function is to be used to release the object from reference
    //! control.
    virtual void decRefWithoutDelete()
    {
      hr_assert(fRefCount > 0);
      fRefCount--;
    }


    //! Decreases the object's reference count.  The reference count \e must
    //! be larger \c 0.  If the reference count falls \c 0 the object is
    //! deleted.
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
