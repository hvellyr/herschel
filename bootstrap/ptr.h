/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_ptr_h
#define bootstrap_ptr_h

#include "common.h"

#include <stdlib.h>


namespace herschel
{
  //! A ref-counting hub for RefCountable objects.  Ptr<> and RefCountable
  //! form the basic of a simple ref-counted memory mechanism.  Always keep
  //! RefCountable objects in Ptr<> variables to avoid leaks or dangling
  //! pointers.
  template<typename T>
  class Ptr
  {
  public:
    //! Create a pointer to NULL.
    Ptr()
      : fObject(NULL)
    {
    }


    //! Create a pointer to the value of \p other.  The object of \p other is
    //! now referenced by at least two pointers.
    Ptr(const Ptr<T>& other)
      : fObject(other.fObject)
    {
      if (fObject != NULL)
        fObject->incRef();
    }


    //! Create a pointer to \p obj.  \p obj may be NULL.
    Ptr(T* obj)
      : fObject(obj)
    {
      if (fObject != NULL)
        fObject->incRef();
    }


    //! If *this is destroyed decrease the ref-count of the referenced object
    //! (unless NULL).  If this decreased the ref-count to 0, the object is
    //! destroyed.
    ~Ptr()
    {
      if (fObject != NULL) {
        fObject->decRef();
        fObject = NULL;
      }
    }


    //! Assign \p obj to *this and increase \p obj's ref-count.  An object
    //! referenced before is removed first, i.e. its ref-count is decreased.
    T* operator=(T* obj)
    {
      if (obj != NULL)
        obj->incRef();

      if (fObject != NULL)
        fObject->decRef();

      fObject = obj;

      return fObject;
    }


    //! Assign \p other's object to this.
    T* operator=(const Ptr<T>& other)
    {
      if (other.fObject != NULL)
        other.fObject->incRef();

      if (fObject != NULL)
        fObject->decRef();

      fObject = other.fObject;

      return fObject;
    }


    //! Return the referenced object.
    T* obj() const
    {
      return fObject;
    }


    //! Access operator; returns the referenced object.
    operator T*() const
    {
      return fObject;
    }


    //! Derefence the referenced object.  Requires (and most likely crashes) if
    //! *this points to NULL.
    T* operator->() const
    {
      hr_assert(fObject);
      return fObject;
    }


    //! Remove the referenced object from our control, i.e. decrease the
    //! object's ref-count <em>without</em> destroying, when reaching
    //! ref-count 0, return it, and set *this to NULL.  This is mostly useful
    //! from factory functions like this:
    //!
    //! <pre>
    //! Person* someFactory()
    //! {
    //!   Ptr<Person> p = new Person();
    //!   ...
    //!   return p.release();
    //! </pre>
    T* release()
    {
      if (fObject != NULL) {
        T* temp = fObject;

        fObject->decRefWithoutDelete();
        fObject = NULL;

        return temp;
      }

      return NULL;
    }

  private:
    T* fObject;
  };
};

#endif
