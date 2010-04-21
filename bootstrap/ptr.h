/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_ptr_h
#define bootstrap_ptr_h

#include "common.h"


namespace heather
{
  template<typename T>
  class Ptr
  {
  public:
    Ptr()
      : fObject(NULL)
    {
    }


    Ptr(const Ptr<T>& other)
      : fObject(other.fObject)
    {
      if (fObject != NULL)
        fObject->incRef();
    }


    Ptr(T* obj)
      : fObject(obj)
    {
      if (fObject != NULL)
        fObject->incRef();
    }


    ~Ptr()
    {
      if (fObject != NULL) {
        fObject->decRef();
        fObject = NULL;
      }
    }


    T* operator=(T* obj)
    {
      if (obj != NULL)
        obj->incRef();

      if (fObject != NULL)
        fObject->decRef();

      fObject = obj;

      return fObject;
    }


    T* operator=(const Ptr<T>& other)
    {
      if (other.fObject != NULL)
        other.fObject->incRef();

      if (fObject != NULL)
        fObject->decRef();

      fObject = other.fObject;

      return fObject;
    }


    T* obj() const
    {
      return fObject;
    }


    operator T*() const
    {
      return fObject;
    }


    T* operator->() const
    {
      assert(fObject);
      return fObject;
    }


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
