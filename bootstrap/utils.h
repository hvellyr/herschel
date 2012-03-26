/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_utils_h
#define bootstrap_utils_h

#include <vector>


namespace herschel
{
  //! Helper type for the vector_of function
  template<typename T>
  class VectorOf
  {
  public:
    VectorOf(T arg)
    {
      fVector.push_back(arg);
    }

    operator std::vector<T> ()
    {
      return fVector;
    }

    VectorOf<T> operator() (T arg)
    {
      fVector.push_back(arg);
      return *this;
    }

    std::vector<T> fVector;

  };

  //! Convience function to construct vector of T elements on the fly.  To
  //! construct a vector of integers you could use for instance:
  //!
  //!  vector_of(1)(2)(3)(4)(5)
  //!
  template<typename T>
  VectorOf<T> vector_of(T arg)
  {
    return VectorOf<T>(arg);
  }
};                              // namespace

#endif                          // bootstrap_utils_h
