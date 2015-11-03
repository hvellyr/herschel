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
  template<typename T>
  std::vector<T> makeVector(T first)
  {
    return std::vector<T>{first};
  }

  template<typename T, typename... Args>
  std::vector<T> makeVector(T first, Args... args)
  {
    return std::vector<T>{first, args...};
  }
}                               // namespace

#endif                          // bootstrap_utils_h
