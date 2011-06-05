/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_valuesaver_h
#define bootstrap_valuesaver_h

namespace herschel
{
  template <typename T>
  class ValueSaver
  {
  public:
    ValueSaver(T& var)
      : fSavedValue(var), fVar(var)
    {
    }

    ValueSaver(T& var, T value)
      : fSavedValue(var), fVar(var)
    {
      fVar = value;
    }


    ~ValueSaver()
    {
      fVar = fSavedValue;
    }

  private:
    T  fSavedValue;
    T& fVar;
  };
};                              // namespace


#endif  // bootstrap_valuesaver_h
