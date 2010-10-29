/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_valuesaver_h
#define bootstrap_valuesaver_h

namespace heather
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
