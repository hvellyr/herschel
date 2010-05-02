/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_numbers_h
#define bootstrap_numbers_h


namespace heather
{
  class Rational
  {
  public:
    Rational()
      : fNumerator(0),
        fDenominator(1)
      { }

    Rational(int num, int den)
      : fNumerator(num),
        fDenominator(den)
      { }
    
    Rational(const Rational& other)
      : fNumerator(other.fNumerator),
        fDenominator(other.fDenominator)
      { }
    
    Rational& operator=(const Rational& other)
    {
      fNumerator = other.fNumerator;
      fDenominator = other.fDenominator;
      return *this;
    }

    bool operator==(const Rational& other) const
    {
      return fNumerator == other.fNumerator &&
        fDenominator == other.fDenominator;
    }

    bool operator!=(const Rational& other) const
    {
      return fNumerator != other.fNumerator ||
        fDenominator != other.fDenominator;
    }

    int numerator() const
    {
      return fNumerator;
    }

    int denominator() const
    {
      return fDenominator;
    }

  private:
    int fNumerator;
    int fDenominator;
  };

};

#endif  // bootstrap_numbers_h
