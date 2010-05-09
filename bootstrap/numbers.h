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

    bool operator>(const Rational& other) const
    {
      return (fNumerator * other.fDenominator -
              other.fNumerator * fDenominator > 0);
    }

    bool operator<(const Rational& other) const
    {
      return (fNumerator * other.fDenominator -
              other.fNumerator * fDenominator < 0);
    }

    Rational operator+(const Rational& other) const
    {
      int otherDen = other.denominator();
      int n = fNumerator * otherDen;
      int na = other.numerator() * fDenominator;
      return Rational(n + na, fDenominator * otherDen);
    }

    int numerator() const
    {
      return fNumerator;
    }

    int denominator() const
    {
      return fDenominator;
    }


    double toReal() const
    {
      return double(fNumerator) / double(fDenominator);
    }

  private:
    int fNumerator;
    int fDenominator;
  };

};

#endif  // bootstrap_numbers_h
