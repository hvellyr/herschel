/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_numbers_h
#define bootstrap_numbers_h


namespace heather
{
  //--------------------------------------------------------------------------

  template<typename T>
  T exponent(T value, int exp)
  {
    T result = 1;
    for (int i = 0; i < exp; i++)
      result *= value;
    return result;
  }


  //--------------------------------------------------------------------------

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
    
    Rational& operator=(const Rational& other);

    bool operator==(const Rational& other) const;
    bool operator!=(const Rational& other) const;
    bool operator>(const Rational& other) const;
    bool operator<(const Rational& other) const;

    Rational operator+(const Rational& other) const;
    Rational operator-(const Rational& other) const;
    Rational operator*(const Rational& other) const;
    Rational operator/(const Rational& other) const;
    Rational exponent(int exp) const;


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
