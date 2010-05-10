/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pexpr_h
#define bootstrap_pexpr_h

#include <vector>

#include "refcountable.h"
#include "ptr.h"
#include "tokenizer.h"
#include "port.h"
#include "parsertypes.h"

namespace heather
{
  //--------------------------------------------------------------------------

  class Pexpr;

  enum PexprType {
    kSeq,
    kNested,
    kPunct,
    kLit,
    kId
  };


  class PexprImpl : public RefCountable
  {
  public:
    virtual PexprType type() const = 0;

    virtual bool operator==(const Pexpr& other) const = 0;

    virtual PexprImpl* unshare()
    {
      // for immutable types unshare is a nop
      return this;
    }

    virtual void toPort(Port<Octet>* port) const = 0;
  };


  typedef std::vector<Pexpr> PexprVector;

  class Pexpr
  {
  public:
    // a sequence expression
    Pexpr();

    // a nested expression
    Pexpr(TokenType left, TokenType right);

    // a punctation expression
    Pexpr(TokenType type);

    // an id expression
    Pexpr(const String& str);
    Pexpr(const char* str);

    // a literal expression
    Pexpr(const Token& token);

    // copy ctor
    Pexpr(const Pexpr& other);
    Pexpr& operator=(const Pexpr& other);

    bool operator==(const Pexpr& other) const;
    bool operator!=(const Pexpr& other) const;



    PexprType type() const;

    bool isSeq() const;
    bool isNested() const;
    bool isLit() const;
    bool isId() const;
    bool isPunct() const;
    bool isSet() const;

    bool isEmpty() const;

    const PexprVector& children() const;
    PexprVector& children();

    void addExpr(const Pexpr& expr);

    Pexpr& operator<<(const Pexpr& expr);
    Pexpr& operator<<(const PexprVector& exprs);
    const Pexpr& operator[](int idx) const;
    Pexpr& operator[](int idx);

    int count() const;

    TokenType punctValue() const;
    String idValue() const;
    Token tokenValue() const;
    TokenType leftToken() const;
    TokenType rightToken() const;
    bool boolLitValue() const;
    int intLitValue() const;
    double realLitValue() const;
    Rational rationalLitValue() const;
    String stringLitValue() const;
    Char charLitValue() const;

    //! useful predicates
    bool isBinarySeq() const;
    bool isBinarySeq(TokenType op) const;
    OperatorType binarySeqOperator() const;

    bool isStringLit() const;
    bool isBoolLit() const;
    bool isIntLit() const;
    bool isRealLit() const;
    bool isRationalLit() const;
    bool isCharLit() const;
    bool isKeyArgLit() const;

    //! indicates whether the pexpr is a symbol-function call (e.g. abc())
    bool isSymFuncall() const;

    bool isId(const char* sym) const;
    bool isId(const String& sym) const;

    void toPort(Port<Octet>* port) const;

  private:
    void unshare();

    //-------- data members
    Ptr<PexprImpl> fImpl;
  };
};


#endif  // bootstrap_pexpr_h
