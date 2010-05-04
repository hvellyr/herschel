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

    virtual PexprImpl* unshare()
    {
      // for immutable types unshare is a nop
      return this;
    }

    virtual void toPort(Port<Octet>* port) const = 0;
  };


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

    PexprType type() const;

    bool isSeq() const;
    bool isNested() const;
    bool isLit() const;
    bool isId() const;
    bool isPunct() const;

    bool isEmpty() const;

    const std::vector<Pexpr>& children() const;
    std::vector<Pexpr>& children();

    void addExpr(const Pexpr& expr);

    Pexpr& operator<<(const Pexpr& expr);
    Pexpr& operator<<(const std::vector<Pexpr>& exprs);
    const Pexpr& operator[](int idx) const;
    Pexpr& operator[](int idx);

    int count() const;

    // copy ctor
    Pexpr(const Pexpr& other);
    Pexpr& operator=(const Pexpr& other);

    TokenType punctValue() const;
    String idValue() const;
    Token tokenValue() const;
    TokenType leftToken() const;
    TokenType rightToken() const;

    //! useful predicates
    bool isBinarySeq(TokenType op) const;
    bool isStringLit() const;

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
