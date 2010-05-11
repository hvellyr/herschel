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
#include "port.h"
#include "numbers.h"


namespace heather
{
  //--------------------------------------------------------------------------

  enum OperatorType
  {
    kOpInvalid,
    kOpAppend,
    kOpAs,
    kOpAssign,
    kOpBitAnd,
    kOpBitOr,
    kOpBitXor,
    kOpBy,
    kOpCompare,
    kOpDivide,
    kOpEllipsis,
    kOpEqual,
    kOpExponent,
    kOpFold,
    kOpGreater,
    kOpGreaterEqual,
    kOpIn,
    kOpIsa,
    kOpLess,
    kOpLessEqual,
    kOpLogicalAnd,
    kOpLogicalOr,
    kOpMapTo,
    kOpMinus,
    kOpMod,
    kOpMultiply,
    kOpPlus,
    kOpRange,
    kOpShiftLeft,
    kOpShiftRight,
    kOpUnequal,
  };


  //--------------------------------------------------------------------------

  class Pexpr;

  enum PexprType {
    kSeq,
    kNested,
    kPunct,
    kLit,
    kId
  };


  enum TokenType
  {
    kEOF,
    kInvalid,

    kSeqToken,                  // prescanned token
    kNestedToken,               // prescanned token

    kPlus,
    kMinus,
    kDivide,
    kMultiply,
    kExponent,
    kFold,
    kCompare,
    kEqual,
    kUnequal,
    kLess,
    kLessEqual,
    kGreater,
    kGreaterEqual,
    kAssign,
    kMapTo,
    kIn,
    kMod,
    kIsa,
    kAs,
    kBy,
    kLogicalAnd,
    kLogicalOr,
    kBitAnd,
    kBitOr,
    kBitXor,
    kShiftLeft,
    kShiftRight,
    kAppend,

    kString,
    kChar,
    kBool,
    kInteger,
    kReal,
    kRational,

    kSymbol,
    kKeyarg,
    kMacroParam,
    kKeyword,

    kParanOpen,
    kParanClose,
    kBracketOpen,
    kBracketClose,
    kBraceOpen,
    kBraceClose,
    kGenericOpen,
    kGenericClose,
    kComma,
    kSemicolon,
    kColon,

    kAt,
    kAmpersand,
    kPipe,
    kBackQuote,
    kQuote,
    kEllipsis,
    kRange,
    kDot,

    kLiteralVectorOpen,
    kLiteralArrayOpen,
    kSangHash,
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
    virtual String toString() const { /* TODO */ return String(); }
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

    // a literal typed constructor for string values
    Pexpr(TokenType type, const String& str);
    Pexpr(TokenType type, const char* str);

    // a literal typed constructor for int values
    Pexpr(TokenType type, int value);

    // a literal typed constructor for double values
    Pexpr(TokenType type, double value);

    // a literal typed constructor for rational values
    Pexpr(TokenType type, Rational value);

    // a literal typed constructor for bool values
    Pexpr(TokenType type, bool value);

    // copy ctor
    Pexpr(const Pexpr& other);
    Pexpr& operator=(const Pexpr& other);

    bool operator==(const Pexpr& other) const;
    bool operator!=(const Pexpr& other) const;


    TokenType tokenType() const;
    PexprType type() const;

    bool isSeq() const;
    bool isNested() const;
    bool isLit() const;
    bool isId() const;
    bool isPunct() const;
    bool isSet() const;

    bool isLitImaginary() const;
    Pexpr& setIsImaginary(bool value);

    bool isEmpty() const;

    const PexprVector& children() const;
    PexprVector& children();

    void addExpr(const Pexpr& expr);

    Pexpr& operator<<(const Pexpr& expr);
    Pexpr& operator<<(const PexprVector& exprs);
    const Pexpr& operator[](int idx) const;
    Pexpr& operator[](int idx);

    int count() const;

    //-------- accessing the values and types
    TokenType punctValue() const;
    String idValue() const;
    TokenType leftToken() const;
    TokenType rightToken() const;
    bool boolLitValue() const;
    int intLitValue() const;
    double realLitValue() const;
    Rational rationalLitValue() const;
    String stringLitValue() const;
    String keywLitValue() const;
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

    String toString() const;

  private:
    void unshare();

    //-------- data members
    Ptr<PexprImpl> fImpl;
  };


  String operator+(const String& one, const Pexpr& two);

};


#endif  // bootstrap_pexpr_h
