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

  class Token;

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


  class TokenImpl : public RefCountable
  {
  public:
    virtual PexprType type() const = 0;

    virtual bool operator==(const Token& other) const = 0;

    virtual TokenImpl* unshare()
    {
      // for immutable types unshare is a nop
      return this;
    }

    virtual void toPort(Port<Octet>* port) const = 0;
    virtual String toString() const { /* TODO */ return String(); }
  };


  typedef std::vector<Token> TokenVector;

  class Token
  {
  public:
    // a sequence expression
    Token();

    // a nested expression
    Token(TokenType left, TokenType right);

    // a punctation expression
    Token(TokenType type);

    // an id expression
    Token(const String& str);
    Token(const char* str);

    // a literal typed constructor for string values
    Token(TokenType type, const String& str);
    Token(TokenType type, const char* str);

    // a literal typed constructor for int values
    Token(TokenType type, int value);

    // a literal typed constructor for double values
    Token(TokenType type, double value);

    // a literal typed constructor for rational values
    Token(TokenType type, Rational value);

    // a literal typed constructor for bool values
    Token(TokenType type, bool value);

    // copy ctor
    Token(const Token& other);
    Token& operator=(const Token& other);

    bool operator==(const Token& other) const;
    bool operator!=(const Token& other) const;


    TokenType tokenType() const;
    PexprType type() const;

    bool isSeq() const;
    bool isNested() const;
    bool isLit() const;
    bool isId() const;
    bool isPunct() const;
    bool isSet() const;

    bool isLitImaginary() const;
    Token& setIsImaginary(bool value);

    bool isEmpty() const;

    const TokenVector& children() const;
    TokenVector& children();

    void addExpr(const Token& expr);

    Token& operator<<(const Token& expr);
    Token& operator<<(const TokenVector& exprs);
    const Token& operator[](int idx) const;
    Token& operator[](int idx);

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
    Ptr<TokenImpl> fImpl;
  };


  String operator+(const String& one, const Token& two);

};


#endif  // bootstrap_pexpr_h
