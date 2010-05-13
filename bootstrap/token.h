/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_token_h
#define bootstrap_token_h

#include <vector>

#include "refcountable.h"
#include "ptr.h"
#include "port.h"
#include "numbers.h"
#include "srcpos.h"


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

  enum ExprType {
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

    // kSeqExpr
    kSeqExpr,                   // prescanned token
    // kNestedExpr
    kNestedExpr,                // prescanned token

    // kPunctExpr
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

    // kLitExpr
    kString,
    kChar,
    kBool,
    kInt,
    kReal,
    kRational,
    kKeyword,

    // kIdExpr
    kSymbol,
    kKeyarg,
    kMacroParam,
  };


  class TokenImpl : public RefCountable
  {
  public:
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
    Token(const SrcPos& where, TokenType left, TokenType right);

    // a punctation expression
    Token(const SrcPos& where, TokenType ttype);

    // an id expression
    Token(const SrcPos& where, const String& str);
    Token(const SrcPos& where, const char* str);

    // a literal typed constructor for string values
    Token(const SrcPos& where, TokenType type, const String& str);
    Token(const SrcPos& where, TokenType type, const char* str);

    // a literal typed constructor for int values
    Token(const SrcPos& where, TokenType type, int value);

    // a literal typed constructor for double values
    Token(const SrcPos& where, TokenType type, double value);

    // a literal typed constructor for rational values
    Token(const SrcPos& where, TokenType type, Rational value);

    // a literal typed constructor for bool values
    Token(const SrcPos& where, TokenType type, bool value);

    // copy ctor
    Token(const Token& other);
    Token& operator=(const Token& other);

    bool operator==(const Token& other) const;
    bool operator!=(const Token& other) const;

    bool operator==(TokenType type) const;
    bool operator!=(TokenType type) const;

    const SrcPos& srcpos() const;
    Token& setSrcpos(const SrcPos& srcpos);


    TokenType tokenType() const;
    ExprType type() const;

    bool isSeq() const;
    bool isNested() const;
    bool isLit() const;
    bool isId() const;
    bool isSymbol() const;
    bool isPunct() const;
    bool isSet() const;

    bool isImaginary() const;
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
    bool boolValue() const;
    int intValue() const;
    double realValue() const;
    Rational rationalValue() const;
    String stringValue() const;
    Char charValue() const;

    //! useful predicates
    bool isBinarySeq() const;
    bool isBinarySeq(TokenType op) const;
    OperatorType binarySeqOperator() const;

    bool isString() const;
    bool isBool() const;
    bool isInt() const;
    bool isReal() const;
    bool isRational() const;
    bool isChar() const;
    bool isKeyArg() const;

    //! indicates whether the token is a symbol-function call (e.g. abc())
    bool isSymFuncall() const;

    bool isSymbol(const char* sym) const;
    bool isSymbol(const String& sym) const;

    void toPort(Port<Octet>* port) const;

    String toString() const;

  private:
    void unshare();

    //-------- data members
    TokenType      fType;
    Ptr<TokenImpl> fImpl;
    SrcPos         fSrcPos;
  };


  String operator+(const String& one, const Token& two);

};


#endif  // bootstrap_token_h
