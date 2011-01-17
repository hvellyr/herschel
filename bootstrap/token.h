/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_token_h
#define bootstrap_token_h

#include <vector>
#include <map>

#if defined(UNITTESTS)
#  include <iostream>
#endif

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
    kOpThen,
    kOpUnequal,
    kOpWhile,
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
    kComma,
    kSemicolon,
    kColon,
    kAt,
    kAmpersand,
    kPipe,
    kQuote,
    kEllipsis,
    kRange,
    kDot,
    kUnionOpen,
    kSangHash,
    kReference,

    kParanOpen,
    kParanClose,
    kBracketOpen,
    kBracketClose,
    kBraceOpen,
    kBraceClose,
    kGenericOpen,
    kGenericClose,
    kLiteralVectorOpen,
    kLiteralArrayOpen,
    kMacroOpen,
    kMacroClose,

    // kLitExpr
    kString,
    kDocString,
    kChar,
    kBool,
    kInt,
    kUInt,
    kReal,
    kRational,
    kKeyword,

    // kIdExpr
    kSymbol,
    kKeyarg,
    kMacroParam,
    kMacroParamAsStr,

    // reserved identifiers
    kDefId,
    kElseId,
    kEofId,
    kExportId,
    kExtendId,
    kExternId,
    kForId,
    kFUNCTIONId,
    kFunctionId,
    kIfId,
    kImportId,
    kLetId,
    kMatchId,
    kModuleId,
    kNilId,
    kNotId,
    kOnId,
    kReifyId,
    kSelectId,
    kThenId,
    kWhenId,
    kWhereId,
    kWhileId,
  };


  class TokenImpl : public RefCountable
  {
  public:
    virtual bool operator==(const Token& other) const = 0;
    virtual bool operator<(const Token& other) const = 0;

    virtual TokenImpl* unshare()
    {
      // for immutable types unshare is a nop
      return this;
    }

    virtual void toPort(Port<Octet>* port) const = 0;
    virtual String toString() const { /* TODO */ return String(); }
  };


  typedef std::vector<Token>      TokenVector;
  typedef std::map<String, Token> NamedTokenMap;

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

    static Token newUniqueSymbolToken(const SrcPos& where,
                                      const char* prefix);

    bool operator==(const Token& other) const;
    bool operator!=(const Token& other) const;

    bool operator==(TokenType type) const;
    bool operator!=(TokenType type) const;

    bool operator<(const Token& other) const;

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
    bool isOperator() const;
    bool isNumber() const;


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

    size_t count() const;

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
    bool isTernarySeq() const;
    bool isThenWhileSeq() const;
    //! indicates whether the expression is a (possible) variable declaration;
    //! possible accepted forms are:
    //!   symbol
    //!   %(symbol : type)
    bool isVariableDecl() const;

    bool isString() const;
    bool isDocString() const;
    bool isBool() const;
    bool isInt() const;
    bool isReal() const;
    bool isRational() const;
    bool isChar() const;
    bool isKeyArg() const;

    bool isNegative() const;

    //! indicates whether the token is a symbol-function call (e.g. abc())
    bool isSymFuncall() const;

    //! indicates whether the token is a range
    bool isRange() const;
    //! indicates whether the token is a constant range (i.e. a range where
    //! from, to, and step are constant literal values only
    bool isConstRange() const;

    void toPort(Port<Octet>* port) const;

    String toString() const;


    bool isQualifiedId() const;
    String baseName() const;
    String nsName() const;

    String macroParamName() const;
    String macroParamType() const;

    //! if the token is a singleton token sequence, return children[0],
    //! otherwise the receiver itself.
    Token unwrapSingleton() const;


    //! indicates whether the receiver is an identifier which can be used
    //! as charname or unit name.
    bool isCharOrUnitName() const;

    TokenVector toTokenVector() const;

  private:
    void unshare();

    friend class IdTokenImpl;
    friend class StringTokenImpl;
    friend class NumberTokenImpl;

    //-------- data members
    TokenType      fType;
    Ptr<TokenImpl> fImpl;
    SrcPos         fSrcPos;
  };


  String operator+(const String& one, const Token& two);

  String operator+(const String& one, const TokenVector& vect);
  String operator+(const String& one, const NamedTokenMap& bindings);

#if defined(UNITTESTS)
  std::ostream& operator <<(std::ostream &os,const Token& token);
  std::ostream& operator<<(std::ostream& os, ExprType type);
#endif

};


#endif  // bootstrap_token_h
