/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <vector>
#include <map>

#if defined(UNITTESTS)
#  include <iostream>
#endif

#include "numbers.hpp"
#include "port.hpp"
#include "srcpos.hpp"


namespace herschel
{
  //--------------------------------------------------------------------------

  enum OperatorType
  {
    kOpInvalid,

    kOpConcat,
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
    kOpRem,
    kOpMultiply,
    kOpPlus,
    kOpRange,
    kOpShiftLeft,
    kOpShiftRight,
    kOpThen,
    kOpUnequal,
    kOpWhile,
  };

  //! Returns a human readable name for operator \p type for debug and error
  //! reporting.
  zstring operatorName(OperatorType type);


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
    kRem,
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
    kConcat,
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
    kMacroOpen2,  // ascii alternative to kMacroOpen
    kMacroClose2,  // ascii alternative to kMacroClose

    // kLitExpr
    kString,
    kDocString,
    kChar,
    kBool,
    kInt,
    kUInt,
    kFloat,
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
    kSelectId,
    kThenId,
    kWhenId,
    kWhereId,
    kWhileId,
  };


  class TokenImpl
  {
  public:
    virtual bool operator==(const Token& other) const = 0;
    virtual bool operator<(const Token& other) const = 0;

    virtual std::shared_ptr<TokenImpl> unshare(std::shared_ptr<TokenImpl> impl) const
    {
      // for immutable types unshare is a nop
      return impl;
    }

    virtual void toPort(Port<Octet>& port) const = 0;
    virtual String toString() const { /* TODO */ return String(); }
  };


  using TokenVector = std::vector<Token>;
  using NamedTokenMap = std::map<String, Token>;

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
    Token(const SrcPos& where, zstring str);

    // a literal typed constructor for string values
    Token(const SrcPos& where, TokenType type, const String& str);
    Token(const SrcPos& where, TokenType type, zstring str);

    // a literal typed constructor for int values
    Token(const SrcPos& where, TokenType type, int value);

    // a literal typed constructor for double values
    Token(const SrcPos& where, TokenType type, double value);

    // a literal typed constructor for rational values
    Token(const SrcPos& where, TokenType type, Rational value);

    // a literal typed constructor for bool values
    Token(const SrcPos& where, TokenType type, bool value);

    static Token newInt(const SrcPos& where, int bitwidth, int64_t value);
    static Token newUInt(const SrcPos& where, int bitwidth, uint64_t value);

    // copy ctor
    Token(const Token& other);
    Token& operator=(const Token& other);

    static Token newUniqueSymbolToken(const SrcPos& where, zstring prefix);

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
    int64_t intValue() const;
    int bitwidth() const;
    double floatValue() const;
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
    bool isFloat() const;
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

    void toPort(Port<Octet>& port) const;

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
    std::shared_ptr<TokenImpl> fImpl;
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


