/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "externc.h"
#include "port.h"
#include "str.h"
#include "strbuf.h"
#include "log.h"


using namespace herschel;

class BadCSyntaxException
{
public:
  BadCSyntaxException() { }
};


ExternCParser::ExternCParser(FirstPass* pass1)
  : fPass(pass1)
{
  fToken = fPass->currentToken();
}


Token
ExternCParser::nextToken()
{
  fToken = fPass->nextToken();
  //printf("[%s] ", (const char*)StrHelper(fToken.toString()));
  return fToken;
}


Token
ExternCParser::scanUntilBrace()
{
  fPass->scanUntilBrace();
  fToken = fPass->currentToken();
  return fToken;
}


static String
makeCTypeName(const String& base, bool isSigned, int ptrDepth)
{
  StringBuffer buf;

  buf << "clang|";

  if (!isSigned && (base == String("int") || base == String("short") ||
                    base == String("char") || base == String("long")) )
    buf << "unsigned-";

  buf << base;

  for (int i = 0; i < ptrDepth; i++)
    buf << "*";

  return buf.toString();
}


Token
ExternCParser::parseTypeSpec()
{
  bool isSigned = true;
  bool isConst = false;
  SrcPos srcpos = fToken.srcpos();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_BadCSyntax, "expected C symbol");
    scanUntilBrace();
    throw BadCSyntaxException();
  }

  if (fToken.idValue() == String("unsigned")) {
    isSigned = false;
    nextToken();
  }
  else if (fToken.idValue() == String("signed")) {
    // all types are signed, unless specified
    nextToken();
  }

  if (fToken.idValue() == String("const")) {
    isConst = true;
    nextToken();
  }

  String type = fToken.idValue();
  int ptrDepth = 0;

  nextToken();
  if (fToken == kMultiply) {
    ptrDepth = 1;
    nextToken();
  }
  else if (fToken == kExponent) {
    ptrDepth = 2;
    nextToken();
  }

  if (type == String("void") || type == String("float") || type == String("double"))
    return Token(srcpos, makeCTypeName(type, true, ptrDepth));
  else if (type == String("char") || type == String("short") ||
           type == String("int") || type == String("long"))
    return Token(srcpos, makeCTypeName(type, isSigned, ptrDepth));
  else if (type.endsWith(String("**")))
    return Token(srcpos, makeCTypeName(type.part(0, type.length() - 2),
                                       isSigned, ptrDepth + 2));
  else if (type.endsWith(String("*")))
    return Token(srcpos, makeCTypeName(type.part(0, type.length() - 1),
                                       isSigned, ptrDepth + 1));
  else {
    errorf(fToken.srcpos(), E_BadCSyntax, "expected C type");
    scanUntilBrace();
    throw BadCSyntaxException();
  }

  return Token(srcpos, makeCTypeName(type, isSigned, ptrDepth));
}


Token
ExternCParser::parseCFunction(const Token& sym, const Token& retType)
{
  SrcPos paranPos = fToken.srcpos();

  hr_assert(fToken == kParanOpen);

  Token retval;
  retval << makeExternDefHead(fToken.srcpos()) << sym;

  nextToken();
  TokenVector params;
  while (fToken == kEllipsis || fToken == kSymbol) {
    if (fToken == kEllipsis) {
      params.push_back(Token() << Token(fToken.srcpos(), "_rest") << fToken);
      nextToken();
      break;
    }

    if (fToken == kSymbol) {
      SrcPos paramSrcpos = fToken.srcpos();
      // parse parameter
      Token paramType = parseTypeSpec();

      Token paramName;
      if (fToken == kSymbol) {
        paramName = fToken;
        nextToken();
      }
      else {
        paramName = Token(paramSrcpos, "any");
      }

      params.push_back(Token() << paramName
                       << Token(paramSrcpos, kColon)
                       << paramType);
    }

    if (fToken == kComma) {
      params.push_back(fToken);
      nextToken();
      continue;
    }
    else if (fToken == kParanClose) {
      break;
    }
    else {
      errorf(fToken.srcpos(), E_BadCSyntax, "expected C parameter name or ','");
      scanUntilBrace();
      throw BadCSyntaxException();
    }
  }

  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_BadCSyntax, "expected ')' for C function");
    scanUntilBrace();
    throw BadCSyntaxException();
  }
  else
    nextToken();

  retval << ( Token(paranPos, kParanOpen, kParanClose)
              << params )
         << Token(paranPos, kColon) << retType
         << Token(paranPos, kEllipsis);
  return retval;
}


TokenVector
ExternCParser::makeExternDefHead(const SrcPos& srcpos)
{
  TokenVector retval;
  retval.push_back(Token(srcpos, kDefId));
  retval.push_back(Token() << Token(srcpos, kExternId)
                   << ( Token(srcpos, kParanOpen, kParanClose)
                        << Token(srcpos, kString, "C") ) );
  return retval;
}


TokenVector
ExternCParser::parseBlock()
{
  // for now only simple things:

  //   int func ( );
  //   int func (int a, const t const * t);
  //   int func (int a, const t const * t, ...);
  //   int var;

  TokenVector retval;

  try {
    while (fToken != kBraceClose) {
      if (fToken == kExternId)
        nextToken();

      Token type = parseTypeSpec();

      if (fToken != kSymbol) {
        errorf(fToken.srcpos(), E_BadCSyntax,
               "expected C function or variable name");
        scanUntilBrace();
        throw BadCSyntaxException();
      }
      Token sym = fToken;

      nextToken();
      Token def;
      if (fToken == kParanOpen)
        def = parseCFunction(sym, type);
      else
        def << makeExternDefHead(type.srcpos()) << sym
            << Token(type.srcpos(), kColon) << type;

      if (fToken == kSemicolon) {
        nextToken();
        if (def.isSet())
          retval.push_back(def);
      }
    }
  }
  catch (const BadCSyntaxException& e) {
    scanUntilBrace();
    if (fToken == kBraceClose)
      nextToken();
    return TokenVector();
  }


  if (fToken == kBraceClose)
    nextToken();

  return retval;
}


