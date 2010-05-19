/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "pass1.h"
#include "tokenizer.h"
#include "valuesaver.h"
#include "tokeneval.h"
#include "log.h"
#include "errcodes.h"
#include "strbuf.h"


//----------------------------------------------------------------------------

using namespace heather;


//----------------------------------------------------------------------------

FirstPass::FirstPass(Parser* parser, const Token& currentToken)
  : fParser(parser),
    fToken(currentToken),
    fEvaluateExprs(true)
{ }


Token
FirstPass::nextToken()
{
  fToken = fParser->nextToken();
  return fToken;
}


Token
FirstPass::scanUntilTopExprAndResume()
{
  while (fToken != kEOF &&
         fToken != kDefId &&
         fToken != kModuleId &&
         fToken != kInterfaceId &&
         fToken != kExportId &&
         fToken != kImportId &&
         fToken != kWhenId &&
         fToken != kExtendId)
    nextToken();

  return Token();
}


Token
FirstPass::scanUntilNextParameter()
{
  while (fToken != kEOF &&
         fToken != kComma &&
         fToken != kParanClose)
    nextToken();

  return Token();
}


template<typename ParseFunctor>
void
FirstPass::parseSequence(ParseFunctor functor,
                         TokenType startToken, TokenType endToken,
                         bool hasSeparator,
                         ErrCodes errorCode,
                         Token& result)
{
  SrcPos startPos = fToken.srcpos();
  nextToken();
  while (fToken != endToken) {
    if (fToken == kEOF)
      break;

    if (!functor(this, result))
      break;

    if (hasSeparator) {
      if (fToken == kComma)
        nextToken();
      else if (fToken != endToken)
        error(fToken.srcpos(), errorCode,
              (StringBuffer() << "expected '"
               << Token(SrcPos(), endToken).toString() << "' or ','").toString());
    }
  }

  if (fToken == endToken) {
    nextToken();
  }
  else {
    error(fToken.srcpos(), errorCode,
          (StringBuffer() << "expected '"
           << Token(SrcPos(), endToken).toString() << "'").toString());

    if (startToken != kInvalid && startPos != fToken.srcpos())
      error(startPos, errorCode,
            (StringBuffer() << "beginning '" << Token(SrcPos(), startToken).toString()
             << "' was here").toString());
    scanUntilTopExprAndResume();
  }
}


//----------------------------------------------------------------------------

struct heather::ModuleParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseTop();
    if (n.isSet())
      result << n;
    else {
      errorf(pass->fToken.srcpos(), E_UnexpectedToken,
             "Unexpected token: %s",
             (const char*)StrHelper(pass->fToken.toString()));
      return false;
    }

    return true;
  }
};

Token
FirstPass::parseModule(bool isModule)
{
  Token tagToken = fToken;
  nextToken();

  Token modExpr;

  if (fToken == kSymbol) {
    Token modName = fToken;

    nextToken();
    if (fToken == kParanOpen) {
      SrcPos begSp = fToken.srcpos();
      nextToken();

      if (fToken == kString) {
        Token publicId = fToken;

        nextToken();
        if (fToken != kParanClose) {
          errorf(fToken.srcpos(), E_ParamMissParanClose,
                 "Syntax error, missing ')'");
        }
        else
          nextToken();

        modExpr = Token()
          << tagToken
          << modName
          << ( Token(begSp, kParanOpen, kParanClose)
               << publicId );
      }
      else {
        errorf(fToken.srcpos(), E_MissingModName,
               "Syntax error, missing module name");
        return scanUntilTopExprAndResume();
      }
    }
    else
      modExpr = Token() << tagToken
                        << modName;


    if (fToken == kBraceOpen) {
      Token defines = Token(fToken.srcpos(), kBraceOpen, kBraceClose);
      parseSequence(ModuleParser(),
                    kBraceOpen, kBraceClose, false, E_MissingBraceClose,
                    defines);
      modExpr << defines;
    }
  }

  return modExpr;
}


struct heather::ExportParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken.isSymbol()) {
      result << pass->fToken;
      pass->nextToken();
    }
    else if (pass->fToken == kMultiply) {
      result << Token(pass->fToken.srcpos(), "*");
      pass->nextToken();
    }
    else {
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL or '*'");
      pass->scanUntilNextParameter();
    }

    return true;
  }
};


Token
FirstPass::parseExport()
{
  Token expr;
  expr << fToken;

  nextToken();

  while (fToken == kSymbol) {
    expr << fToken;
    nextToken();
  }

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    scanUntilTopExprAndResume();
    return Token();
  }

  Token symbols = Token(fToken.srcpos(), kParanOpen, kParanClose);
  parseSequence(ExportParser(),
                kParanOpen, kParanClose, true, E_BadParameterList,
                symbols);

  expr << symbols;

  return expr;
}


struct heather::ImportRenameParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    if (pass->fToken != kSymbol) {
      errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
      pass->scanUntilNextParameter();
    }
    else {
      Token first = pass->fToken;

      pass->nextToken();
      if (pass->fToken != kMapTo) {
        errorf(pass->fToken.srcpos(), E_MapToExpected, "expected '->'");
        pass->scanUntilNextParameter();
      }
      else {
        Token maptoToken = pass->fToken;

        pass->nextToken();
        if (pass->fToken != kSymbol) {
          errorf(pass->fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
          pass->scanUntilNextParameter();
        }
        else {
          Token second = pass->fToken;

          result << ( Token() << first << maptoToken << second );

          pass->nextToken();
        }
      }
    }

    return true;
  }
};


Token
FirstPass::parseImport()
{
  Token expr;
  expr << fToken;
  nextToken();

  if (fToken != kString) {
    errorf(fToken.srcpos(), E_StringExpected, "expected STRING");
    return scanUntilTopExprAndResume();
  }

  expr << fToken;

  nextToken();
  if (fToken == kParanOpen) {
    Token renames = Token(fToken.srcpos(), kParanOpen, kParanClose);

    parseSequence(ImportRenameParser(),
                  kParanOpen, kParanClose, true, E_BadParameterList,
                  renames);

    expr << renames;
  }

  return expr;
}


Token
FirstPass::parseTypeSpec()
{
  // TODO
  return Token();
}


struct heather::LiteralVectorParser
{
  LiteralVectorParser()
    : fIsDict(false)
  { }

  bool operator() (FirstPass* pass, Token& result)
  {
    Token expr = pass->parseExpr();

    if (result.isEmpty()) {
      if (expr.isBinarySeq(kMapTo))
        fIsDict = true;
      result << expr;
    }
    else if (fIsDict) {
      if (!expr.isBinarySeq(kMapTo))
        errorf(expr.srcpos(), E_InconsistentArgs,
               "For literal dictionaries all elements must be '->' pairs");
      else
        result << expr;
    }
    else if (expr.isSet())
      result << expr;

    return true;
  }

  bool fIsDict;
};


Token
FirstPass::parseLiteralVector()
{
  Token nested = Token(fToken.srcpos(), kLiteralVectorOpen, kParanClose);
  parseSequence(LiteralVectorParser(),
                kLiteralVectorOpen, kParanClose, true, E_BadParameterList,
                nested);
  return nested;
}


struct heather::LiteralArrayParser
{
  bool operator() (FirstPass* pass, Token& result)
  {
    Token n = pass->parseExpr();
    if (n.isSet())
      result << n;
    else {
      errorf(pass->fToken.srcpos(), E_UnexpectedToken,
             "Unexpected token: %s", (const char*)StrHelper(pass->fToken.toString()));
      return false;
    }

    return true;
  }
};


Token
FirstPass::parseLiteralArray()
{
  Token array = Token(fToken.srcpos(), kLiteralArrayOpen, kBracketClose);
  parseSequence(LiteralArrayParser(),
                kLiteralArrayOpen, kBracketClose, true, E_BadParameterList,
                array);
  return array;
}


Token
FirstPass::parseIf()
{
  Token ifToken = fToken;

  nextToken();

  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }
  SrcPos posp = fToken.srcpos();
  nextToken();

  Token test = parseExpr();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_ParamMissParanClose,
           "Syntax error, missing ')'");
  }
  else
    nextToken();

  Token consequent = parseExpr();

  Token result;
  result << ifToken;

  if (test.isSeq())
    result << ( Token(posp, kParanOpen, kParanClose)
                << test.children() );
  else
    result << ( Token(posp, kParanOpen, kParanClose)
                << test );
  result << consequent;

  if (fToken == kElseId) {
    Token elseToken = fToken;
    nextToken();

    Token alternate = parseExpr();

    result << elseToken << alternate;
  }

  return result;
}


bool
FirstPass::parseFunctionsParams(TokenVector* exprlist)
{
  SrcPos startPos = fToken.srcpos();

  // TODO
  if (fToken == kParanClose) {
    nextToken();
    return true;
  }
  else {
    error(fToken.srcpos(), E_BadParameterList, String("expected ')'"));

    if (startPos != fToken.srcpos())
      error(startPos, E_BadParameterList, String("beginning ')' was here"));
    scanUntilTopExprAndResume();
    return false;
  }
}


Token
FirstPass::parseOn()
{
  Token tagToken = fToken;
  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
    return scanUntilTopExprAndResume();
  }

  Token keyToken = fToken;

#if 0
  MacroId macroId = qualifiedIdForLookup(keyToken.idValue());
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Token(keyToken.srcpos(), macroId.name());

  if (macro != NULL) {
    Token expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, kIsLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
  {
    bool ignoreStmt = false;

    if (keyToken != Parser::syncToken &&
        keyToken != Parser::initToken &&
        keyToken != Parser::deleteToken &&
        keyToken != Parser::exitToken &&
        keyToken != Parser::signalToken) {
      errorf(keyToken.srcpos(), E_UnknownOnKey, "unknown 'on'-keyword.");
      ignoreStmt = true;
    }

    nextToken();
    if (fToken != kParanOpen) {
      errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
      return scanUntilTopExprAndResume();
    }
    SrcPos posp = fToken.srcpos();
    nextToken();

    TokenVector params;

    if (parseFunctionsParams(&params)) {
      Token body = parseExpr();

      if (!ignoreStmt)
        return Token() << tagToken << keyToken
                       << ( Token(posp, kParanOpen, kParanClose)
                            << params )
                       << body;
      else
        return Token() << Token(tagToken.srcpos(), "unspecified");
    }

    return Token();
  }
}


Token
FirstPass::parseAnonFun()
{
  nextToken();
  // TODO

  return Token();
}


void
FirstPass::parseFuncallParams(TokenVector* params)
{
  SrcPos startPos = fToken.srcpos();

  while (fToken != kParanClose) {
    if (fToken == kEOF)
      break;

    if (fToken.isKeyArg()) {
      Token key = fToken;
      nextToken();

      Token val = parseExpr();
      params->push_back(key);
      params->push_back(val);
    }
    else {
      Token val = parseExpr();
      params->push_back(val);
    }

    if (fToken == kComma) {
      params->push_back(fToken);
      nextToken();
    }
    else if (fToken != kParanClose) {
      error(fToken.srcpos(), E_BadParameterList,
            String("expected ')' or ','"));
    }
  }

  if (fToken == kParanClose) {
    nextToken();
  }
  else {
    error(fToken.srcpos(), E_BadParameterList, String("expected ')'"));

    if (startPos != fToken.srcpos())
      error(startPos, E_BadParameterList, String("beginning ')' was here"));
    scanUntilTopExprAndResume();
  }
}


Token
FirstPass::parseFunctionCall(const Token& expr,
                             const TokenVector& preScannedArgs,
                             bool parseParams)
{
  TokenVector effParams;
  if (!preScannedArgs.empty())
    effParams.assign(preScannedArgs.begin(), preScannedArgs.end());

  if (parseParams) {
    TokenVector params;
    parseFuncallParams(&params);

    if (!params.empty()) {
      if (!effParams.empty())
        effParams.push_back(Token(params.front().srcpos(), kComma));

      effParams.insert(effParams.end(),
                       params.begin(), params.end());
    }
  }

  return Token() << expr
                 << ( Token(expr.srcpos(), kParanOpen, kParanClose)
                      << effParams );
}


Token
FirstPass::parseParamCall(const Token& expr,
                          const TokenVector& preScannedArgs,
                          bool parseParams)
{
  if (expr.isSymbol()) {
#if 0
    MacroId macroId  = qualifiedIdForLookup(expr.idValue());
    Ptr<Macro> macro = lookupMacro(macroId);
    MacroType mtype  = lookupMacroType(macroId);
    String mname     = Token(expr.srcpos(), macroId.name());

    if (macro != NULL) {
      Token expr= parseMakeMacroCall(mname, &preScannedArgs, macro, mtype,
                                     parseParams, false);
      if (!expr.isSet())
        throw IncompleteMacroException(macroId);
    }
#endif
  }

  return parseFunctionCall(expr, preScannedArgs, parseParams);
}


Token
FirstPass::parseSlice(const Token& expr)
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  Token idx = parseExpr();

  if (fToken != kBracketClose)
    errorf(fToken.srcpos(), E_MissingBracketClose, "expected ']'");
  else
    nextToken();

  return Token() << Token(startPos, "slice")
                 << ( Token(startPos, kParanOpen, kParanClose)
                      << expr
                      << Token(idx.srcpos(), kComma)
                      << idx );
}


Token
FirstPass::parseAccess(const Token& expr)
{
  TokenVector args;
  if (fToken == kParanOpen) {
    nextToken();
    return parseAccess(parseParamCall(expr, args, true));
  }
  else if (fToken == kBracketOpen) {
    return parseAccess(parseSlice(expr));
  }
  else if (fToken == kDot) {
    nextToken();
    if (fToken != kSymbol) {
      errorf(fToken.srcpos(), E_SymbolExpected, "expected SYMBOL");
      return scanUntilTopExprAndResume();
    }
    Token symToken = fToken;

    nextToken();
    args.push_back(expr);
    if (fToken == kParanOpen) {
      nextToken();
      return parseAccess(parseParamCall(symToken, args, true));
    }
    else if (fToken == kBracketOpen || fToken == kDot)
      return parseAccess(parseParamCall(symToken, args, false));
    else
      return parseParamCall(symToken, args, false);
  }

  return expr;
}


Token
FirstPass::parseGroup()
{
  Token expr = parseExpr();
  if (fToken != kParanClose) {
    errorf(fToken.srcpos(), E_MissingParanClose, "expected closing ')'");
  }
  else
    nextToken();

  return expr;
}


void
FirstPass::parseExprListUntilBrace(TokenVector* result, bool endAtToplevelId)
{
  for ( ; ; ) {
    Token expr;

    if (fToken == kDefId ||
        fToken == kExtendId ||
        fToken == kExportId ||
        fToken == kImportId ||
        fToken == kInterfaceId ||
        fToken == kModuleId)
    {
      if (!endAtToplevelId) {
        error(fToken.srcpos(), E_UnexpectedTopExpr,
              String("unexpected top level expression: ") + fToken.toString());
        return;
      }
      else
        return;
    }
    else if (fToken == kLetId) {
      expr = parseDef(true);
    }
    else if (fToken == kBraceClose) {
      return;
    }
    else if (fToken == kEOF) {
      return;
    }
    else if (fToken == kSemicolon) {
      nextToken();
      continue;
    }
    else if (fToken == kWhenId) {
      expr = parseWhen(false);
    }
    else {
      expr = parseExpr();
    }

    if (expr.isSet())
      result->push_back(expr);
  }
}


Token
FirstPass::parseBlock()
{
  SrcPos startPos = fToken.srcpos();
  nextToken();

  TokenVector exprlist;
  parseExprListUntilBrace(&exprlist, false);

  SrcPos bosp = fToken.srcpos();
  if (fToken != kBraceClose) {
    errorf(fToken.srcpos(), E_MissingBraceClose, "expected '}'");

    if (startPos != fToken.srcpos())
      error(startPos, E_MissingBraceClose, String("beginning '{' was here"));
  }
  else
    nextToken();

  return Token(bosp, kBraceOpen, kBraceClose) << exprlist;
}


Token
FirstPass::parseAtomicExpr()
{
  switch (fToken.tokenType()) {
  case kBool:
  case kInt:
  case kReal:
  case kRational:
  case kString:
  case kChar:
  case kKeyword:
  case kNilId:
  case kEofId:
    {
      Token t = fToken;
      nextToken();
      return t;
    }

  case kIfId:
    return parseIf();
  case kOnId:
    return parseOn();
  case kFunctionId:
    return parseAnonFun();
  case kNotId:
    // TODO
  case kSelectId:
    // TODO
  case kUntilId:
    // TODO
    break;
  case kWhenId:
    return parseWhen(false);
  case kWhileId:
    // TODO
  case kForId:
    // TODO
  case kMatchId:
    // TODO
    break;

  case kLetId:
    assert(0);
    break;

  case kSymbol:
    {
      Token t = fToken;
      nextToken();
      return parseAccess(t);
    }

  case kLiteralVectorOpen:
    return parseLiteralVector();

  case kLiteralArrayOpen:
    return parseLiteralArray();

  case kParanOpen:
    nextToken();
    return parseAccess(parseGroup());
  case kBraceOpen:
    return parseAccess(parseBlock());

  default:
    ;
  }

  return Token();
}


Token
FirstPass::makeAssignToken(const Token& expr1, const Token& expr2,
                           const SrcPos& op1Srcpos) const
{
  if (expr1.isSymFuncall()) {
    // rename the function call in expr1 to name! and append expr2 as last
    // parameter to expr1's parameter list
    return Token() << Token(expr1[0].srcpos(), expr1[0].idValue() + "!")
                   << ( Token(op1Srcpos, kParanOpen, kParanClose)
                        << expr1.children()
                        << Token(op1Srcpos, kComma)
                        << expr2 );
  }

  return Token() << expr1
                 << Token(op1Srcpos, kAssign)
                 << expr2;
}


Token
FirstPass::makeBinaryToken(const Token& expr1, OperatorType op1,
                           const Token& expr2,
                           const SrcPos& op1Srcpos) const
{
  if (op1 == kOpAssign)
    return makeAssignToken(expr1, expr2, op1Srcpos);
  else
    return Token() << expr1
                   << Token(op1Srcpos, operatorToTokenType(op1))
                   << expr2;
}


bool
FirstPass::isRightOperator(OperatorType op1) const
{
  return (op1 == kOpAssign);
}


int
FirstPass::weightOperator(OperatorType op1) const
{
  switch (op1) {
  case kOpFold:
  case kOpMapTo:
  case kOpBy:             return  10;

  case kOpRange:
  case kOpEllipsis:       return  20;

  case kOpLogicalAnd:
  case kOpLogicalOr:      return  30;

  case kOpIsa:            return  35;

  case kOpEqual:
  case kOpUnequal:
  case kOpLess:
  case kOpLessEqual:
  case kOpGreater:
  case kOpGreaterEqual:
  case kOpCompare:        return  40;

  case kOpIn:             return  45;

  case kOpAppend:         return  47;

  case kOpBitAnd:
  case kOpBitOr:
  case kOpBitXor:         return  50;

  case kOpShiftLeft:
  case kOpShiftRight:     return  80;

  case kOpPlus:
  case kOpMinus:          return  90;

  case kOpMultiply:
  case kOpDivide:
  case kOpMod:            return 100;

  case kOpExponent:       return 110;

  case kOpAs:             return 200;

  case kOpAssign:         return 999999;

  case kOpInvalid:
    assert(0);
  }

  return 999999;
}


bool
FirstPass::isOpWeightAbove(OperatorType op1, OperatorType op2) const
{
  return weightOperator(op1) > weightOperator(op2);
}


Token
FirstPass::parseExprRec(const Token& expr1,
                        OperatorType op1, const SrcPos& op1Srcpos)
{
  if (op1 == kOpInvalid)
    return expr1;

  nextToken();
  SrcPos before2ndPos = fToken.srcpos();
  Token expr2 = parseAtomicExpr();
  OperatorType op2 = tokenTypeToOperator(fToken.tokenType());
  SrcPos op2Srcpos = fToken.srcpos();

  if (!expr2.isSet()) {
    errorf(before2ndPos, E_MissingRHExpr, "no right hand expression");
    return expr1;
  }

  if (op2 == kOpInvalid) {
    if (op1 == kOpAssign)
      return makeAssignToken(expr1, expr2, op1Srcpos);
    else
      return Token() << expr1
                     << Token(op1Srcpos, operatorToTokenType(op1))
                     << expr2;
  }
  else {
    if (!isRightOperator(op1) && isOpWeightAbove(op1, op2))
      return parseExprRec(makeBinaryToken(expr1, op1, expr2, op1Srcpos),
                          op2,
                          op2Srcpos);
    else
      return makeBinaryToken(expr1, op1, parseExprRec(expr2, op2, op2Srcpos),
                             op1Srcpos);
  }
}


Token
FirstPass::parseExpr()
{
  Token expr1 = parseAtomicExpr();
  OperatorType op1 = tokenTypeToOperator(fToken.tokenType());
  SrcPos opSrcpos = fToken.srcpos();
  if (op1 != kOpInvalid)
    return parseExprRec(expr1, op1, opSrcpos);
  return expr1;
}


void
FirstPass::parseTopExprUntilBrace(TokenVector* result)
{
  while (fToken != kBraceClose) {
    if (fToken == kEOF)
      break;
    Token topexpr = parseTop();
    result->push_back(topexpr);
  }

  if (fToken == kBraceClose)
    nextToken();
}


Token
FirstPass::parseTopOrExprList(bool isTopLevel)
{
  if (isTopLevel) {
    if (fToken == kBraceOpen) {
      SrcPos bosp = fToken.srcpos();
      nextToken();
      TokenVector exprs;
      parseTopExprUntilBrace(&exprs);

      return Token(bosp, kBraceOpen, kBraceClose) << exprs;
    }
    return parseTop();
  }
  return parseExpr();
}


Token
FirstPass::parseWhen(bool isTopLevel)
{
  Token result;
  result << fToken;

  nextToken();

  bool inclConsequent = true;
  bool inclAlternate = true;

  if (fToken.isSymbol()) {
    if (fToken == Parser::ignoreToken) {
      inclConsequent = false;
    }
    else if (fToken == Parser::includeToken) {
      inclAlternate = false;
    }
    else {
      errorf(fToken.srcpos(), E_UnexpectedToken,
             "only 'ignore' or 'include' are valid symbols here");
      errorf(fToken.srcpos(), E_UnexpectedToken, "assume 'ignore'");

      inclConsequent = false;
    }
    nextToken();
  }
  else if (fToken == kParanOpen) {
    SrcPos posp = fToken.srcpos();
    nextToken();

    Token test = parseExpr();
    if (fToken != kParanClose) {
      errorf(fToken.srcpos(), E_ParamMissParanClose, "missing ')'");
      if (fToken == kBraceOpen) {
        // try to continue with this
      }
      else
        return scanUntilTopExprAndResume();
    }
    else
      nextToken();

    if (fEvaluateExprs) {
      TokenEvalContext ctx(fParser->configVarRegistry());
      Token p = ctx.evalToken(test);
      if (p.isBool()) {
        inclConsequent = p.boolValue();
        inclAlternate = !p.boolValue();
      }
      else {
        fprintf(stderr, "ERROR: when-expression did not evaluate to boolean. "
                "Treat it as false\n");
        inclConsequent = false;
        inclAlternate = true;
      }
    }
    else
      result << ( Token(posp, kParanOpen, kParanClose) << test );
  }
  else if (fToken == kBraceOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "missing parameters or key for 'when' clause");
    errorf(fToken.srcpos(), E_MissingParanOpen, "assume 'ignore' here");
    // try to continue with this

    inclConsequent = false;
    inclAlternate = true;
  }
  else {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    scanUntilTopExprAndResume();
    return Token();
  }


  Token consequent;
  Token alternate;
  Token elseToken;

  {
    ValueSaver<bool> keep(fEvaluateExprs, inclConsequent);
    consequent = parseTopOrExprList(isTopLevel);
  }

  if (fToken == kElseId) {
    elseToken = fToken;
    nextToken();
    {
      ValueSaver<bool> keep(fEvaluateExprs, inclAlternate);
      alternate = parseTopOrExprList(isTopLevel);
    }
  }

  if (inclConsequent && inclAlternate) {
    result << consequent;
    if (alternate.isSet())
      result << elseToken << alternate;

    return result;
  }
  else if (inclConsequent)
    return consequent;
  else if (inclAlternate)
    return alternate;

  assert(0);
  return Token();
}


Token
FirstPass::parseVarDef(const Token& defToken, const Token& tagToken, bool isLocal)
{
  Token keepTagToken = tagToken;

  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
    return scanUntilTopExprAndResume();
  }
  Token symbolToken = fToken;

  nextToken();

  return parseVarDef2(defToken, keepTagToken, symbolToken, isLocal);
}


Token
FirstPass::evaluateConfigExpr(const Token& initExpr)
{
  TokenEvalContext ctx(fParser->configVarRegistry());
  return ctx.evalToken(initExpr);
}


Token
FirstPass::parseVarDef2(const Token& defToken, const Token& tagToken,
                        const Token& symbolToken,
                        bool isLocal)
{
  Token type;
  Token colonToken, assignToken;

  if (fToken == kColon) {
    colonToken = fToken;
    nextToken();
    type = parseTypeSpec();
  }

  Token initExpr;
  if (fToken == kAssign) {
    assignToken = fToken;
    nextToken();

    SrcPos pos = fToken.srcpos();
    initExpr = parseExpr();
    if (!initExpr.isSet())
      errorf(pos, E_MissingRHExpr, "no value in var init");
  }

  Token vardefExpr;
  vardefExpr << defToken;
  if (tagToken.isSet())
    vardefExpr << tagToken;

  vardefExpr << symbolToken;

  if (type.isSet())
    vardefExpr << colonToken << type;

  if (tagToken == Parser::configToken) {
    if (fEvaluateExprs) {
      if (!initExpr.isSet()) {
        error(symbolToken.srcpos(), E_DefNoInitValue,
              ( String("Config variable '") + symbolToken +
                "' without default value") );

        // if no default value is given assume ''
        initExpr = Token(symbolToken.srcpos(), kString, "");
        if (!assignToken.isSet())
          assignToken = Token(symbolToken.srcpos(), kAssign);
      }
      fParser->configVarRegistry()->registerValue(symbolToken.idValue(),
                                                  evaluateConfigExpr(initExpr));
      // even if we have to evaluate the config var expression, we have to
      // keep the constructed expr since config-vars can be used like
      // normal const-vars by code
    }
  }

  if (initExpr.isSet())
    vardefExpr << assignToken << initExpr;

  return vardefExpr;
}


Token
FirstPass::parseCharDef(const Token& defToken)
{
  Token tagToken = fToken;

  nextToken();

  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "Missing name");
    return scanUntilTopExprAndResume();
  }
  Token charNameToken = fToken;

  nextToken();
  if (fToken != kAssign)
    throw UnexpectedTokenException(fToken, "expected =");
  Token assignToken = fToken;

  nextToken();
  if (fToken != kInt)
    throw UnexpectedTokenException(fToken, "expected INTEGER");
  Token codePointToken = fToken;

  int codePoint = fToken.intValue();
  if (codePoint < 0 || codePoint > 0x10FFFF)
    throw SyntaxException(String("invalid expected INTEGER"));

  nextToken();

  if (fEvaluateExprs) {
    fParser->charRegistry()->registerValue(charNameToken.idValue(),
                                           codePoint);
    return Token();
  }
  else
    return Token() << defToken << tagToken << charNameToken
                   << assignToken << codePointToken;
}


Token
FirstPass::parseFunctionDef(const Token& defToken, const Token& tagToken,
                            const Token& symToken, bool isLocal)
{
  // TODO
  return Token();
}


Token
FirstPass::parseFunctionOrVarDef(const Token& defToken, bool isLocal)
{
  assert(fToken == kSymbol);

  Token symToken = fToken;

#if 0
  MacroId macroId = qualifiedIdForLookup(symToken.idValue());
  Ptr<Macro> macro = lookupMacro(macroId);
  MacroType mtype = lookupMacroType(macroId);
  String mname = Token(macroId.name());

  if (macro != NULL) {
    Token expr= parseMakeMacroCall(mname, NULL, macro, mtype, true, isLocal);
    if (!expr.isSet())
      throw IncompleteMacroException(macroId);
  }
  else
#endif
  {
    nextToken();
    if (fToken == kParanOpen)
      return parseFunctionDef(defToken, Token(), symToken, isLocal);

    return parseVarDef2(defToken, Token(), symToken, isLocal);
  }
  return Token();
}


Token
FirstPass::parseGenericFunctionDef(const Token& defToken, bool isLocal)
{
  Token tagToken;
  if (isLocal) {
    errorf(fToken.srcpos(), E_LocalGenericFunc,
           "inner generic functions are not supported.  'generic' ignored");
  }
  else
    tagToken = fToken;

  nextToken();
  if (fToken != kSymbol) {
    errorf(fToken.srcpos(), E_MissingDefName, "expected function name");
    return scanUntilTopExprAndResume();
  }
  Token symToken = fToken;

  nextToken();
  if (fToken != kParanOpen) {
    errorf(fToken.srcpos(), E_MissingParanOpen, "expected '('");
    return scanUntilTopExprAndResume();
  }

  return parseFunctionDef(defToken, tagToken, symToken, isLocal);
}


Token
FirstPass::parseDef(bool isLocal)
{
  Token defToken = fToken;
  nextToken();

  if (fToken == Parser::typeToken) {
    // TODO
  }
  else if (fToken == Parser::aliasToken) {
    // TODO
  }
  else if (fToken == Parser::classToken) {
    // TODO
  }
  else if (fToken == Parser::enumToken) {
    // TODO
  }
  else if (fToken == Parser::measureToken) {
    // TODO
  }
  else if (fToken == Parser::unitToken) {
    // TODO
  }
  else if (fToken == Parser::constToken ||
           fToken == Parser::fluidToken ||
           fToken == Parser::configToken) {
    return parseVarDef(defToken, fToken, isLocal);
  }
  else if (fToken == Parser::genericToken) {
    return parseGenericFunctionDef(defToken, isLocal);
  }
  else if (fToken == Parser::charToken) {
    return parseCharDef(defToken);
  }
  else if (fToken == Parser::macroToken) {
    // TODO
  }
  else if (fToken == kSymbol)
    return parseFunctionOrVarDef(defToken, isLocal);
  else {
    errorf(fToken.srcpos(), E_DefInitValueUnexpectedToken,
           "Bad init value: %s", (const char*)StrHelper(fToken.toString()));
    return scanUntilTopExprAndResume();
  }

  return Token();
}


Token
FirstPass::parseTop()
{
  if (fToken == kModuleId) {
    return parseModule(true);
  }
  else if (fToken == kInterfaceId) {
    nextToken();
    return parseModule(false);
  }
  else if (fToken == kExportId) {
    return parseExport();
  }
  else if (fToken == kImportId) {
    return parseImport();
  }
  else if (fToken == kDefId) {
    return parseDef(false);
  }
  else if (fToken == kWhenId) {
    return parseWhen(true);
  }
  else if (fToken == kExtendId) {
    // TODO
  }
  else {
    errorf(fToken.srcpos(), E_UnexpectedToken,
           "Unexpected token: %s", (const char*)StrHelper(fToken.toString()));
    return scanUntilTopExprAndResume();
  }

  return Token();
}


Token
FirstPass::parse()
{
  Token seq;

  nextToken();
  while (fToken != kEOF) {
    Token n = parseTop();
    if (n.isSet())
      seq << n;
  }

  return seq;
}


