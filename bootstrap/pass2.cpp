/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "tokenizer.h"
#include "pass2.h"
#include "token.h"
#include "errcodes.h"
#include "log.h"
#include "parsertypes.h"


using namespace heather;

//----------------------------------------------------------------------------

SecondPass::SecondPass(Parser* parser)
  : fParser(parser)
{ }


void
SecondPass::parseTopExprlist(AptNode* rootNode, const Token& expr)
{
  for (TokenVector::const_iterator it = expr.children().begin();
       it != expr.children().end();
       it++)
  {
    Ptr<AptNode> targetNode = ( !fLastModules.empty()
                                ? fLastModules.front().obj()
                                : rootNode );

    Ptr<AptNode> n = parseExpr(*it);
    if (n != NULL)
      targetNode->appendNode(n);
  }
}


AptNode*
SecondPass::parseModule(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kModuleId);
  assert(expr[1].isSymbol());

  String modName = expr[1].idValue();
  String publicId;

  if (expr.count() > 2) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);
    assert(expr[2].count() == 1);
    assert(expr[2][0].isString());

    publicId = expr[2][0].stringValue();
  }

  Ptr<ModuleNode> modNode = new ModuleNode(expr.srcpos(),
                                           modName, publicId);

  if (expr.count() > 3) {
    assert(expr[3].isNested() && expr[3].leftToken() == kBraceOpen);

    fLastModules.push_front(modNode.obj());
    parseTopExprlist(modNode, expr[3]);
    fLastModules.pop_front();
  }
  else
    fLastModules.push_front(modNode.obj());

  return modNode.release();
}


AptNode*
SecondPass::parseExport(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kExportId);

  int symbolOfs = 1;
  ExportNode::VizType vizType = ExportNode::kPrivate;
  if (expr[1].isSymbol()) {
    if (expr[1] == Parser::publicToken)
      vizType = ExportNode::kPublic;
    else if (expr[1] == Parser::innerToken)
      vizType = ExportNode::kInner;
    else if (expr[1] == Parser::outerToken)
      vizType = ExportNode::kOuter;
    else {
      error(expr[1].srcpos(), E_UnknownVisibility,
            String("unknown visibility level: ") + expr[1]);
    }

    symbolOfs = 2;
  }

  assert(expr.count() > symbolOfs);
  StringList symbols;
  if (expr[symbolOfs].isNested()) {
    Token symbolExprs = expr[symbolOfs];
    for (int j = 0; j < symbolExprs.count(); j++) {
      if (symbolExprs[j].isSymbol())
        symbols.push_back(symbolExprs[j].idValue());
    }
  }

  bool isFinal = false;
  if (expr.count() >= symbolOfs + 2) {
    assert(expr[symbolOfs + 1] == kAs);
    assert(expr[symbolOfs + 2] == Parser::finalToken);

    isFinal = true;
  }

  return new ExportNode(expr[0].srcpos(), vizType, isFinal, symbols);
}


AptNode*
SecondPass::parseImport(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kImportId);
  assert(expr[1].isString());

  String codeFile = expr[1].stringValue();
  StringStringMap renames;

  if (expr.count() >= 3) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);

    Token renExprs = expr[2];
    for (int i = 0; i < renExprs.count(); i++) {
      Token renExpr = renExprs[i];
      if (renExpr.isBinarySeq(kMapTo)) {
        assert(renExpr[0].isSymbol());
        assert(renExpr[2].isSymbol());

        renames.insert(std::make_pair(
                         renExpr[0].idValue(), renExpr[2].idValue()));
      }
    }
  }

  return new ImportNode(expr[1].srcpos(), codeFile, renames);
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseTypeSpec(const Token& expr)
{
  // TODO
  return NULL;
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseTypeDef(const Token& expr, bool isType)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseAliasDef(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseSlotDef(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseEnumDef(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseMeasureDef(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseUnitDef(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseVarDef(const Token& expr, VardefFlags flags, int ofs)
{
  assert(ofs >= 1);
  assert(ofs < expr.count());
  assert(expr[ofs] == kSymbol);

  const TokenVector& seq = expr.children();
  String sym = seq[ofs].idValue();
  ofs++;

  Ptr<AptNode> type;
  if (ofs + 1 < expr.count() && seq[ofs] == kColon) {
    type = parseTypeSpec(seq[ofs + 1]);
    ofs += 2;
  }

  Ptr<AptNode> initExpr;
  if (ofs + 1 < expr.count() && seq[ofs] == kAssign) {
    initExpr = parseExpr(seq[ofs + 1]);
    ofs += 2;
  }

  return new VardefNode(expr.srcpos(),
                        sym, flags, type, initExpr);
}


AptNode*
SecondPass::parseFunctionDef(const Token& expr)
{
  assert(expr.isSeq());
  assert(expr.count() >= 3);
  assert(expr[0] == kDefId || expr[0] == kLetId);

  int ofs = 1;
  bool isGeneric = false;
  if (expr[ofs] == Parser::genericToken) {
    isGeneric = true;
    ofs++;
  }

  assert(expr[ofs] == kSymbol);
  String sym = expr[ofs].idValue();
  ofs++;

  assert(expr[ofs].isNested());
  NodeList params;
  parseParameters(&params, expr[ofs].children());
  ofs++;

  Ptr<AptNode> type;
  if (ofs < expr.count()) {
    if (expr[ofs] == kColon) {
      type = parseTypeSpec(expr[ofs + 1]);
      ofs += 2;
    }
  }

  Ptr<AptNode> reify;
  if (ofs < expr.count()) {
    if (expr[ofs].isSeq() && expr[ofs].count() > 1 &&
        expr[ofs][0] == kReifyId)
    {
      // TODO
      ofs++;
    }
  }

  Ptr<AptNode> where;
  if (ofs < expr.count()) {
    if (expr[ofs].isSeq() && expr[ofs].count() > 1 &&
        expr[ofs][0] == kWhereId)
    {
      // TODO.  Don't parse the where clause into apt nodes here, but enrich a
      // passed in context.  The 'Where' information is used to transform
      // quoted types into full type spec.
      ofs++;
    }
  }

  Ptr<AptNode> body;
  if (ofs < expr.count()) {
    body = parseExpr(expr[ofs]);
    ofs++;
  }

  return new FuncDefNode(expr.srcpos(),
                         sym, isGeneric, params, body);
}


AptNode*
SecondPass::newDefNode(AptNode* node, bool isLet)
{
  if (isLet)
    return new LetNode(node);
  else
    return new DefNode(node);
}


AptNode*
SecondPass::parseDef(const Token& expr)
{
  assert(expr.count() >= 2);
  assert(expr[0] == kLetId || expr[0] == kDefId);

  if (expr[1] == Parser::typeToken)
    return parseTypeDef(expr, false);

  else if (expr[1] == Parser::classToken)
    return parseTypeDef(expr, true);

  else if (expr[1] == Parser::aliasToken)
    return parseAliasDef(expr);

  else if (expr[1] == Parser::slotToken)
    return parseSlotDef(expr);

  else if (expr[1] == Parser::enumToken)
    return parseEnumDef(expr);

  else if (expr[1] == Parser::measureToken)
    return parseMeasureDef(expr);

  else if (expr[1] == Parser::unitToken)
    return parseUnitDef(expr);

  else if (expr[1] == Parser::constToken)
    return parseVarDef(expr, kConstVar, 2);
  else if (expr[1] == Parser::fluidToken)
    return parseVarDef(expr, kFluidVar, 2);
  else if (expr[1] == Parser::configToken)
    return parseVarDef(expr, kConfigVar, 2);

  else if (expr[1] == Parser::genericToken)
    return parseFunctionDef(expr);

  else if (expr[1] == Parser::charToken) {
    // TODO
    return NULL;
  }

  else if (expr[1] == Parser::macroToken) {
    assert(0);                  // should never come here actually
    return NULL;
  }

  else if (expr[1] == kSymbol) {
    if (expr.count() >= 3) {
      if (expr[2].isNested())
        return parseFunctionDef(expr);

      assert(expr[2] == kAssign || expr[2] == kColon);
      return parseVarDef(expr, kNormalVar, 1);
    }

    return parseVarDef(expr, kNormalVar, 1);
  }

  assert(0);

  return NULL;
}


AptNode*
SecondPass::parseIf(const Token& expr)
{
  assert(expr.count() >= 3);
  assert(expr[0] == kIfId);
  assert(expr[1].isNested());
  assert(expr[1].count() > 0);

  Ptr<AptNode> test = parseTokenVector(expr[1].children());
  Ptr<AptNode> consequent = parseExpr(expr[2]);
  Ptr<AptNode> alternate;

  if (expr.count() >= 4) {
    assert(expr[3] == kElseId);
    alternate = parseExpr(expr[4]);
  }

  return new IfNode(expr.srcpos(),
                    test, consequent, alternate);
}


AptNode*
SecondPass::parseParameter(const Token& expr)
{
  if (expr == kSymbol)
    return new ParamNode(expr.srcpos(),
                         String(), expr.idValue(), kPosArg, NULL, NULL);
  assert(expr.isSeq());
  assert(expr.count() > 0);

  int ofs = 0;
  const TokenVector& seq = expr.children();

  String key;
  ParamFlags paramType = kPosArg;
  if (seq[ofs] == kKeyarg) {
    key = seq[ofs].idValue();

    assert(expr.count() >= 2);
    ofs++;

    paramType = kNamedArg;
  }

  assert(seq[ofs] == kSymbol);

  String sym = seq[ofs].idValue();
  ofs++;

  Ptr<AptNode> type;
  if (ofs + 1 < expr.count()) {
    if (seq[ofs] == kColon) {
      type = parseTypeSpec(seq[ofs + 1]);
      ofs += 2;
    }
    else if (seq[ofs] == kAt) {
      type = parseTypeSpec(seq[ofs + 1]);
      ofs += 2;

      if (paramType == kNamedArg)
        errorf(expr.srcpos(), E_SpecNamedParam,
               "Specialized named parameters are not allowed");
      else
        paramType = kSpecArg;
    }
  }

  Ptr<AptNode> initExpr;
  if (ofs < expr.count()) {
    if (seq[ofs] == kAssign) {
      assert(ofs + 1 < expr.count());

      initExpr = parseExpr(seq[ofs + 1]);
      ofs += 2;

      paramType = kNamedArg;
      if (key.isEmpty())
        key = sym;
    }
    else if (seq[ofs] == kEllipsis) {
      assert(key.isEmpty());
      ofs++;

      paramType = kRestArg;
    }
  }

  return new ParamNode(expr.srcpos(), key, sym, paramType, type, initExpr);
}


void
SecondPass::parseParameters(NodeList* parameters, const TokenVector& seq)
{
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    Ptr<AptNode> param = parseParameter(seq[i]);
    if (param)
      parameters->push_back(param);
  }
}


AptNode*
SecondPass::parseOn(const Token& expr)
{
  assert(expr.count() == 4);
  assert(expr[0] == kOnId);
  assert(expr[1] == kSymbol);
  assert(expr[2].isNested());

  NodeList params;
  parseParameters(&params, expr[2].children());

  return new OnNode(expr.srcpos(),
                    expr[1].idValue(), params,
                    parseExpr(expr[3]));
}


AptNode*
SecondPass::parseClosure(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseBinary(const Token& expr)
{
  assert(expr.count() >= 3);

  switch (expr[1].tokenType()) {
  case kAssign:
    return new AssignNode(expr.srcpos(),
                          parseExpr(expr[0]),
                          parseExpr(expr[2]));
  case kRange:
    if (expr.count() >= 5) {
      assert(expr[3] == kBy);
      return new RangeNode(expr.srcpos(),
                           parseExpr(expr[0]),
                           parseExpr(expr[2]),
                           parseExpr(expr[4]));
    }
    else
      return new RangeNode(expr.srcpos(),
                           parseExpr(expr[0]),
                           parseExpr(expr[1]),
                           NULL);
  default:
    ;
  }

  return new BinaryNode(expr.srcpos(),
                        parseExpr(expr[0]),
                        tokenTypeToOperator(expr[1].tokenType()),
                        parseExpr(expr[2]));
}


AptNode*
SecondPass::parseFunCall(const Token& expr)
{
  assert(expr.isSeq());
  assert(expr.count() == 2);
  assert(expr[1].isNested());
  assert(expr[1].leftToken() == kParanOpen);
  assert(expr[1].rightToken() == kParanClose);

  Ptr<AptNode> first = parseExpr(expr[0]);
  Ptr<AptNode> funcall = new ApplyNode(expr.srcpos(), first);

  const TokenVector& seq = expr[1].children();
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    Ptr<AptNode> arg = parseExpr(seq[i]);

    funcall->appendNode(arg);
  }

  return funcall.release();
}


AptNode*
SecondPass::parseFor(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseSelect(const Token& expr)
{
  // TODO
  return NULL;
}


AptNode*
SecondPass::parseMatch(const Token& expr)
{
  // TODO
  return NULL;
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseTokenVector(const TokenVector& seq)
{
  if (!seq.empty())
    return parseSeq(Token() << seq);
  return NULL;
}


AptNode*
SecondPass::parseSeq(const Token& expr)
{
  assert(expr.isSeq());
  if (expr.isEmpty())
    return NULL;

  Token first = expr[0];
  if (first == kModuleId)
    return parseModule(expr);
  else if (first == kExportId)
    return parseExport(expr);
  else if (first == kImportId)
    return parseImport(expr);

  else if (first == kDefId || first == kLetId) {
    Ptr<AptNode> node = parseDef(expr);
    if (node != NULL)
      return newDefNode(node, first == kLetId);
    return NULL;
  }
  else if (first == kIfId)
    return parseIf(expr);
  else if (first == kOnId)
    return parseOn(expr);
  else if (first == kFunctionId)
    return parseClosure(expr);
  else if (first == kForId)
    return parseFor(expr);
  else if (first == kSelectId)
    return parseSelect(expr);
  else if (first == kMatchId)
    return parseMatch(expr);
  else if (expr.isBinarySeq())
    return parseBinary(expr);
  else if (expr.count() == 2) {
    if (expr[1].isNested())
      return parseFunCall(expr);
  }

  return parseExpr(expr[0]);
}


AptNode*
SecondPass::parseBlock(const Token& expr)
{
  assert(expr.isNested());
  assert(expr.leftToken() == kBraceOpen);
  assert(expr.rightToken() == kBraceClose);

  if (expr.count() == 0) {
    return new SymbolNode(expr.srcpos(), String("unspecified"));
  }
  else if (expr.count() == 1) {
    return parseExpr(expr[0]);
  }
  else {
    const TokenVector& seq = expr.children();
    Ptr<BlockNode> block = new BlockNode(expr.srcpos());
    for (size_t i = 0; i < seq.size(); i++) {
      Ptr<AptNode> item = parseExpr(seq[i]);
      if (item != NULL)
        block->appendNode(item);
    }

    return block.release();
  }
}


AptNode*
SecondPass::parseLiteralVector(const Token& expr)
{
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralVectorOpen);
  assert(expr.rightToken() == kParanClose);

  Ptr<AptNode> vector = new VectorNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    Ptr<AptNode> item = parseExpr(seq[i]);
    vector->appendNode(item);
  }

  return vector.release();
}


AptNode*
SecondPass::parseLiteralArray(const Token& expr)
{
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralArrayOpen);
  assert(expr.rightToken() == kBracketClose);

  Ptr<AptNode> array = new ArrayNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    Ptr<AptNode> item = parseExpr(seq[i]);
    array->appendNode(item);
  }

  return array.release();
}


AptNode*
SecondPass::parseLiteralDict(const Token& expr)
{
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralVectorOpen);
  assert(expr.rightToken() == kParanClose);

  Ptr<DictNode> dict = new DictNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    assert(seq[i].isBinarySeq(kMapTo));

    Ptr<AptNode> key = parseExpr(seq[i][0]);
    Ptr<AptNode> value = parseExpr(seq[i][2]);

    dict->addPair(key, value);
  }

  return dict.release();
}


AptNode*
SecondPass::parseNested(const Token& expr)
{
  assert(expr.isNested());

  switch (expr.leftToken()) {
  case kBraceOpen:
    return parseBlock(expr);

  case kLiteralVectorOpen:
    if (expr.count() > 0 && expr[0].isBinarySeq(kMapTo))
      return parseLiteralDict(expr);
    else
      return parseLiteralVector(expr);

  case kLiteralArrayOpen:
    return parseLiteralArray(expr);

  case kParanOpen:
  case kBracketOpen:
  case kGenericOpen:

  default:
    assert(0);                  // you should not be here.
  }

  return NULL;
}


AptNode*
SecondPass::parseExpr(const Token& expr)
{
  switch (expr.type()) {
  case kId:
    return new SymbolNode(expr.srcpos(), expr.idValue());

  case kLit:
    switch (expr.tokenType()) {
    case kBool:
    case kInt:
      return new IntNode(expr.srcpos(), expr.intValue(), expr.isImaginary());
    case kRational:
      return new RationalNode(expr.srcpos(), expr.rationalValue(), expr.isImaginary());
    case kReal:
      return new RealNode(expr.srcpos(), expr.realValue(), expr.isImaginary());
    case kChar:
      return new CharNode(expr.srcpos(), expr.charValue());
    case kString:
      return new StringNode(expr.srcpos(), expr.stringValue());
    case kKeyword:
      return new KeywordNode(expr.srcpos(), expr.stringValue());

    case kDocString:
      // TODO
      return NULL;

    default:
      assert(0);
    }
    break;

  case kSeq:
    return parseSeq(expr);

  case kNested:                 // TODO
    return parseNested(expr);

  case kPunct:                  // TODO
    errorf(expr.srcpos(), E_UnexpectedToken,
           "Unexpected token");
    return NULL;
  }

  return NULL;
}


AptNode*
SecondPass::parse(const Token& exprs)
{
  assert(exprs.isSeq());

  Ptr<CompileUnitNode> node = new CompileUnitNode(SrcPos());

  parseTopExprlist(node, exprs);

  return node.release();
}
