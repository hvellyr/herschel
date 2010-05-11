/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "tokenizer.h"
#include "pass2.h"
#include "pexpr.h"


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
SecondPass::parseModule(const Token& expr, bool isModule)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isSymbol("module") || expr[0].isSymbol("interface"));
  assert(expr[1].isSymbol());

  String modName = expr[1].idValue();
  String publicId;

  if (expr.count() > 2) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);
    assert(expr[2].count() == 1);
    assert(expr[2][0].isString());

    publicId = expr[2][0].stringValue();
  }

  Ptr<ModuleNode> modNode = new ModuleNode(modName, publicId, isModule);

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
  assert(expr[0].isSymbol("export"));
  assert(expr[expr.count() - 1].isNested());
  
  StringList flags;
  for (int i = 1; i < expr.count() - 1; i++) {
    assert(expr[i].isSymbol());
    flags.push_back(expr[i].idValue());
  }

  StringList symbols;
  Token symbolExprs = expr[expr.count() - 1];
  for (int j = 0; j < symbolExprs.count(); j++) {
    assert(symbolExprs[j].isSymbol());
    symbols.push_back(symbolExprs[j].idValue());
  }

  return new ExportNode(flags, symbols);
}


AptNode*
SecondPass::parseImport(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isSymbol("import"));
  assert(expr[1].isString());

  String codeFile = expr[1].stringValue();
  StringStringMap renames;

  if (expr.count() >= 3) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);

    Token renExprs = expr[2];
    for (int i = 0; i < renExprs.count(); i++) {
      Token renExpr = renExprs[i];
      assert(renExpr.isBinarySeq(kMapTo));
      assert(renExpr[0].isSymbol());
      assert(renExpr[2].isSymbol());

      renames.insert(std::make_pair(
                       renExpr[0].idValue(), renExpr[2].idValue()));
    }
  }

  return new ImportNode(codeFile, renames);
}


AptNode*
SecondPass::parseSeq(const Token& expr)
{
  assert(expr.isSeq());
  if (expr.isEmpty())
    return NULL;

  Token first = expr[0];
  if (first.isSymbol("module"))
    return parseModule(expr, true);
  else if (first.isSymbol("interface"))
    return parseModule(expr, false);
  else if (first.isSymbol("export"))
    return parseExport(expr);
  else if (first.isSymbol("import"))
    return parseImport(expr);

  // TODO
  return NULL;
}


AptNode*
SecondPass::parseExpr(const Token& expr)
{
  switch (expr.type()) {
  case kId:                     // TODO
    return NULL;
  case kLit:                    // TODO
    return NULL;
  case kSeq:
    return parseSeq(expr);
  case kNested:                 // TODO
    return NULL;
  case kPunct:                  // TODO
    throw UnexpectedTokenException(expr);
  }

  return NULL;
}


AptNode*
SecondPass::parse(const Token& exprs)
{
  assert(exprs.isSeq());

  Ptr<CompileUnitNode> node = new CompileUnitNode;

  parseTopExprlist(node, exprs);

  return node.release();
}
