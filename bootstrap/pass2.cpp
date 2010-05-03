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
SecondPass::parseTopExprlist(AptNode* rootNode, const Pexpr& expr)
{
  for (std::vector<Pexpr>::const_iterator it = expr.children().begin();
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
SecondPass::parseModule(const Pexpr& expr, bool isModule)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isId("module") || expr[0].isId("interface"));
  assert(expr[1].isId());

  String modName = expr[1].idValue();
  String publicId;

  if (expr.count() > 2) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);
    assert(expr[2].count() == 1);
    assert(expr[2][0].isStringLit());

    publicId = expr[2][0].tokenValue().fStrValue;
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
SecondPass::parseExport(const Pexpr& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isId("export"));
  assert(expr[expr.count() - 1].isNested());
  
  StringList flags;
  for (int i = 1; i < expr.count() - 1; i++) {
    assert(expr[i].isId());
    flags.push_back(expr[i].idValue());
  }

  StringList symbols;
  Pexpr symbolExprs = expr[expr.count() - 1];
  for (int j = 0; j < symbolExprs.count(); j++) {
    assert(symbolExprs[j].isId());
    symbols.push_back(symbolExprs[j].idValue());
  }

  return new ExportNode(flags, symbols);
}


AptNode*
SecondPass::parseImport(const Pexpr& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isId("import"));
  assert(expr[1].isStringLit());

  String codeFile = expr[1].tokenValue().fStrValue;
  StringStringMap renames;

  if (expr.count() >= 3) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);

    Pexpr renExprs = expr[2];
    for (int i = 0; i < renExprs.count(); i++) {
      Pexpr renExpr = renExprs[i];
      assert(renExpr.isBinarySeq(kMapTo));
      assert(renExpr[0].isId());
      assert(renExpr[2].isId());

      renames.insert(std::make_pair(
                       renExpr[0].idValue(), renExpr[2].idValue()));
    }
  }

  return new ImportNode(codeFile, renames);
}


AptNode*
SecondPass::parseSeq(const Pexpr& expr)
{
  assert(expr.isSeq());
  if (expr.isEmpty())
    return NULL;

  Pexpr first = expr[0];
  if (first.isId(String("module")))
    return parseModule(expr, true);
  else if (first.isId(String("interface")))
    return parseModule(expr, false);
  else if (first.isId(String("export")))
    return parseExport(expr);
  else if (first.isId(String("import")))
    return parseImport(expr);

  // TODO
  return NULL;
}


AptNode*
SecondPass::parseExpr(const Pexpr& expr)
{
  switch (expr.type()) {
  case kId:                     // TODO
  case kLit:                    // TODO
  case kSeq:
    return parseSeq(expr);
  case kNested:                 // TODO
  case kPunct:                  // TODO
    throw UnexpectedTokenException(Token(expr.punctValue()));
  }

  return NULL;
}


AptNode*
SecondPass::parse(const Pexpr& exprs)
{
  assert(exprs.isSeq());

  Ptr<CompileUnitNode> node = new CompileUnitNode;

  parseTopExprlist(node, exprs);

  return node.release();
}
