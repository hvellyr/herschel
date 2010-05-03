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


AptNode*
SecondPass::parseModule(const Pexpr& expr, bool isModule)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0].isId("module") || expr[0].isId("interface"));
  assert(expr[1].isId());

  String modName = expr[1].strValue();
  String publicId;

  if (expr.count() > 2) {
    assert(expr[2].isNested());
    assert(expr[2].count() == 1);
    assert(expr[2][0].isLit());
    assert(expr[2][0].tokenValue().fType == kString);

    publicId = expr[2][0].tokenValue().fStrValue;
  }

  return new ModuleNode(modName, publicId, isModule);
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

  for (std::vector<Pexpr>::const_iterator it = exprs.children().begin();
       it != exprs.children().end();
       it++)
  {
    Ptr<AptNode> n = parseExpr(*it);
    if (n != NULL)
      node->appendNode(n);
  }

  return node.release();
}
