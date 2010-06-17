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

  Ptr<ModuleNode> modNode = new ModuleNode(expr[1].srcpos(),
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
  return NULL;
  // assert(expr.isSeq() && expr.count() >= 2);
  // assert(expr[0] == kExportId);
  // assert(expr[expr.count() - 1].isNested());

  // StringList flags;
  // for (int i = 1; i < expr.count() - 1; i++) {
  //   assert(expr[i].isSymbol());
  //   flags.push_back(expr[i].idValue());
  // }

  // StringList symbols;
  // Token symbolExprs = expr[expr.count() - 1];
  // for (int j = 0; j < symbolExprs.count(); j++) {
  //   if (symbolExprs[j].isSymbol())
  //     symbols.push_back(symbolExprs[j].idValue());
  // }

  // return new ExportNode(flags, symbols);
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

  return new ImportNode(codeFile, renames);
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

  Ptr<CompileUnitNode> node = new CompileUnitNode;

  parseTopExprlist(node, exprs);

  return node.release();
}
