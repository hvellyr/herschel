/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"
#include "annotate.h"
#include "scope.h"
#include "symbol.h"

//----------------------------------------------------------------------------

using namespace heather;

Annotator::Annotator()
{
}


void
Annotator::annotateNode(AptNode* node, Scope* scope)
{
  node->annotate(this, scope);
}


void
Annotator::annotate(CompileUnitNode* node, Scope* scope)
{
  Ptr<Scope> newScope = new Scope(scope);

  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++) {
    annotateNode(nl[i], newScope);
  }
}


//------------------------------------------------------------------------------

void
Annotator::annotate(AptNode* node, Scope* scope)
{
}


void
Annotator::annotate(SymbolNode* node, Scope* scope)
{
}


//------------------------------------------------------------------------------

void
Annotator::annotate(DefNode* node, Scope* scope)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    annotate(vardefNode, scope, false);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    annotate(funcNode, scope, false);
    return;
  }

  // TODO
  assert(0);
}


void
Annotator::annotate(LetNode* node, Scope* scope)
{
  VardefNode* vardefNode = dynamic_cast<VardefNode*>(node->defNode());
  if (vardefNode != NULL) {
    annotate(vardefNode, scope, true);
    return;
  }

  FuncDefNode* funcNode = dynamic_cast<FuncDefNode*>(node->defNode());
  if (funcNode != NULL) {
    annotate(funcNode, scope, true);
    return;
  }

  // TODO
  assert(0);
}


void
Annotator::annotate(VardefNode* node, Scope* scope, bool isLocal)
{
  printf("Register var: %s (scope: %p)\n", (const char*)StrHelper(node->symbolName()),
         scope);
  assert(!isLocal || !isQualified(node->symbolName()));

  if (scope->checkForRedefinition(node->srcpos(),
                                  Scope::kNormal, node->symbolName()))
    return;

  scope->registerVar(node->srcpos(), node->symbolName(), node);
}


void
Annotator::annotate(FuncDefNode* node, Scope* scope, bool isLocal)
{
  printf("Register function: %s (scope: %p)\n", (const char*)StrHelper(node->funcName()),
         scope);
  assert(!isLocal || !isQualified(node->funcName()));

  if (scope->checkForRedefinition(node->srcpos(),
                                  Scope::kNormal, node->funcName()))
    return;

  scope->registerFunction(node->srcpos(), node->funcName(), node);

  Ptr<Scope> bodyScope = new Scope(scope);
  for (size_t pidx = 0; pidx < node->params().size(); pidx++)
    annotateNode(node->params()[pidx], bodyScope);

  annotateNode(node->body(), bodyScope);
}


void
Annotator::annotate(BlockNode* node, Scope* scope)
{
  Ptr<Scope> newScope = new Scope(scope);

  NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++) {
    annotateNode(nl[i], newScope);
  }
}


void
Annotator::annotate(ParamNode* node, Scope* scope)
{
  printf("Register param: %s (scope: %p)\n", (const char*)StrHelper(node->symbolName()),
         scope);
  assert(!isQualified(node->symbolName()));

  if (scope->checkForRedefinition(node->srcpos(),
                                  Scope::kNormal, node->symbolName()))
    return;

  scope->registerVar(node->srcpos(), node->symbolName(), node);
}


//------------------------------------------------------------------------------
