/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.

   - look up all name references and complete their namespaces
 */

#include "typify.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"
#include "str.h"


using namespace heather;

//----------------------------------------------------------------------------

Typifier::Typifier()
  : fPhase(kTypify)
{
}


void
Typifier::typifyRecursively(AptNode* node)
{
  fPhase = kTypify;
  typifyNode(node);

  fPhase = kCheck;
  typifyNode(node);
}


void
Typifier::typifyNode(AptNode* node)
{
  node->typify(this);
}


void
Typifier::typify(CompileUnitNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typifyNodeList(NodeList& nl)
{
  for (size_t i = 0; i < nl.size(); i++)
    typifyNode(nl[i]);
}


//------------------------------------------------------------------------------

void
Typifier::typify(SymbolNode* node)
{
  // TODO
}


void
Typifier::typify(ArraySymbolNode* node)
{
  //TODO
}


//------------------------------------------------------------------------------

void
Typifier::typify(DefNode* node)
{
  typifyNode(node->defNode());
}


void
Typifier::typify(LetNode* node)
{
  typifyNode(node->defNode());
}


void
Typifier::typify(VardefNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());

  if (fPhase == kTypify) {
    assert(node->scope() != NULL);
    String typenm = ( node->type().isDef()
                      ? node->type().typeName()
                      : Type::kAnyTypeName );
    Type varty = node->scope()->lookupType(typenm, true);
    if (!varty.isDef()) {
      errorf(node->srcpos(), E_UndefinedType,
             "undefined type '%s'", (const char*)StrHelper(typenm));
      node->scope()->dumpDebug();
    }
    else {
      assert(varty.isDef());
      node->setType(varty);

      if (node->initExpr() != NULL) {
        if (varty.isAny()) {
          // infer the vardef type from the init expression
          node->setType(node->initExpr()->type());
        }
        else if (!node->initExpr()->type().isDef()) {
          errorf(node->initExpr()->srcpos(), E_TypeMismatch,
                 "Undefined type in variable initialization");
        }
        else if (!isContravariant(varty, node->initExpr()->type(),
                                  node->scope(), node->srcpos())) {
          errorf(node->initExpr()->srcpos(), E_TypeMismatch,
                 "type mismatch in variable initialization");
        }
        else {
          // infer the vardef type from the init expression
          node->setType(node->initExpr()->type());
        }
      }
    }
  }
}


void
Typifier::typify(FuncDefNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());
}


void
Typifier::typify(FunctionNode* node)
{
  typifyNodeList(node->params());
  if (node->body() != NULL)
    typifyNode(node->body());
}


void
Typifier::typify(SlotdefNode* node)
{
  // TODO
}


void
Typifier::typify(BlockNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(ParamNode* node)
{
  if (node->initExpr() != NULL)
    typifyNode(node->initExpr());
}


void
Typifier::typify(ApplyNode* node)
{
  typifyNode(node->base());
  typifyNodeList(node->children());
}


void
Typifier::typify(ArrayNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(AssignNode* node)
{
  typifyNode(node->lvalue());
  typifyNode(node->rvalue());
}


void
Typifier::typify(BinaryNode* node)
{
  typifyNode(node->left());
  typifyNode(node->right());
}


void
Typifier::typify(NegateNode* node)
{
  typifyNode(node->base());
}


void
Typifier::typify(IfNode* node)
{
  typifyNode(node->test());
  typifyNode(node->consequent());
  if (node->alternate())
    typifyNode(node->alternate());
}


void
Typifier::typify(KeyargNode* node)
{
  typifyNode(node->value());
}


void
Typifier::typify(MatchNode* node)
{
  typifyNode(node->expr());
  for (size_t i = 0; i < node->mappings().size(); i++) {
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(SelectNode* node)
{
  typifyNode(node->test());
  if (node->comparator() != NULL)
    typifyNode(node->comparator());

  for (size_t i = 0; i < node->mappings().size(); i++) {
    if (node->mappings()[i].fTestValues.empty()) {
      typifyNode(node->mappings()[i].fConsequent);
    }
    else {
      for (size_t j = 0; j < node->mappings()[i].fTestValues.size(); j++)
        typifyNode(node->mappings()[i].fTestValues[j]);
    }
    typifyNode(node->mappings()[i].fConsequent);
  }
}


void
Typifier::typify(OnNode* node)
{
  typifyNodeList(node->params());
  typifyNode(node->body());
}


void
Typifier::typify(RangeNode* node)
{
  typifyNode(node->from());
  typifyNode(node->to());
  if (node->by() != NULL)
    typifyNode(node->by());
}


void
Typifier::typify(ThenWhileNode* node)
{
  typifyNode(node->first());
  typifyNode(node->step());
  typifyNode(node->test());
}


void
Typifier::typify(TypeDefNode* node)
{
  // TODO
}


void
Typifier::typify(WhileNode* node)
{
  typifyNode(node->test());
  typifyNode(node->body());
}


void
Typifier::typify(VectorNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(DictNode* node)
{
  typifyNodeList(node->children());
}


void
Typifier::typify(CastNode* node)
{
  typifyNode(node->base());
}


//------------------------------------------------------------------------------

namespace heather
{
  void
  typifyNodeType(AptNode* node, const Type& type, const String& defaultTypeName)
  {
    String typenm = ( type.isDef()
                      ? type.typeName()
                      : defaultTypeName );
    Type ty = node->scope()->lookupType(typenm, true);
    if (!ty.isDef()) {
      errorf(node->srcpos(), E_UndefinedType,
             "undefined type '%s'", (const char*)StrHelper(typenm));
      node->setType(Type::newAny(true));
    }
    else
      node->setType(ty);
  }
};

void
Typifier::typify(BoolNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Type::kBoolTypeName);
  }
}


void
Typifier::typify(CharNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Type::kCharTypeName);
  }
}


void
Typifier::typify(RationalNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Type::kRationalTypeName);
  }
}


void
Typifier::typify(RealNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Type::kRealTypeName);
  }
}


void
Typifier::typify(IntNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, node->type(), Type::kIntTypeName);
  }
}


void
Typifier::typify(StringNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Type::kStringTypeName);
  }
}


void
Typifier::typify(KeywordNode* node)
{
  if (fPhase == kTypify) {
    typifyNodeType(node, Type(), Type::kKeywordTypeName);
  }
}


void
Typifier::typify(UnitConstNode* node)
{
  typifyNode(node->value());
}
