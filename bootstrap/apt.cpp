/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "sysconf.h"

#include <string.h>

#include "annotate.h"
#include "apt.h"
#include "codegen.h"
#include "scope.h"
#include "strbuf.h"
#include "xmlout.h"

#include "llvm/Value.h"

#include <string>
#include <map>

using namespace heather;


template<typename T>
T* nodeClone(T* node)
{
  if (node != NULL)
    return node->clone();
  return NULL;
}


template<typename T>
T* nodeClone(const Ptr<T>& node)
{
  if (node != NULL)
    return node->clone();
  return NULL;
}


void
copyNodes(NodeList* dst, const NodeList* src)
{
  for (NodeList::const_iterator it = src->begin(); it != src->end(); ++it) {
    dst->push_back(nodeClone(it->obj()));
  }
}


NodeList
copyNodes(const NodeList& src)
{
  NodeList dst;
  copyNodes(&dst, &src);
  return dst;
}


//----------------------------------------------------------------------------

AptNode::AptNode(const SrcPos& srcpos, Scope* scope)
  : fSrcPos(srcpos),
    fScope(scope)
{
}


const SrcPos&
AptNode::srcpos() const
{
  return fSrcPos;
}


Scope*
AptNode::scope()
{
  return fScope;
}


NodeList&
AptNode::children()
{
  return fChildren;
}


const NodeList&
AptNode::children() const
{
  return fChildren;
}


void
AptNode::appendNode(AptNode* node)
{
  fChildren.push_back(node);
}


void
AptNode::appendNodes(const NodeList& nodes)
{
  fChildren.insert(fChildren.end(), nodes.begin(), nodes.end());
}


llvm::Value*
AptNode::codegen(CodeGenerator* generator) const
{
  return NULL;
}


//----------------------------------------------------------------------------

StringNode::StringNode(const SrcPos& srcpos, Scope* scope,
                       const String& value)
  : AptNode(srcpos, scope),
    fValue(value)
{
}


StringNode*
StringNode::clone() const
{
  return new StringNode(fSrcPos, fScope, fValue);
}


void
StringNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
StringNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
StringNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const SrcPos& srcpos, Scope* scope,
                         const String& value)
  :AptNode(srcpos, scope),
   fValue(value)
{
}


KeywordNode*
KeywordNode::clone() const
{
  return new KeywordNode(fSrcPos, fScope, fValue);
}


void
KeywordNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
KeywordNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
KeywordNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const SrcPos& srcpos, Scope* scope,
                       const String& value)
  : AptNode(srcpos, scope),
    fValue(value)
{
}


SymbolNode::SymbolNode(const SrcPos& srcpos, Scope* scope,
                       const String& value,
                       const TypeVector& generics)
  : AptNode(srcpos, scope),
    fValue(value),
    fGenerics(generics)
{ }


SymbolNode*
SymbolNode::clone() const
{
  return new SymbolNode(fSrcPos, fScope, fValue);
}


void
SymbolNode::setName(const String& nm)
{
  fValue = nm;
}


const String&
SymbolNode::name() const
{
  return fValue;
}


std::string
SymbolNode::string() const
{
  return std::string(StrHelper(fValue));
}


void
SymbolNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
SymbolNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
SymbolNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

ArraySymbolNode::ArraySymbolNode(const SrcPos& srcpos, Scope* scope,
                                 const String& value)
  : SymbolNode(srcpos, scope, value)
{ }


ArraySymbolNode*
ArraySymbolNode::clone() const
{
  return new ArraySymbolNode(fSrcPos, fScope, fValue);
}


void
ArraySymbolNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ArraySymbolNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
ArraySymbolNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, Scope* scope, int value,
                 bool isImaginary,
                 const Type& type)
  : NumberNode<int>(srcpos, scope, value, isImaginary, type)
{
}


IntNode*
IntNode::clone() const
{
  return new IntNode(fSrcPos, fScope, fValue, fIsImaginary, fType.clone());
}


llvm::Value*
IntNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
IntNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


void
IntNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, Scope* scope, double value,
                   bool isImaginary,
                   const Type& type)
  : NumberNode<double>(srcpos, scope, value, isImaginary, type)
{
}


RealNode*
RealNode::clone() const
{
  return new RealNode(fSrcPos, fScope, fValue, fIsImaginary, fType.clone());
}


void
RealNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
RealNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
RealNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const SrcPos& srcpos, Scope* scope,
                           const Rational& value, bool isImaginary,
                           const Type& type)
  : NumberNode<Rational>(srcpos, scope, value, isImaginary, type)
{
}


RationalNode*
RationalNode::clone() const
{
  return new RationalNode(fSrcPos, fScope, fValue,
                          fIsImaginary, fType.clone());
}


void
RationalNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
RationalNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
RationalNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

CharNode::CharNode(const SrcPos& srcpos, Scope* scope, Char value)
  : AptNode(srcpos, scope),
    fValue(value)
{ }


CharNode*
CharNode::clone() const
{
  return new CharNode(fSrcPos, fScope, fValue);
}


void
CharNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
CharNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
CharNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

BoolNode::BoolNode(const SrcPos& srcpos, Scope* scope, bool value)
  : AptNode(srcpos, scope),
    fValue(value)
{ }


BoolNode*
BoolNode::clone() const
{
  return new BoolNode(fSrcPos, fScope, fValue);
}


void
BoolNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
BoolNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
BoolNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

UnitConstNode::UnitConstNode(const SrcPos& srcpos, Scope* scope, AptNode* value,
                             const TypeUnit& unit)
  : AptNode(srcpos, scope),
    fValue(value),
    fUnit(unit)
{
}


UnitConstNode*
UnitConstNode::clone() const
{
  return new UnitConstNode(fSrcPos, fScope, nodeClone(fValue), fUnit);
}


void
UnitConstNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
UnitConstNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
UnitConstNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos, Scope* scope)
  : AptNode(srcpos, scope)
{}


CompileUnitNode*
CompileUnitNode::clone() const
{
  Ptr<CompileUnitNode> node = new CompileUnitNode(fSrcPos, fScope);
  copyNodes(&node->fChildren, &fChildren);
  return node.release();
}


void
CompileUnitNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
CompileUnitNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
CompileUnitNode::annotate(Annotator* annotator)
{
  annotator->annotate(this);
}


//----------------------------------------------------------------------------

BaseDefNode::BaseDefNode(const SrcPos& srcpos, Scope* scope, AptNode* defined)
  : AptNode(srcpos, scope),
    fDefined(defined)
{ }


AptNode*
BaseDefNode::defNode() const
{
  return fDefined;
}


//----------------------------------------------------------------------------

LetNode::LetNode(Scope* scope, AptNode* node)
  : BaseDefNode(node->srcpos(), scope, node)
{ }


LetNode*
LetNode::clone() const
{
  return new LetNode(fScope, nodeClone(fDefined.obj()));
}


void
LetNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
LetNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
LetNode::annotate(Annotator* annotator)
{
  annotator->annotate(this);
}


//----------------------------------------------------------------------------

DefNode::DefNode(Scope* scope, AptNode* node)
  : BaseDefNode(node->srcpos(), scope, node)
{ }


DefNode*
DefNode::clone() const
{
  return new DefNode(fScope, nodeClone(fDefined.obj()));
}


void
DefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
DefNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
DefNode::annotate(Annotator* annotator)
{
  annotator->annotate(this);
}


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos, Scope* scope,
                         const String& symbolName, const Type& type,
                         AptNode* initExpr)
  : AptNode(srcpos, scope),
    fSymbolName(symbolName),
    fType(type),
    fInitExpr(initExpr)
{ }


const String&
BindingNode::symbolName() const
{
  return fSymbolName;
}


const Type&
BindingNode::type() const
{
  return fType;
}


AptNode*
BindingNode::initExpr() const
{
  return fInitExpr;
}


//----------------------------------------------------------------------------

VardefNode::VardefNode(const SrcPos& srcpos, Scope* scope,
                       const String& symbolName, VardefFlags flags,
                       const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, scope, symbolName, type, initExpr),
    fFlags(flags)
{
}


VardefNode*
VardefNode::clone() const
{
  return new VardefNode(fSrcPos, fScope, fSymbolName, fFlags,
                        fType.clone(), nodeClone(fInitExpr));
}


void
VardefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


bool
VardefNode::isConst() const
{
  return fFlags == kConstVar;
}


bool
VardefNode::isConfig() const
{
  return fFlags == kConfigVar;
}


bool
VardefNode::isEnum() const
{
  return fFlags == kEnumVar;
}


llvm::Value*
VardefNode::codegen(CodeGenerator* generator) const
{
  // this should never be called directly.  See codegen::DefNode
  assert(0);
  return NULL;
}


void
VardefNode::annotate(Annotator* annotator)
{
  // this should never be called directly.  See Annotator::DefNode
  assert(0);
}



//----------------------------------------------------------------------------

ParamNode::ParamNode(const SrcPos& srcpos, Scope* scope,
                     const String& keyName,
                     const String& symbolName, ParamFlags flags,
                     const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, scope, symbolName, type, initExpr),
    fKey(keyName),
    fFlags(flags)
{
  assert(heaImplies(fFlags == kNamedArg, !fKey.isEmpty()));
}


ParamNode*
ParamNode::clone() const
{
  return new ParamNode(fSrcPos, fScope, fKey, fSymbolName, fFlags,
                       fType.clone(), nodeClone(fInitExpr));
}


void
ParamNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


ParamFlags
ParamNode::flags() const
{
  return fFlags;
}


bool
ParamNode::isRestArg() const
{
  return (fFlags & kRestArg) != 0;
}


const String&
ParamNode::key() const
{
  return fKey;
}


llvm::Value*
ParamNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
ParamNode::annotate(Annotator* annotator)
{
  annotator->annotate(this);
}


//----------------------------------------------------------------------------

SlotdefNode::SlotdefNode(const SrcPos& srcpos, Scope* scope,
                         const String& symbolName,
                         unsigned int flags,
                         const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, scope, symbolName, type, initExpr),
    fFlags(flags)
{ }


SlotdefNode*
SlotdefNode::clone() const
{
  return new SlotdefNode(fSrcPos, fScope, fSymbolName, fFlags,
                         fType.clone(), nodeClone(fInitExpr));
}


void
SlotdefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
SlotdefNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
SlotdefNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

ArrayNode*
ArrayNode::clone() const
{
  Ptr<ArrayNode> an = new ArrayNode(fSrcPos, fScope);
  copyNodes(&an->fChildren, &fChildren);
  return an.release();
}


void
ArrayNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ArrayNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
ArrayNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

VectorNode*
VectorNode::clone() const
{
  Ptr<VectorNode> vect = new VectorNode(fSrcPos, fScope);
  copyNodes(&vect->fChildren, &fChildren);
  return vect.release();
}


void
VectorNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
VectorNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
VectorNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

DictNode*
DictNode::clone() const
{
  Ptr<DictNode> dict = new DictNode(fSrcPos, fScope);
  copyNodes(&dict->fChildren, &fChildren);
  return dict.release();
}


void
DictNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


void
DictNode::addPair(AptNode* key, AptNode* value)
{
  assert(key != NULL);
  assert(value != NULL);

  appendNode(new BinaryNode(key->srcpos(), fScope, key, kOpMapTo, value));
}


llvm::Value*
DictNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
DictNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(const SrcPos& srcpos, Scope* scope,
                       AptNode* left, OperatorType op, AptNode* right)
  : AptNode(srcpos, scope),
    fLeft(left),
    fRight(right),
    fOp(op)
{
  assert(fOp != kOpInvalid);
}


BinaryNode*
BinaryNode::clone() const
{
  return new BinaryNode(fSrcPos, fScope,
                        nodeClone(fLeft), fOp, nodeClone(fRight));
}


OperatorType
BinaryNode::op() const
{
  return fOp;
}


AptNode*
BinaryNode::left() const
{
  return fLeft;
}


void
BinaryNode::setLeft(AptNode* node)
{
  fLeft = node;
}


AptNode*
BinaryNode::right() const
{
  return fRight;
}


void
BinaryNode::setRight(AptNode* node)
{
  fRight = node;
}


bool
BinaryNode::isMapTo() const
{
  return fOp == kOpMapTo;
}


void
BinaryNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
BinaryNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
BinaryNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

NegateNode::NegateNode(const SrcPos& srcpos, Scope* scope, AptNode* base)
  : AptNode(srcpos, scope),
    fBase(base)
{ }


const AptNode*
NegateNode::base() const
{
  return fBase;
}


AptNode*
NegateNode::base()
{
  return fBase;
}


NegateNode*
NegateNode::clone() const
{
  return new NegateNode(fSrcPos, fScope, nodeClone(fBase));
}


void
NegateNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
NegateNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
NegateNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//------------------------------------------------------------------------------

RangeNode::RangeNode(const SrcPos& srcpos, Scope* scope,
                     AptNode* from, AptNode* to, AptNode* by)
  : AptNode(srcpos, scope),
    fFrom(from),
    fTo(to),
    fBy(by)
{ }


RangeNode*
RangeNode::clone() const
{
  return new RangeNode(fSrcPos, fScope,
                       nodeClone(fFrom), nodeClone(fTo), nodeClone(fBy));
}


void
RangeNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


AptNode*
RangeNode::from() const
{
  return fFrom;
}


AptNode*
RangeNode::to() const
{
  return fTo;
}


AptNode*
RangeNode::by() const
{
  return fBy;
}


llvm::Value*
RangeNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
RangeNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//--------------------------------------------------------------------------

ThenWhileNode::ThenWhileNode(const SrcPos& srcpos, Scope* scope,
                             AptNode* first, AptNode* step, AptNode* test)
  : AptNode(srcpos, scope),
    fFirst(first),
    fStep(step),
    fTest(test)
{ }


ThenWhileNode*
ThenWhileNode::clone() const
{
  return new ThenWhileNode(fSrcPos, fScope,
                           nodeClone(fFirst),
                           nodeClone(fStep),
                           nodeClone(fTest));
}


void
ThenWhileNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ThenWhileNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
ThenWhileNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//--------------------------------------------------------------------------

AssignNode::AssignNode(const SrcPos& srcpos, Scope* scope,
                       AptNode* lvalue, AptNode* rvalue)
  : AptNode(srcpos, scope),
    fLValue(lvalue),
    fRValue(rvalue)
{ }


AssignNode*
AssignNode::clone() const
{
  return new AssignNode(fSrcPos, fScope,
                        nodeClone(fLValue),
                        nodeClone(fRValue));
}


AptNode*
AssignNode::lvalue() const
{
  return fLValue;
}


AptNode*
AssignNode::rvalue() const
{
  return fRValue;
}


void
AssignNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
AssignNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
AssignNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//------------------------------------------------------------------------------

IfNode::IfNode(const SrcPos& srcpos, Scope* scope,
               AptNode* test, AptNode* consequent, AptNode* alternate)
  : AptNode(srcpos, scope),
    fTest(test),
    fConsequent(consequent),
    fAlternate(alternate)
{ }


IfNode*
IfNode::clone() const
{
  return new IfNode(fSrcPos, fScope,
                    nodeClone(fTest),
                    nodeClone(fConsequent),
                    nodeClone(fAlternate));
}


void
IfNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


AptNode*
IfNode::test() const
{
  return fTest;
}


AptNode*
IfNode::consequent() const
{
  return fConsequent;
}


AptNode*
IfNode::alternate() const
{
  return fAlternate;
}


void
IfNode::setAlternate(AptNode* node)
{
  fAlternate = node;
}


llvm::Value*
IfNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
IfNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//------------------------------------------------------------------------------

SelectNode::SelectNode(const SrcPos& srcpos, Scope* scope,
                       AptNode* test, AptNode* comparator)
  : AptNode(srcpos, scope),
    fTest(test),
    fComparator(comparator)
{
}


SelectNode*
SelectNode::clone() const
{
  Ptr<SelectNode> newNode = new SelectNode(fSrcPos, fScope,
                                           nodeClone(fTest),
                                           nodeClone(fComparator));
  for (size_t i = 0; i < fMappings.size(); i++) {
    newNode->fMappings.push_back(
      SelectMapping(copyNodes(fMappings[i].fTestValues),
                    nodeClone(fMappings[i].fConsequent)));
  }

  return newNode.release();
}


void
SelectNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


void
SelectNode::addMapping(const NodeList& mappings, AptNode* consequent)
{
  fMappings.push_back(SelectMapping(mappings, consequent));
}


void
SelectNode::addMapping(AptNode* mapping, AptNode* consequent)
{
  NodeList nl;
  nl.push_back(mapping);
  fMappings.push_back(SelectMapping(nl, consequent));
}


void
SelectNode::addElseMapping(AptNode* alternate)
{
  NodeList nl;
  fMappings.push_back(SelectMapping(nl, alternate));
}


SelectNode::SelectMapping::SelectMapping(const NodeList& values,
                                         AptNode* consequent)
  : fTestValues(values),
    fConsequent(consequent)
{
}


SelectNode::SelectMapping::SelectMapping(const SelectMapping& other)
  : fTestValues(other.fTestValues),
    fConsequent(other.fConsequent)
{
}


llvm::Value*
SelectNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
SelectNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//------------------------------------------------------------------------------

MatchNode::MatchNode(const SrcPos& srcpos, Scope* scope, AptNode* expr)
  : AptNode(srcpos, scope),
    fExpr(expr)
{
}


MatchNode*
MatchNode::clone() const
{
  Ptr<MatchNode> newNode = new MatchNode(fSrcPos, fScope, nodeClone(fExpr));
  for (size_t i = 0; i < fMappings.size(); i++) {
    newNode->fMappings.push_back(
      MatchMapping(fMappings[i].fSrcPos,
                   fMappings[i].fVarName,
                   fMappings[i].fMatchType.clone(),
                   nodeClone(fMappings[i].fConsequent)));
  }

  return newNode.release();
}


void
MatchNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


void
MatchNode::addMapping(const SrcPos& srcpos, const String& varName,
                      const Type& matchType,
                      AptNode* consequent)
{
  fMappings.push_back(MatchMapping(srcpos, varName, matchType, consequent));
}


MatchNode::MatchMapping::MatchMapping(const SrcPos& srcpos,
                                      const String& varName,
                                      const Type& matchType,
                                      AptNode* consequent)
  : fSrcPos(srcpos),
    fVarName(varName),
    fMatchType(matchType),
    fConsequent(consequent)
{
}


MatchNode::MatchMapping::MatchMapping(const MatchMapping& other)
  : fSrcPos(other.fSrcPos),
    fVarName(other.fVarName),
    fMatchType(other.fMatchType),
    fConsequent(other.fConsequent)
{
}


llvm::Value*
MatchNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
MatchNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//------------------------------------------------------------------------------

OnNode::OnNode(const SrcPos& srcpos, Scope* scope,
               const String& key, const NodeList& params, AptNode* body)
  : AptNode(srcpos, scope),
    fKey(key),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


OnNode*
OnNode::clone() const
{
  return new OnNode(fSrcPos, fScope, fKey,
                    copyNodes(fParams),
                    nodeClone(fBody));
}


void
OnNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
OnNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
OnNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

BlockNode::BlockNode(const SrcPos& srcpos, Scope* scope)
  : AptNode(srcpos, scope)
{ }


BlockNode*
BlockNode::clone() const
{
  Ptr<BlockNode> block = new BlockNode(fSrcPos, fScope);
  copyNodes(&block->fChildren, &fChildren);
  return block.release();
}


void
BlockNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
BlockNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
BlockNode::annotate(Annotator* annotator)
{
  annotator->annotate(this);
}


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos&   srcpos,
                           Scope*          scope,
                           const NodeList& params,
                           const Type&     retType,
                           AptNode*        body)
  : AptNode(srcpos, scope),
    fRetType(retType),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


FunctionNode*
FunctionNode::clone() const
{
  return new FunctionNode(fSrcPos, fScope, copyNodes(fParams),
                          fRetType.clone(), nodeClone(fBody));
}


void
FunctionNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


const NodeList&
FunctionNode::params() const
{
  return fParams;
}


const Type&
FunctionNode::retType() const
{
  return fRetType;
}


llvm::Value*
FunctionNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
FunctionNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

FuncDefNode::FuncDefNode(const SrcPos& srcpos,
                         Scope* scope,
                         const String& sym,
                         unsigned int flags,
                         const NodeList& params,
                         const Type& retType,
                         AptNode* body)
  : FunctionNode(srcpos, scope, params, retType, body),
    fSym(sym),
    fFlags(flags)
{ }


FuncDefNode*
FuncDefNode::clone() const
{
  return new FuncDefNode(fSrcPos, fScope, fSym, fFlags, fParams,
                         fRetType.clone(), nodeClone(fBody));
}


void
FuncDefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


bool
FuncDefNode::isGeneric() const
{
  return (fFlags & kFuncIsGeneric) != 0;
}


bool
FuncDefNode::isAbstract() const
{
  return (fFlags & kFuncIsAbstract) != 0;
}


const String&
FuncDefNode::funcName() const
{
  return fSym;
}


llvm::Value*
FuncDefNode::codegen(CodeGenerator* generator) const
{
  // this should never be called directly.  See codegen::DefNode
  assert(0);
  return NULL;
}


void
FuncDefNode::annotate(Annotator* annotator)
{
  // this should never be called directly.  See Annotator::DefNode
  assert(0);
}



//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, Scope* scope, AptNode* base)
  : AptNode(srcpos, scope),
    fBase(base)
{ }


ApplyNode*
ApplyNode::clone() const
{
  Ptr<ApplyNode> apply = new ApplyNode(fSrcPos, fScope, nodeClone(fBase));
  copyNodes(&apply->fChildren, &fChildren);
  return apply.release();
}


AptNode*
ApplyNode::base() const
{
  return fBase;
}


void
ApplyNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ApplyNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
ApplyNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos, Scope* scope,
                       const String& key, AptNode* value)
  : AptNode(srcpos, scope),
    fKey(key),
    fValue(value)
{ }


KeyargNode*
KeyargNode::clone() const
{
  return new KeyargNode(fSrcPos, fScope, fKey, nodeClone(fValue));
}


void
KeyargNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
KeyargNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
KeyargNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

WhileNode::WhileNode(const SrcPos& srcpos, Scope* scope,
                     AptNode* test, AptNode* body)
  : AptNode(srcpos, scope),
    fTest(test),
    fBody(body)
{ }


WhileNode*
WhileNode::clone() const
{
  return new WhileNode(fSrcPos, fScope,
                       nodeClone(fTest),
                       nodeClone(fBody));
}


void
WhileNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
WhileNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


AptNode*
WhileNode::body() const
{
  return fBody;
}


AptNode*
WhileNode::test() const
{
  return fTest;
}


void
WhileNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

TypeDefNode::TypeDefNode(const SrcPos&   srcpos,
                         Scope*          scope,
                         const String&   typeName,
                         bool            isClass,
                         const Type&     isa,
                         const NodeList& params,
                         const NodeList& slots,
                         const NodeList& reqProtocol,
                         const NodeList& onExprs)
  : AptNode(srcpos, scope),
    fTypeName(typeName),
    fIsClass(isClass),
    fParams(params),
    fSlots(slots),
    fReqProtocol(reqProtocol),
    fOnExprs(onExprs),
    fIsa(isa)
{ }


TypeDefNode*
TypeDefNode::clone() const
{
  return new TypeDefNode(fSrcPos, fScope, fTypeName, fIsClass, fIsa.clone(),
                         copyNodes(fParams),
                         copyNodes(fSlots),
                         copyNodes(fReqProtocol),
                         copyNodes(fOnExprs));
}


void
TypeDefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
TypeDefNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
TypeDefNode::annotate(Annotator* an)
{
  an->annotate(this);
}


//----------------------------------------------------------------------------

CastNode::CastNode(const SrcPos& srcpos,
                   Scope* scope,
                   AptNode* base,
                   const Type& type)
  : AptNode(srcpos, scope),
    fBase(base),
    fType(type)
{ }

AptNode*
CastNode::base() const
{
  return fBase;
}


const Type&
CastNode::type() const
{
  return fType;
}


CastNode*
CastNode::clone() const
{
  return new CastNode(fSrcPos, fScope, nodeClone(fBase), fType.clone());
}


void
CastNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
CastNode::codegen(CodeGenerator* generator) const
{
  return generator->codegen(this);
}


void
CastNode::annotate(Annotator* an)
{
  an->annotate(this);
}


