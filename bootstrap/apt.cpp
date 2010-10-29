/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "sysconf.h"

#include <string.h>

#include "annotate.h"
#include "transform.h"
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

AptNode::AptNode(const SrcPos& srcpos)
  : fSrcPos(srcpos)
{
}


const SrcPos&
AptNode::srcpos() const
{
  return fSrcPos;
}


Scope*
AptNode::scope() const
{
  return fScope;
}


AptNode*
AptNode::setScope(Scope* scope)
{
  fScope = scope;
  return this;
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

namespace heather {
  template<typename T>
  T* cloneScope(const T* src, T* dst)
  {
    dst->setScope(src->scope());
    return dst;
  }
}


StringNode::StringNode(const SrcPos& srcpos,
                       const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


StringNode*
StringNode::clone() const
{
  return heather::cloneScope(this, new StringNode(fSrcPos, fValue));
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


AptNode*
StringNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const SrcPos& srcpos,
                         const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


KeywordNode*
KeywordNode::clone() const
{
  return cloneScope(this, new KeywordNode(fSrcPos, fValue));
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


AptNode*
KeywordNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const SrcPos& srcpos,
                       const String& value)
  : AptNode(srcpos),
    fValue(value),
    fRefersTo(kFreeVar),
    fIsShared(false)
{
}


SymbolNode::SymbolNode(const SrcPos& srcpos,
                       const String& value,
                       const TypeVector& generics)
  : AptNode(srcpos),
    fValue(value),
    fGenerics(generics),
    fRefersTo(kFreeVar),
    fIsShared(false)
{ }


SymbolNode*
SymbolNode::clone() const
{
  return cloneScope(this, new SymbolNode(fSrcPos, fValue));
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


SymReferType
SymbolNode::refersTo() const
{
  return fRefersTo;
}


void
SymbolNode::setRefersTo(SymReferType type, bool isShared)
{
  fRefersTo = type;
  fIsShared = isShared;
}


bool
SymbolNode::isShared() const
{
  return fIsShared;
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


AptNode*
SymbolNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

ArraySymbolNode::ArraySymbolNode(const SrcPos& srcpos,
                                 const String& value)
  : SymbolNode(srcpos, value)
{ }


ArraySymbolNode*
ArraySymbolNode::clone() const
{
  return cloneScope(this, new ArraySymbolNode(fSrcPos, fValue));
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


AptNode*
ArraySymbolNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int value,
                 bool isImaginary,
                 const Type& type)
  : NumberNode<int>(srcpos, value, isImaginary, type)
{
}


IntNode*
IntNode::clone() const
{
  return cloneScope(this,
                    new IntNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
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


AptNode*
IntNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, double value,
                   bool isImaginary,
                   const Type& type)
  : NumberNode<double>(srcpos, value, isImaginary, type)
{
}


RealNode*
RealNode::clone() const
{
  return cloneScope(this,
                    new RealNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
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


AptNode*
RealNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const SrcPos& srcpos,
                           const Rational& value, bool isImaginary,
                           const Type& type)
  : NumberNode<Rational>(srcpos, value, isImaginary, type)
{
}


RationalNode*
RationalNode::clone() const
{
  return cloneScope(this,
                    new RationalNode(fSrcPos, fValue,
                                     fIsImaginary, fType.clone()));
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


AptNode*
RationalNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

CharNode::CharNode(const SrcPos& srcpos, Char value)
  : AptNode(srcpos),
    fValue(value)
{ }


CharNode*
CharNode::clone() const
{
  return cloneScope(this, new CharNode(fSrcPos, fValue));
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


AptNode*
CharNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

BoolNode::BoolNode(const SrcPos& srcpos, bool value)
  : AptNode(srcpos),
    fValue(value)
{ }


BoolNode*
BoolNode::clone() const
{
  return cloneScope(this, new BoolNode(fSrcPos, fValue));
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


AptNode*
BoolNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

UnitConstNode::UnitConstNode(const SrcPos& srcpos, AptNode* value,
                             const TypeUnit& unit)
  : AptNode(srcpos),
    fValue(value),
    fUnit(unit)
{
}


UnitConstNode*
UnitConstNode::clone() const
{
  return cloneScope(this, new UnitConstNode(fSrcPos, nodeClone(fValue), fUnit));
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


AptNode*
UnitConstNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{}


CompileUnitNode*
CompileUnitNode::clone() const
{
  Ptr<CompileUnitNode> node = new CompileUnitNode(fSrcPos);
  copyNodes(&node->fChildren, &fChildren);
  return cloneScope(this, node.release());
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


AptNode*
CompileUnitNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

BaseDefNode::BaseDefNode(const SrcPos& srcpos, AptNode* defined)
  : AptNode(srcpos),
    fDefined(defined)
{ }


AptNode*
BaseDefNode::defNode() const
{
  return fDefined;
}


//----------------------------------------------------------------------------

LetNode::LetNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


LetNode*
LetNode::clone() const
{
  return cloneScope(this, new LetNode(nodeClone(fDefined.obj())));
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


AptNode*
LetNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

DefNode::DefNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


DefNode*
DefNode::clone() const
{
  return cloneScope(this, new DefNode(nodeClone(fDefined.obj())));
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


AptNode*
DefNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos,
                         const String& symbolName, const Type& type,
                         AptNode* initExpr)
  : AptNode(srcpos),
    fSymbolName(symbolName),
    fType(type),
    fInitExpr(initExpr),
    fAllocType(kAlloc_Local)
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


void
BindingNode::setAllocType(BindingAllocType allocType)
{
  fAllocType = allocType;
}


BindingAllocType
BindingNode::allocType() const
{
  return fAllocType;
}


//----------------------------------------------------------------------------

VardefNode::VardefNode(const SrcPos& srcpos,
                       const String& symbolName, VardefFlags flags,
                       bool isLocal,
                       const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fIsLocal(isLocal),
    fFlags(flags)
{
}


VardefNode*
VardefNode::clone() const
{
  Ptr<VardefNode> n = new VardefNode(fSrcPos, fSymbolName, fFlags,
                                     fIsLocal,
                                     fType.clone(), nodeClone(fInitExpr));
  n->setLinkage(fLinkage);
  return cloneScope(this, n.release());
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


bool
VardefNode::isLocal() const
{
  return fIsLocal;
}


VardefFlags
VardefNode::flags() const
{
  return fFlags;
}


const String&
VardefNode::linkage() const
{
  return fLinkage;
}


void
VardefNode::setLinkage(const String& linkage)
{
  fLinkage = linkage;
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


AptNode*
VardefNode::transform(Transformator* tr)
{
  return tr->transform(this);
}



//----------------------------------------------------------------------------

ParamNode::ParamNode(const SrcPos& srcpos,
                     const String& keyName,
                     const String& symbolName, ParamFlags flags,
                     const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fKey(keyName),
    fFlags(flags)
{
  assert(heaImplies(fFlags == kNamedArg, !fKey.isEmpty()));
}


ParamNode*
ParamNode::clone() const
{
  return cloneScope(this,
                    new ParamNode(fSrcPos, fKey, fSymbolName, fFlags,
                                  fType.clone(), nodeClone(fInitExpr)));
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


AptNode*
ParamNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

SlotdefNode::SlotdefNode(const SrcPos& srcpos,
                         const String& symbolName,
                         unsigned int flags,
                         const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fFlags(flags)
{ }


SlotdefNode*
SlotdefNode::clone() const
{
  return cloneScope(this,
                    new SlotdefNode(fSrcPos, fSymbolName, fFlags,
                                    fType.clone(), nodeClone(fInitExpr)));
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


AptNode*
SlotdefNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

ArrayNode::ArrayNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{ }


ArrayNode*
ArrayNode::clone() const
{
  Ptr<ArrayNode> an = new ArrayNode(fSrcPos);
  copyNodes(&an->fChildren, &fChildren);
  return cloneScope(this, an.release());
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


AptNode*
ArrayNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

VectorNode::VectorNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{ }


VectorNode*
VectorNode::clone() const
{
  Ptr<VectorNode> vect = new VectorNode(fSrcPos);
  copyNodes(&vect->fChildren, &fChildren);
  return cloneScope(this, vect.release());
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


AptNode*
VectorNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

DictNode::DictNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{ }


DictNode*
DictNode::clone() const
{
  Ptr<DictNode> dict = new DictNode(fSrcPos);
  copyNodes(&dict->fChildren, &fChildren);
  return cloneScope(this, dict.release());
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

  appendNode(new BinaryNode(key->srcpos(), key, kOpMapTo, value));
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


AptNode*
DictNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(const SrcPos& srcpos,
                       AptNode* left, OperatorType op, AptNode* right)
  : AptNode(srcpos),
    fLeft(left),
    fRight(right),
    fOp(op)
{
  assert(fOp != kOpInvalid);
}


BinaryNode*
BinaryNode::clone() const
{
  return cloneScope(this,
                    new BinaryNode(fSrcPos, nodeClone(fLeft),
                                   fOp, nodeClone(fRight)));
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


AptNode*
BinaryNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

NegateNode::NegateNode(const SrcPos& srcpos, AptNode* base)
  : AptNode(srcpos),
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
  return cloneScope(this, new NegateNode(fSrcPos, nodeClone(fBase)));
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


AptNode*
NegateNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//------------------------------------------------------------------------------

RangeNode::RangeNode(const SrcPos& srcpos,
                     AptNode* from, AptNode* to, AptNode* by)
  : AptNode(srcpos),
    fFrom(from),
    fTo(to),
    fBy(by)
{ }


RangeNode*
RangeNode::clone() const
{
  return cloneScope(this,
                    new RangeNode(fSrcPos, nodeClone(fFrom),
                                  nodeClone(fTo), nodeClone(fBy)));
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


AptNode*
RangeNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//--------------------------------------------------------------------------

ThenWhileNode::ThenWhileNode(const SrcPos& srcpos,
                             AptNode* first, AptNode* step, AptNode* test)
  : AptNode(srcpos),
    fFirst(first),
    fStep(step),
    fTest(test)
{ }


ThenWhileNode*
ThenWhileNode::clone() const
{
  return cloneScope(this, new ThenWhileNode(fSrcPos,
                                            nodeClone(fFirst),
                                            nodeClone(fStep),
                                            nodeClone(fTest)));
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


AptNode*
ThenWhileNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//--------------------------------------------------------------------------

AssignNode::AssignNode(const SrcPos& srcpos,
                       AptNode* lvalue, AptNode* rvalue)
  : AptNode(srcpos),
    fLValue(lvalue),
    fRValue(rvalue)
{ }


AssignNode*
AssignNode::clone() const
{
  return cloneScope(this, new AssignNode(fSrcPos,
                                         nodeClone(fLValue),
                                         nodeClone(fRValue)));
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
AssignNode::setLvalue(AptNode* val)
{
  fLValue = val;
}


void
AssignNode::setRvalue(AptNode* val)
{
  fRValue = val;
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


AptNode*
AssignNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//------------------------------------------------------------------------------

IfNode::IfNode(const SrcPos& srcpos,
               AptNode* test, AptNode* consequent, AptNode* alternate)
  : AptNode(srcpos),
    fTest(test),
    fConsequent(consequent),
    fAlternate(alternate)
{ }


IfNode*
IfNode::clone() const
{
  return cloneScope(this, new IfNode(fSrcPos,
                                     nodeClone(fTest),
                                     nodeClone(fConsequent),
                                     nodeClone(fAlternate)));
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


AptNode*
IfNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//------------------------------------------------------------------------------

SelectNode::SelectNode(const SrcPos& srcpos,
                       AptNode* test, AptNode* comparator)
  : AptNode(srcpos),
    fTest(test),
    fComparator(comparator)
{
}


SelectNode*
SelectNode::clone() const
{
  Ptr<SelectNode> newNode = new SelectNode(fSrcPos,
                                           nodeClone(fTest),
                                           nodeClone(fComparator));
  for (size_t i = 0; i < fMappings.size(); i++) {
    newNode->fMappings.push_back(
      SelectMapping(copyNodes(fMappings[i].fTestValues),
                    nodeClone(fMappings[i].fConsequent)));
  }

  return cloneScope(this, newNode.release());
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


AptNode*
SelectNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//------------------------------------------------------------------------------

MatchNode::MatchNode(const SrcPos& srcpos, AptNode* expr)
  : AptNode(srcpos),
    fExpr(expr)
{
}


MatchNode*
MatchNode::clone() const
{
  Ptr<MatchNode> newNode = new MatchNode(fSrcPos, nodeClone(fExpr));
  for (size_t i = 0; i < fMappings.size(); i++) {
    newNode->fMappings.push_back(
      MatchMapping(fMappings[i].fSrcPos,
                   fMappings[i].fVarName,
                   fMappings[i].fMatchType.clone(),
                   nodeClone(fMappings[i].fConsequent)));
  }

  return cloneScope(this, newNode.release());
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


AptNode*
MatchNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//------------------------------------------------------------------------------

OnNode::OnNode(const SrcPos& srcpos,
               const String& key, const NodeList& params, AptNode* body)
  : AptNode(srcpos),
    fKey(key),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


OnNode*
OnNode::clone() const
{
  return cloneScope(this, new OnNode(fSrcPos, fKey,
                                     copyNodes(fParams),
                                     nodeClone(fBody)));
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


AptNode*
OnNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


const String&
OnNode::key() const
{
  return fKey;
}


const AptNode*
OnNode::body() const
{
  return fBody;
}


AptNode*
OnNode::body()
{
  return fBody;
}


const NodeList&
OnNode::params() const
{
  return fParams;
}


NodeList&
OnNode::params()
{
  return fParams;
}


//----------------------------------------------------------------------------

BlockNode::BlockNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{ }


BlockNode*
BlockNode::clone() const
{
  Ptr<BlockNode> block = new BlockNode(fSrcPos);
  copyNodes(&block->fChildren, &fChildren);
  return cloneScope(this, block.release());
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


AptNode*
BlockNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos&   srcpos,
                           const NodeList& params,
                           const Type&     retType,
                           AptNode*        body)
  : AptNode(srcpos),
    fRetType(retType),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


FunctionNode*
FunctionNode::clone() const
{
  return cloneScope(this,
                    new FunctionNode(fSrcPos, copyNodes(fParams),
                                     fRetType.clone(), nodeClone(fBody)));
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


AptNode*
FunctionNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

FuncDefNode::FuncDefNode(const SrcPos& srcpos,
                         const String& sym,
                         unsigned int flags,
                         const NodeList& params,
                         const Type& retType,
                         AptNode* body)
  : FunctionNode(srcpos, params, retType, body),
    fSym(sym),
    fFlags(flags)
{ }


FuncDefNode*
FuncDefNode::clone() const
{
  Ptr<FuncDefNode> n = new FuncDefNode(fSrcPos, fSym, fFlags,
                                       fParams,
                                       fRetType.clone(), nodeClone(fBody));
  n->setLinkage(fLinkage);
  return cloneScope(this, n.release());
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


const String&
FuncDefNode::linkage() const
{
  return fLinkage;
}


void
FuncDefNode::setLinkage(const String& linkage)
{
  fLinkage = linkage;
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


AptNode*
FuncDefNode::transform(Transformator* tr)
{
  return tr->transform(this);
}



//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, AptNode* base)
  : AptNode(srcpos),
    fBase(base)
{ }


ApplyNode*
ApplyNode::clone() const
{
  Ptr<ApplyNode> apply = new ApplyNode(fSrcPos, nodeClone(fBase));
  copyNodes(&apply->fChildren, &fChildren);
  return cloneScope(this, apply.release());
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


AptNode*
ApplyNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos,
                       const String& key, AptNode* value)
  : AptNode(srcpos),
    fKey(key),
    fValue(value)
{ }


KeyargNode*
KeyargNode::clone() const
{
  return cloneScope(this, new KeyargNode(fSrcPos, fKey, nodeClone(fValue)));
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


AptNode*
KeyargNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

WhileNode::WhileNode(const SrcPos& srcpos, AptNode* test, AptNode* body)
  : AptNode(srcpos),
    fTest(test),
    fBody(body)
{ }


WhileNode*
WhileNode::clone() const
{
  return cloneScope(this, new WhileNode(fSrcPos,
                                        nodeClone(fTest),
                                        nodeClone(fBody)));
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


AptNode*
WhileNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

TypeDefNode::TypeDefNode(const SrcPos&   srcpos,
                         const String&   typeName,
                         bool            isClass,
                         const Type&     isa,
                         const NodeList& params,
                         const NodeList& slots,
                         const NodeList& reqProtocol,
                         const NodeList& onExprs)
  : AptNode(srcpos),
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
  return cloneScope(this,
                    new TypeDefNode(fSrcPos, fTypeName, fIsClass, fIsa.clone(),
                                    copyNodes(fParams),
                                    copyNodes(fSlots),
                                    copyNodes(fReqProtocol),
                                    copyNodes(fOnExprs)));
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


AptNode*
TypeDefNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


//----------------------------------------------------------------------------

CastNode::CastNode(const SrcPos& srcpos,
                   AptNode* base,
                   const Type& type)
  : AptNode(srcpos),
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
  return cloneScope(this,
                    new CastNode(fSrcPos, nodeClone(fBase), fType.clone()));
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


AptNode*
CastNode::transform(Transformator* tr)
{
  return tr->transform(this);
}


