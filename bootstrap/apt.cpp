/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "sysconf.h"

#include <string.h>

#include "annotate.h"
#include "apt.h"
#include "codegen.h"
#include "predefined.h"
#include "scope.h"
#include "strbuf.h"
#include "transform.h"
#include "traverse.h"
#include "typify.h"
#include "utils.h"
#include "xmlout.h"

#include "llvm/Value.h"

#include <string>
#include <map>

#if defined(UNITTESTS)
#  include <UnitTest++.h>
#  include <iostream>
#endif

using namespace herschel;


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
herschel::copyNodes(NodeList* dst, const NodeList* src)
{
  for (NodeList::const_iterator it = src->begin(); it != src->end(); ++it) {
    dst->push_back(nodeClone(it->obj()));
  }
}


NodeList
herschel::copyNodes(const NodeList& src)
{
  NodeList dst;
  copyNodes(&dst, &src);
  return dst;
}


//----------------------------------------------------------------------------

AptNode::AptNode(const SrcPos& srcpos)
  : fSrcPos(srcpos),
    fTypeConvKind(kNoConv),
    fIsInTailPos(false),
    fIsSingleTypeRequired(false)
{
}


AptNode::AptNode(const SrcPos& srcpos, const Type& type)
  : fSrcPos(srcpos),
    fType(type),
    fTypeConvKind(kNoConv),
    fIsInTailPos(false),
    fIsSingleTypeRequired(false)
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


const Type&
AptNode::type() const
{
  return fType;
}


void
AptNode::setType(const Type& type)
{
  fType = type;
}


const Type&
AptNode::dstType() const
{
  return fDstType;
}


void
AptNode::setDstType(const Type& type)
{
  fDstType = type;
}


TypeConvKind
AptNode::typeConv() const
{
  return fTypeConvKind;
}


void
AptNode::setTypeConv(TypeConvKind typeConv)
{
  fTypeConvKind = typeConv;
}


llvm::Value*
AptNode::codegen(CodeGenerator* generator) const
{
  return NULL;
}


bool
AptNode::isInTailPos() const
{
  return fIsInTailPos;
}


void
AptNode::setIsInTailPos(bool value)
{
  fIsInTailPos = value;
}


bool
AptNode::isSingleTypeRequired() const
{
  return fIsSingleTypeRequired;
}


void
AptNode::setIsSingleTypeRequired(bool value)
{
  fIsSingleTypeRequired = value;
}


//----------------------------------------------------------------------------

LoopAnnotatable::LoopAnnotatable()
  : fLoopId(0)
{}


int
LoopAnnotatable::loopId() const
{
  return fLoopId;
}


void
LoopAnnotatable::setLoopId(int loopId)
{
  fLoopId = loopId;
}


//----------------------------------------------------------------------------

DelayTypeAnnotatable::DelayTypeAnnotatable()
  : fDelayTypeSpec(false)
{}


bool
DelayTypeAnnotatable::isTypeSpecDelayed() const
{
  return fDelayTypeSpec;
}


void
DelayTypeAnnotatable::setTypeSpecDelayed(bool value)
{
  fDelayTypeSpec = value;
}


//----------------------------------------------------------------------------

const String&
LinkableSymbol::linkage() const
{
  return fLinkage;
}


void
LinkableSymbol::setLinkage(const String& linkage)
{
  fLinkage = linkage;
}


bool
LinkableSymbol::hasCLinkage() const
{
  return fLinkage == String("C");
}


//----------------------------------------------------------------------------

ListNode::ListNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{
}


NodeList&
ListNode::children()
{
  return fChildren;
}


const NodeList&
ListNode::children() const
{
  return fChildren;
}


void
ListNode::appendNode(AptNode* node)
{
  fChildren.push_back(node);
}


void
ListNode::appendNodes(const NodeList& nodes)
{
  if (!nodes.empty())
    fChildren.insert(fChildren.end(), nodes.begin(), nodes.end());
}


#define DEF_RENDER(_type)                       \
void                                            \
_type::render(XmlRenderer* renderer) const      \
{                                               \
  renderer->renderNode(this);                   \
}

#define DEF_CODEGEN(_type)                      \
llvm::Value*                                    \
_type::codegen(CodeGenerator* generator) const  \
{                                               \
  return generator->codegen(this);              \
}


#define DEF_TRAVERSE(_type)                     \
void                                            \
_type::traverse(Traversator* traversator)       \
{                                               \
  traversator->traverse(this);                  \
}


#define DEF_ANNOTATE(_type)                     \
void                                            \
_type::annotate(Annotator* an)                  \
{                                               \
  an->annotate(this);                           \
}


#define DEF_TRANSFORM(_type)                    \
AptNode*                                        \
_type::transform(Transformator* tr)             \
{                                               \
  return tr->transform(this);                   \
}


#define DEF_TYPIFY(_type)                       \
void                                            \
_type::typify(Typifier* typifier)               \
{                                               \
  typifier->typify(this);                       \
}


//----------------------------------------------------------------------------

UndefNode::UndefNode()
  : AptNode(SrcPos())
{ }


UndefNode*
UndefNode::clone() const
{
  return new UndefNode();
}


DEF_RENDER(UndefNode)
DEF_CODEGEN(UndefNode)
DEF_ANNOTATE(UndefNode)
DEF_TRAVERSE(UndefNode)
DEF_TRANSFORM(UndefNode)
DEF_TYPIFY(UndefNode)


//----------------------------------------------------------------------------

namespace herschel {
  template<typename T>
  T* cloneScope(const T* src, T* dst)
  {
    dst->setScope(src->scope());
    dst->setType(src->type());
    dst->setDstType(src->dstType());
    dst->setTypeConv(src->typeConv());
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
  return herschel::cloneScope(this, new StringNode(fSrcPos, fValue));
}


const String&
StringNode::value() const
{
  return fValue;
}


DEF_RENDER(StringNode)
DEF_CODEGEN(StringNode)
DEF_ANNOTATE(StringNode)
DEF_TRAVERSE(StringNode)
DEF_TRANSFORM(StringNode)
DEF_TYPIFY(StringNode)


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


const String&
KeywordNode::value() const
{
  return fValue;
}


DEF_RENDER(KeywordNode)
DEF_CODEGEN(KeywordNode)
DEF_ANNOTATE(KeywordNode)
DEF_TRAVERSE(KeywordNode)
DEF_TRANSFORM(KeywordNode)
DEF_TYPIFY(KeywordNode)


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
  SymbolNode* newnd = new SymbolNode(fSrcPos, fValue);
  newnd->setLinkage(linkage());
  return cloneScope(this, newnd);
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


const TypeVector&
SymbolNode::generics() const
{
  return fGenerics;
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


DEF_RENDER(SymbolNode)
DEF_CODEGEN(SymbolNode)
DEF_ANNOTATE(SymbolNode)
DEF_TRAVERSE(SymbolNode)
DEF_TRANSFORM(SymbolNode)
DEF_TYPIFY(SymbolNode)


//----------------------------------------------------------------------------

ArrayTypeNode::ArrayTypeNode(const SrcPos& srcpos, AptNode* typeNode)
  : AptNode(srcpos),
    fTypeNode(typeNode)
{ }


AptNode*
ArrayTypeNode::typeNode() const
{
  return fTypeNode;
}


ArrayTypeNode*
ArrayTypeNode::clone() const
{
  return cloneScope(this, new ArrayTypeNode(fSrcPos, nodeClone(fTypeNode)));
}


DEF_RENDER(ArrayTypeNode)
DEF_CODEGEN(ArrayTypeNode)
DEF_ANNOTATE(ArrayTypeNode)
DEF_TRAVERSE(ArrayTypeNode)
DEF_TRANSFORM(ArrayTypeNode)
DEF_TYPIFY(ArrayTypeNode)


//--------------------------------------------------------------------------

TypeNode::TypeNode(const SrcPos& srcpos, const Type& type)
  : AptNode(srcpos, type)
{ }


TypeNode*
TypeNode::clone() const
{
  return cloneScope(this, new TypeNode(fSrcPos, fType.clone()));
}


DEF_RENDER(TypeNode)
DEF_CODEGEN(TypeNode)
DEF_ANNOTATE(TypeNode)
DEF_TRAVERSE(TypeNode)
DEF_TRANSFORM(TypeNode)
DEF_TYPIFY(TypeNode)


//----------------------------------------------------------------------------

BaseNumberNode::BaseNumberNode(const SrcPos& srcpos, bool isImaginary,
                               const Type& type)
  : AptNode(srcpos, type),
    fIsImaginary(isImaginary)
{ }


bool
BaseNumberNode::isImaginary() const
{
  return fIsImaginary;
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int64_t value, bool isImaginary,
                 const Type& type)
  : NumberNode<int64_t>(srcpos, value, isImaginary, type)
{
}


IntNode*
IntNode::clone() const
{
  return cloneScope(this,
                    new IntNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
}


DEF_RENDER(IntNode)
DEF_CODEGEN(IntNode)
DEF_ANNOTATE(IntNode)
DEF_TRAVERSE(IntNode)
DEF_TRANSFORM(IntNode)
DEF_TYPIFY(IntNode)


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


DEF_RENDER(RealNode)
DEF_CODEGEN(RealNode)
DEF_ANNOTATE(RealNode)
DEF_TRAVERSE(RealNode)
DEF_TRANSFORM(RealNode)
DEF_TYPIFY(RealNode)


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


DEF_RENDER(RationalNode)
DEF_CODEGEN(RationalNode)
DEF_ANNOTATE(RationalNode)
DEF_TRAVERSE(RationalNode)
DEF_TRANSFORM(RationalNode)
DEF_TYPIFY(RationalNode)


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


Char
CharNode::value() const
{
  return fValue;
}


DEF_RENDER(CharNode)
DEF_CODEGEN(CharNode)
DEF_ANNOTATE(CharNode)
DEF_TRAVERSE(CharNode)
DEF_TRANSFORM(CharNode)
DEF_TYPIFY(CharNode)


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


Char
BoolNode::value() const
{
  return fValue;
}


DEF_RENDER(BoolNode)
DEF_CODEGEN(BoolNode)
DEF_ANNOTATE(BoolNode)
DEF_TRAVERSE(BoolNode)
DEF_TRANSFORM(BoolNode)
DEF_TYPIFY(BoolNode)


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


AptNode*
UnitConstNode::value() const
{
  return fValue;
}


void
UnitConstNode::setValue(AptNode* node)
{
  fValue = node;
}


TypeUnit
UnitConstNode::unit() const
{
  return fUnit;
}


DEF_RENDER(UnitConstNode)
DEF_CODEGEN(UnitConstNode)
DEF_ANNOTATE(UnitConstNode)
DEF_TRAVERSE(UnitConstNode)
DEF_TRANSFORM(UnitConstNode)
DEF_TYPIFY(UnitConstNode)


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos)
  : ListNode(srcpos)
{}


CompileUnitNode*
CompileUnitNode::clone() const
{
  Ptr<CompileUnitNode> node = new CompileUnitNode(fSrcPos);
  copyNodes(&node->fChildren, &fChildren);
  return cloneScope(this, node.release());
}


DEF_RENDER(CompileUnitNode)
DEF_CODEGEN(CompileUnitNode)
DEF_ANNOTATE(CompileUnitNode)
DEF_TRAVERSE(CompileUnitNode)
DEF_TRANSFORM(CompileUnitNode)
DEF_TYPIFY(CompileUnitNode)


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


void
BaseDefNode::setDefNode(AptNode* val)
{
  fDefined = val;
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


DEF_RENDER(LetNode)
DEF_CODEGEN(LetNode)
DEF_ANNOTATE(LetNode)
DEF_TRAVERSE(LetNode)
DEF_TRANSFORM(LetNode)
DEF_TYPIFY(LetNode)


//----------------------------------------------------------------------------

DefNode::DefNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


DefNode*
DefNode::clone() const
{
  return cloneScope(this, new DefNode(nodeClone(fDefined.obj())));
}


DEF_RENDER(DefNode)
DEF_CODEGEN(DefNode)
DEF_ANNOTATE(DefNode)
DEF_TRAVERSE(DefNode)
DEF_TRANSFORM(DefNode)
DEF_TYPIFY(DefNode)


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos,
                         const String& symbolName, const Type& type,
                         AptNode* initExpr)
  : AptNode(srcpos, type),
    fSymbolName(symbolName),
    fInitExpr(initExpr),
    fAllocType(kAlloc_Local)
{ }


AptNode*
BindingNode::initExpr() const
{
  return fInitExpr;
}


void
BindingNode::setInitExpr(AptNode* val)
{
  fInitExpr = val;
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


const String&
BindingNode::name() const
{
  return fSymbolName;
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


DEF_RENDER(VardefNode)
DEF_TRAVERSE(VardefNode)
DEF_TRANSFORM(VardefNode)
DEF_TYPIFY(VardefNode)


llvm::Value*
VardefNode::codegen(CodeGenerator* generator) const
{
  hr_invalid("this should never be called directly.  See codegen::DefNode");
  return NULL;
}


void
VardefNode::annotate(Annotator* annotator)
{
  hr_invalid("this should never be called directly.  See Annotator::DefNode");
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
  hr_assert(implies(fFlags == kNamedArg, !fKey.isEmpty()));
}


ParamNode*
ParamNode::clone() const
{
  return cloneScope(this,
                    new ParamNode(fSrcPos, fKey, fSymbolName, fFlags,
                                  fType.clone(), nodeClone(fInitExpr)));
}


ParamFlags
ParamNode::flags() const
{
  return fFlags;
}


bool
ParamNode::isPositional() const
{
  return (fFlags & kPosArg) != 0;
}


bool
ParamNode::isRestArg() const
{
  return (fFlags & kRestArg) != 0;
}


bool
ParamNode::isSpecArg() const
{
  return (fFlags & kSpecArg) != 0;
}


const String&
ParamNode::key() const
{
  return fKey;
}


DEF_RENDER(ParamNode)
DEF_CODEGEN(ParamNode)
DEF_ANNOTATE(ParamNode)
DEF_TRAVERSE(ParamNode)
DEF_TRANSFORM(ParamNode)
DEF_TYPIFY(ParamNode)


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


unsigned int
SlotdefNode::flags() const
{
  return fFlags;
}


bool
SlotdefNode::isAuto() const
{
  return (fFlags & kAutoSlot) != 0;
}


DEF_RENDER(SlotdefNode)
DEF_CODEGEN(SlotdefNode)
DEF_ANNOTATE(SlotdefNode)
DEF_TRAVERSE(SlotdefNode)
DEF_TRANSFORM(SlotdefNode)
DEF_TYPIFY(SlotdefNode)


//----------------------------------------------------------------------------

ArrayNode::ArrayNode(const SrcPos& srcpos)
  : ListNode(srcpos)
{ }


ArrayNode*
ArrayNode::clone() const
{
  Ptr<ArrayNode> an = new ArrayNode(fSrcPos);
  copyNodes(&an->fChildren, &fChildren);
  return cloneScope(this, an.release());
}


DEF_RENDER(ArrayNode)
DEF_CODEGEN(ArrayNode)
DEF_ANNOTATE(ArrayNode)
DEF_TRAVERSE(ArrayNode)
DEF_TRANSFORM(ArrayNode)
DEF_TYPIFY(ArrayNode)


//----------------------------------------------------------------------------

VectorNode::VectorNode(const SrcPos& srcpos)
  : ListNode(srcpos)
{ }


VectorNode*
VectorNode::clone() const
{
  Ptr<VectorNode> vect = new VectorNode(fSrcPos);
  copyNodes(&vect->fChildren, &fChildren);
  return cloneScope(this, vect.release());
}


DEF_RENDER(VectorNode)
DEF_CODEGEN(VectorNode)
DEF_ANNOTATE(VectorNode)
DEF_TRAVERSE(VectorNode)
DEF_TRANSFORM(VectorNode)
DEF_TYPIFY(VectorNode)


//----------------------------------------------------------------------------

DictNode::DictNode(const SrcPos& srcpos)
  : ListNode(srcpos)
{ }


DictNode*
DictNode::clone() const
{
  Ptr<DictNode> dict = new DictNode(fSrcPos);
  copyNodes(&dict->fChildren, &fChildren);
  return cloneScope(this, dict.release());
}


void
DictNode::addPair(AptNode* key, AptNode* value)
{
  hr_assert(key != NULL);
  hr_assert(value != NULL);

  appendNode(new BinaryNode(key->srcpos(), key, kOpMapTo, value));
}


DEF_RENDER(DictNode)
DEF_CODEGEN(DictNode)
DEF_ANNOTATE(DictNode)
DEF_TRAVERSE(DictNode)
DEF_TRANSFORM(DictNode)
DEF_TYPIFY(DictNode)


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(const SrcPos& srcpos,
                       AptNode* left, OperatorType op, AptNode* right)
  : AptNode(srcpos),
    fLeft(left),
    fRight(right),
    fOp(op)
{
  hr_assert(fOp != kOpInvalid);
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


DEF_RENDER(BinaryNode)
DEF_CODEGEN(BinaryNode)
DEF_ANNOTATE(BinaryNode)
DEF_TRAVERSE(BinaryNode)
DEF_TRANSFORM(BinaryNode)
DEF_TYPIFY(BinaryNode)


//----------------------------------------------------------------------------

UnaryNode::UnaryNode(const SrcPos& srcpos,
                     UnaryOperatorType op,
                     AptNode* base)
  : AptNode(srcpos),
    fBase(base),
    fOp(op)
{ }


AptNode*
UnaryNode::base() const
{
  return fBase;
}


void
UnaryNode::setBase(AptNode* base)
{
  fBase = base;
}


UnaryOperatorType
UnaryNode::op() const
{
  return fOp;
}


UnaryNode*
UnaryNode::clone() const
{
  return cloneScope(this, new UnaryNode(fSrcPos, fOp, nodeClone(fBase)));
}


DEF_RENDER(UnaryNode)
DEF_CODEGEN(UnaryNode)
DEF_ANNOTATE(UnaryNode)
DEF_TRAVERSE(UnaryNode)
DEF_TRANSFORM(UnaryNode)
DEF_TYPIFY(UnaryNode)


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


AptNode*
RangeNode::from() const
{
  return fFrom;
}


void
RangeNode::setFrom(AptNode* node)
{
  fFrom = node;
}


AptNode*
RangeNode::to() const
{
  return fTo;
}


void
RangeNode::setTo(AptNode* node)
{
  fTo = node;
}


AptNode*
RangeNode::by() const
{
  return fBy;
}


void
RangeNode::setBy(AptNode* node)
{
  fBy = node;
}


DEF_RENDER(RangeNode)
DEF_CODEGEN(RangeNode)
DEF_ANNOTATE(RangeNode)
DEF_TRAVERSE(RangeNode)
DEF_TRANSFORM(RangeNode)
DEF_TYPIFY(RangeNode)


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


DEF_RENDER(AssignNode)
DEF_CODEGEN(AssignNode)
DEF_ANNOTATE(AssignNode)
DEF_TRAVERSE(AssignNode)
DEF_TRANSFORM(AssignNode)
DEF_TYPIFY(AssignNode)


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
IfNode::setTest(AptNode* node)
{
  fTest = node;
}


void
IfNode::setConsequent(AptNode* node)
{
  fConsequent = node;
}


void
IfNode::setAlternate(AptNode* node)
{
  fAlternate = node;
}


DEF_RENDER(IfNode)
DEF_CODEGEN(IfNode)
DEF_ANNOTATE(IfNode)
DEF_TRAVERSE(IfNode)
DEF_TRANSFORM(IfNode)
DEF_TYPIFY(IfNode)


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
SelectNode::addMapping(const NodeList& mappings, AptNode* consequent)
{
  fMappings.push_back(SelectMapping(mappings, consequent));
}


void
SelectNode::addMapping(AptNode* mapping, AptNode* consequent)
{
  fMappings.push_back(
    SelectMapping(vector_of<Ptr<AptNode> >(mapping), consequent));
}


void
SelectNode::addElseMapping(AptNode* alternate)
{
  fMappings.push_back(SelectMapping(NodeList(), alternate));
}


AptNode*
SelectNode::test() const
{
  return fTest;
}


void
SelectNode::setTest(AptNode* nd)
{
  fTest = nd;
}


AptNode*
SelectNode::comparator() const
{
  return fComparator;
}


void
SelectNode::setComparator(AptNode* nd)
{
  fComparator = nd;
}


SelectNode::SelectMappingVector&
SelectNode::mappings()
{
  return fMappings;
}


size_t
SelectNode::mappingCount() const
{
  return fMappings.size();
}


const SelectNode::SelectMapping&
SelectNode::mappingAt(size_t i) const
{
  return fMappings[i];
}


void
SelectNode::setConsequentAt(size_t i, AptNode* consq)
{
  fMappings[i].fConsequent = consq;
}


void
SelectNode::setTestValueAt(size_t i, size_t j, AptNode* value)
{
  fMappings[i].fTestValues[j] = value;
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


DEF_RENDER(SelectNode)
DEF_CODEGEN(SelectNode)
DEF_ANNOTATE(SelectNode)
DEF_TRAVERSE(SelectNode)
DEF_TRANSFORM(SelectNode)
DEF_TYPIFY(SelectNode)


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
MatchNode::addMapping(const SrcPos& srcpos, const String& varName,
                      const Type& matchType,
                      AptNode* consequent)
{
  fMappings.push_back(MatchMapping(srcpos, varName, matchType, consequent));
}


AptNode*
MatchNode::expr() const
{
  return fExpr;
}


void
MatchNode::setExpr(AptNode* nd)
{
  fExpr = nd;
}


MatchNode::MatchMappingVector&
MatchNode::mappings()
{
  return fMappings;
}


size_t
MatchNode::mappingCount() const
{
  return fMappings.size();
}


const MatchNode::MatchMapping&
MatchNode::mappingAt(size_t i) const
{
  return fMappings[i];
}


void
MatchNode::setConsequentAt(size_t i, AptNode* consq)
{
  fMappings[i].fConsequent = consq;
}


DEF_RENDER(MatchNode)
DEF_CODEGEN(MatchNode)
DEF_ANNOTATE(MatchNode)
DEF_TRAVERSE(MatchNode)
DEF_TRANSFORM(MatchNode)
DEF_TYPIFY(MatchNode)


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


//------------------------------------------------------------------------------

OnNode::OnNode(const SrcPos& srcpos,
               const String& key, const NodeList& params, AptNode* body)
  : ListNode(srcpos),
    fKey(key),
    fBody(body)
{
  fChildren.assign(params.begin(), params.end());
}


OnNode*
OnNode::clone() const
{
  return cloneScope(this, new OnNode(fSrcPos, fKey,
                                     copyNodes(fChildren),
                                     nodeClone(fBody)));
}


const String&
OnNode::key() const
{
  return fKey;
}


AptNode*
OnNode::body() const
{
  return fBody;
}


void
OnNode::setBody(AptNode* node)
{
  fBody = node;
}


const NodeList&
OnNode::params() const
{
  return fChildren;
}


NodeList&
OnNode::params()
{
  return fChildren;
}


DEF_RENDER(OnNode)
DEF_CODEGEN(OnNode)
DEF_ANNOTATE(OnNode)
DEF_TRAVERSE(OnNode)
DEF_TRANSFORM(OnNode)
DEF_TYPIFY(OnNode)


//----------------------------------------------------------------------------

BlockNode::BlockNode(const SrcPos& srcpos)
  : ListNode(srcpos)
{ }


BlockNode*
BlockNode::clone() const
{
  Ptr<BlockNode> block = new BlockNode(fSrcPos);
  copyNodes(&block->fChildren, &fChildren);
  return cloneScope(this, block.release());
}


DEF_RENDER(BlockNode)
DEF_CODEGEN(BlockNode)
DEF_ANNOTATE(BlockNode)
DEF_TRAVERSE(BlockNode)
DEF_TRANSFORM(BlockNode)
DEF_TYPIFY(BlockNode)


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos&   srcpos,
                           const NodeList& params,
                           const Type&     retType,
                           AptNode*        body)
  : ListNode(srcpos),
    fRetType(retType),
    fBody(body)
{
  fChildren.assign(params.begin(), params.end());
}


FunctionNode*
FunctionNode::clone() const
{
  return cloneScope(this,
                    new FunctionNode(fSrcPos, copyNodes(fChildren),
                                     fRetType.clone(), nodeClone(fBody)));
}


const NodeList&
FunctionNode::params() const
{
  return fChildren;
}


NodeList&
FunctionNode::params()
{
  return fChildren;
}


const Type&
FunctionNode::retType() const
{
  return fRetType;
}


void
FunctionNode::setRetType(const Type& type)
{
  fRetType = type;
}


AptNode*
FunctionNode::body() const
{
  return fBody;
}


void
FunctionNode::setBody(AptNode* node)
{
  fBody = node;
}


size_t
FunctionNode::specializedParamsCount() const
{
  size_t specArgCount = 0;

  for (NodeList::const_iterator it = params().begin(), e = params().end();
       it != e;
       it++)
  {
    if (const ParamNode* prm = dynamic_cast<const ParamNode*>(it->obj())) {
      if (prm->isSpecArg())
        specArgCount++;
    }
  }

  return specArgCount;
}


bool
FunctionNode::hasSpecializedParams() const
{
  return specializedParamsCount() > 0;
}



DEF_RENDER(FunctionNode)
DEF_CODEGEN(FunctionNode)
DEF_ANNOTATE(FunctionNode)
DEF_TRAVERSE(FunctionNode)
DEF_TRANSFORM(FunctionNode)
DEF_TYPIFY(FunctionNode)


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
                                       copyNodes(fChildren),
                                       fRetType.clone(), nodeClone(fBody));
  n->setLinkage(fLinkage);
  return cloneScope(this, n.release());
}


bool
FuncDefNode::isGeneric() const
{
  return (fFlags & kFuncIsGeneric) != 0;
}


bool
FuncDefNode::isMethod() const
{
  return (fFlags & kFuncIsMethod) != 0;
}


bool
FuncDefNode::isAbstract() const
{
  return (fFlags & kFuncIsAbstract) != 0;
}


const String&
FuncDefNode::name() const
{
  return fSym;
}


bool
FuncDefNode::isAppMain() const
{
  return name() == Names::kAppMain;
}


llvm::Value*
FuncDefNode::codegen(CodeGenerator* generator) const
{
  hr_invalid("this should never be called directly.  See codegen::DefNode");
  return NULL;
}


void
FuncDefNode::annotate(Annotator* annotator)
{
  hr_invalid("this should never be called directly.  See Annotator::DefNode");
}

DEF_RENDER(FuncDefNode)
DEF_TRAVERSE(FuncDefNode)
DEF_TRANSFORM(FuncDefNode)
DEF_TYPIFY(FuncDefNode)


//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, AptNode* base)
  : ListNode(srcpos),
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
ApplyNode::setBase(AptNode* node)
{
  fBase = node;
}


bool
ApplyNode::isSimpleCall() const
{
  const SymbolNode* sym = dynamic_cast<const SymbolNode*>(base());
  return sym != NULL && sym->generics().empty();
}


String
ApplyNode::simpleCallName() const
{
  const SymbolNode* sym = dynamic_cast<const SymbolNode*>(base());
  return (sym != NULL
          ? sym->name()
          : String());
}


DEF_RENDER(ApplyNode)
DEF_CODEGEN(ApplyNode)
DEF_ANNOTATE(ApplyNode)
DEF_TRAVERSE(ApplyNode)
DEF_TRANSFORM(ApplyNode)
DEF_TYPIFY(ApplyNode)


#if defined(UNITTESTS)

TEST(ApplyNode)
{
  Ptr<ApplyNode> an = new ApplyNode(SrcPos(), new SymbolNode(SrcPos(), String("xyz")));
  CHECK(an->isSimpleCall());
  CHECK_EQUAL(an->simpleCallName(), String("xyz"));

  Ptr<ApplyNode> an2 = new ApplyNode(SrcPos(),
                                     new ApplyNode(SrcPos(),
                                                   new SymbolNode(SrcPos(),
                                                                  String("get-func"))));
  CHECK(!an2->isSimpleCall());
  CHECK_EQUAL(an2->simpleCallName(), String());
}

#endif  // #if defined(UNITTESTS)


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos,
                       const String& key, AptNode* value)
  : AptNode(srcpos),
    fKey(key),
    fValue(value)
{
  hr_assert(fValue != NULL);
}


KeyargNode*
KeyargNode::clone() const
{
  return cloneScope(this, new KeyargNode(fSrcPos, fKey, nodeClone(fValue)));
}


const String&
KeyargNode::key() const
{
  return fKey;
}


AptNode*
KeyargNode::value() const
{
  return fValue;
}


void
KeyargNode::setValue(AptNode* node)
{
  fValue = node;
}


DEF_RENDER(KeyargNode)
DEF_CODEGEN(KeyargNode)
DEF_ANNOTATE(KeyargNode)
DEF_TRAVERSE(KeyargNode)
DEF_TRANSFORM(KeyargNode)
DEF_TYPIFY(KeyargNode)


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


AptNode*
WhileNode::body() const
{
  return fBody;
}


void
WhileNode::setBody(AptNode* node)
{
  fBody = node;
}


AptNode*
WhileNode::test() const
{
  return fTest;
}


void
WhileNode::setTest(AptNode* node)
{
  fTest = node;
}


DEF_RENDER(WhileNode)
DEF_CODEGEN(WhileNode)
DEF_ANNOTATE(WhileNode)
DEF_TRAVERSE(WhileNode)
DEF_TRANSFORM(WhileNode)
DEF_TYPIFY(WhileNode)


//----------------------------------------------------------------------------

TypeDefNode::TypeDefNode(const SrcPos&   srcpos,
                         const String&   typeName,
                         bool            isClass,
                         const Type&     isa,
                         const NodeList& params,
                         const NodeList& slots,
                         const NodeList& onExprs)
  : AptNode(srcpos),
    fTypeName(typeName),
    fIsClass(isClass),
    fParams(params),
    fSlots(slots),
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
                                    copyNodes(fOnExprs)));
}


const String&
TypeDefNode::name() const
{
  return fTypeName;
}


const Type&
TypeDefNode::defType() const
{
  return fIsa;
}


bool
TypeDefNode::isClass() const
{
  return fIsClass;
}


const NodeList&
TypeDefNode::params() const
{
  return fParams;
}


const NodeList&
TypeDefNode::slots() const
{
  return fSlots;
}


const NodeList&
TypeDefNode::onExprs() const
{
  return fOnExprs;
}


NodeList&
TypeDefNode::params()
{
  return fParams;
}


NodeList&
TypeDefNode::slots()
{
  return fSlots;
}


NodeList&
TypeDefNode::onExprs()
{
  return fOnExprs;
}

DEF_RENDER(TypeDefNode)
DEF_CODEGEN(TypeDefNode)
DEF_ANNOTATE(TypeDefNode)
DEF_TRAVERSE(TypeDefNode)
DEF_TRANSFORM(TypeDefNode)
DEF_TYPIFY(TypeDefNode)


//----------------------------------------------------------------------------

CastNode::CastNode(const SrcPos& srcpos,
                   AptNode* base,
                   const Type& type)
  : AptNode(srcpos, type),
    fBase(base)
{ }

AptNode*
CastNode::base() const
{
  return fBase;
}


void
CastNode::setBase(AptNode* node)
{
  fBase = node;
}


CastNode*
CastNode::clone() const
{
  return cloneScope(this,
                    new CastNode(fSrcPos, nodeClone(fBase), fType.clone()));
}


DEF_RENDER(CastNode)
DEF_CODEGEN(CastNode)
DEF_ANNOTATE(CastNode)
DEF_TRAVERSE(CastNode)
DEF_TRANSFORM(CastNode)
DEF_TYPIFY(CastNode)


//--------------------------------------------------------------------------------

SlotRefNode::SlotRefNode(const SrcPos& srcpos,
                         AptNode* base, const String& slotName)
  : AptNode(srcpos),
    fBase(base),
    fSlotName(slotName)
{ }


SlotRefNode*
SlotRefNode::clone() const
{
  return cloneScope(this,
                    new SlotRefNode(fSrcPos, nodeClone(fBase), fSlotName));
}


AptNode*
SlotRefNode::base() const
{
  return fBase;
}


void
SlotRefNode::setBase(AptNode* base)
{
  fBase = base;
}


String
SlotRefNode::slotName() const
{
  return fSlotName;
}


DEF_RENDER(SlotRefNode)
DEF_CODEGEN(SlotRefNode)
DEF_ANNOTATE(SlotRefNode)
DEF_TRAVERSE(SlotRefNode)
DEF_TRANSFORM(SlotRefNode)
DEF_TYPIFY(SlotRefNode)


//--------------------------------------------------------------------------------

NodeList
herschel::newNodeList()
{
  return NodeList();
}


NodeList
herschel::newNodeList(AptNode* n1)
{
  NodeList nl;
  if (n1 != NULL)
    nl.push_back(n1);
  return nl;
}


NodeList
herschel::newNodeList(AptNode* n1, AptNode* n2)
{
  NodeList nl;
  if (n1 != NULL)
    nl.push_back(n1);
  if (n2 != NULL)
    nl.push_back(n2);
  return nl;
}


NodeList
herschel::newNodeList(AptNode* n1, AptNode* n2, AptNode* n3)
{
  NodeList nl;
  if (n1 != NULL)
    nl.push_back(n1);
  if (n2 != NULL)
    nl.push_back(n2);
  if (n3 != NULL)
    nl.push_back(n3);
  return nl;
}


NodeList
herschel::newNodeList(AptNode* n1, AptNode* n2, AptNode* n3, AptNode* n4)
{
  NodeList nl;
  if (n1 != NULL)
    nl.push_back(n1);
  if (n2 != NULL)
    nl.push_back(n2);
  if (n3 != NULL)
    nl.push_back(n3);
  if (n4 != NULL)
    nl.push_back(n4);
  return nl;
}


NodeList&
herschel::appendNodes(NodeList& dst, const NodeList& nl)
{
  if (!nl.empty())
    dst.insert(dst.end(), nl.begin(), nl.end());
  return dst;
}


AptNode*
herschel::singletonNodeListOrNull(const NodeList& nl)
{
  hr_assert(nl.size() < 2);
  if (nl.size() == 1)
    return nl[0].obj();
  return NULL;
}
