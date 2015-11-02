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

#include "llvm/IR/Value.h"

#include <string>
#include <map>


using namespace herschel;


template<typename T>
std::shared_ptr<T> nodeClone(std::shared_ptr<T> node)
{
  if (node)
    return node->clone();
  return nullptr;
}


void
herschel::copyNodes(NodeList* dst, const NodeList* src)
{
  for (auto& s : *src) {
    dst->push_back(nodeClone(s));
  }
}


NodeList
herschel::copyNodes(const NodeList& src)
{
  NodeList dst;
  copyNodes(&dst, &src);
  return dst;
}


zstring
herschel::convkind2str(TypeConvKind kind)
{
  switch (kind) {
  case kNoConv:         return "no-conv";
  case kTypeCheckConv:  return "type-check";
  case kAtom2PlainConv: return "atom2plain";
  case kPlain2AtomConv: return "plain2atom";
  }

  return "unknown-conv";
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


AptNode::~AptNode()
{
}


const SrcPos&
AptNode::srcpos() const
{
  return fSrcPos;
}


std::shared_ptr<Scope>
AptNode::scope() const
{
  return fScope;
}


void
AptNode::setScope(std::shared_ptr<Scope> scope)
{
  fScope = scope;
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
  return nullptr;
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


void
AptNode::dump() const
{
  Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stderr));
  out->render(*this);
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
ListNode::appendNode(std::shared_ptr<AptNode> node)
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
  renderer->renderNode(*this);                  \
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
  traversator->traverse(*this);                 \
}


#define DEF_ANNOTATE(_type)                                       \
void                                                              \
_type::annotate(Annotator* an, std::shared_ptr<AptNode> nd)       \
{                                                                 \
  using ConcreteType = std::remove_pointer<decltype(this)>::type; \
  return an->annotate(                                            \
    std::dynamic_pointer_cast<ConcreteType>(nd));                 \
}


#define DEF_TRANSFORM(_type)                                      \
std::shared_ptr<AptNode>                                          \
_type::transform(Transformator* tr, std::shared_ptr<AptNode> nd)  \
{                                                                 \
  using ConcreteType = std::remove_pointer<decltype(this)>::type; \
  return tr->transform(                                           \
    std::dynamic_pointer_cast<ConcreteType>(nd));                 \
}


#define DEF_TYPIFY(_type)                       \
void                                            \
_type::typify(Typifier& typifier)               \
{                                               \
  typifier.typify(*this);                       \
}


//----------------------------------------------------------------------------

UndefNode::UndefNode()
  : AptNode(SrcPos())
{ }


std::shared_ptr<AptNode>
UndefNode::clone() const
{
  return makeUndefNode();
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
  std::shared_ptr<T> cloneScope(const T* src, std::shared_ptr<T> dst)
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


std::shared_ptr<AptNode>
StringNode::clone() const
{
  return herschel::cloneScope(this, makeStringNode(fSrcPos, fValue));
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


std::shared_ptr<AptNode>
KeywordNode::clone() const
{
  return cloneScope(this, makeKeywordNode(fSrcPos, fValue));
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


std::shared_ptr<AptNode>
SymbolNode::clone() const
{
  auto newnd = makeSymbolNode(fSrcPos, fValue);
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

ArrayTypeNode::ArrayTypeNode(const SrcPos& srcpos,
                             std::shared_ptr<AptNode> typeNode)
  : AptNode(srcpos),
    fTypeNode(std::move(typeNode))
{ }


std::shared_ptr<AptNode>
ArrayTypeNode::typeNode() const
{
  return fTypeNode;
}


std::shared_ptr<AptNode>
ArrayTypeNode::clone() const
{
  return cloneScope(this, makeArrayTypeNode(fSrcPos, nodeClone(fTypeNode)));
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


std::shared_ptr<AptNode>
TypeNode::clone() const
{
  return cloneScope(this, makeTypeNode(fSrcPos, fType.clone()));
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


std::shared_ptr<AptNode>
IntNode::clone() const
{
  return cloneScope(this, makeIntNode(fSrcPos, fValue, fIsImaginary,
                                      fType.clone()));
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


std::shared_ptr<AptNode>
RealNode::clone() const
{
  return cloneScope(this, makeRealNode(fSrcPos, fValue, fIsImaginary,
                                       fType.clone()));
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


std::shared_ptr<AptNode>
RationalNode::clone() const
{
  return cloneScope(this, makeRationalNode(fSrcPos, fValue,
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


std::shared_ptr<AptNode>
CharNode::clone() const
{
  return cloneScope(this, makeCharNode(fSrcPos, fValue));
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


std::shared_ptr<AptNode>
BoolNode::clone() const
{
  return cloneScope(this, makeBoolNode(fSrcPos, fValue));
}


bool
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

UnitConstNode::UnitConstNode(const SrcPos& srcpos,
                             std::shared_ptr<AptNode> value,
                             const TypeUnit& unit)
  : AptNode(srcpos),
    fValue(std::move(value)),
    fUnit(unit)
{
}


std::shared_ptr<AptNode>
UnitConstNode::clone() const
{
  return cloneScope(this, makeUnitConstNode(fSrcPos, nodeClone(fValue), fUnit));
}


std::shared_ptr<AptNode>
UnitConstNode::value() const
{
  return fValue;
}


void
UnitConstNode::setValue(std::shared_ptr<AptNode> node)
{
  fValue = std::move(node);
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


std::shared_ptr<AptNode>
CompileUnitNode::clone() const
{
  auto node = makeCompileUnitNode(fSrcPos);
  copyNodes(&node->fChildren, &fChildren);
  return cloneScope(this, std::move(node));
}


DEF_RENDER(CompileUnitNode)
DEF_CODEGEN(CompileUnitNode)
DEF_ANNOTATE(CompileUnitNode)
DEF_TRAVERSE(CompileUnitNode)
DEF_TRANSFORM(CompileUnitNode)
DEF_TYPIFY(CompileUnitNode)


//----------------------------------------------------------------------------

BaseDefNode::BaseDefNode(const SrcPos& srcpos,
                         std::shared_ptr<AptNode> defined)
  : AptNode(srcpos),
    fDefined(std::move(defined))
{ }


std::shared_ptr<AptNode>
BaseDefNode::defNode() const
{
  return fDefined;
}


void
BaseDefNode::setDefNode(std::shared_ptr<AptNode> val)
{
  fDefined = std::move(val);
}


//----------------------------------------------------------------------------

LetNode::LetNode(std::shared_ptr<AptNode> node)
  : BaseDefNode(node->srcpos(), std::move(node))
{ }


std::shared_ptr<AptNode>
LetNode::clone() const
{
  return cloneScope(this, makeLetNode(nodeClone(fDefined)));
}


DEF_RENDER(LetNode)
DEF_CODEGEN(LetNode)
DEF_ANNOTATE(LetNode)
DEF_TRAVERSE(LetNode)
DEF_TRANSFORM(LetNode)
DEF_TYPIFY(LetNode)


//----------------------------------------------------------------------------

DefNode::DefNode(std::shared_ptr<AptNode> node)
  : BaseDefNode(node->srcpos(), std::move(node))
{ }


std::shared_ptr<AptNode>
DefNode::clone() const
{
  return cloneScope(this, makeDefNode(nodeClone(fDefined)));
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
                         std::shared_ptr<AptNode> initExpr)
  : AptNode(srcpos, type),
    fSymbolName(symbolName),
    fInitExpr(std::move(initExpr)),
    fAllocType(kAlloc_Local)
{ }


std::shared_ptr<AptNode>
BindingNode::initExpr() const
{
  return fInitExpr;
}


void
BindingNode::setInitExpr(std::shared_ptr<AptNode> val)
{
  fInitExpr = std::move(val);
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
                       bool isLocal, const Type& type,
                       std::shared_ptr<AptNode> initExpr)
  : BindingNode(srcpos, symbolName, type, std::move(initExpr)),
    fIsLocal(isLocal),
    fFlags(flags)
{
}


std::shared_ptr<AptNode>
VardefNode::clone() const
{
  auto n = makeVardefNode(fSrcPos, fSymbolName, fFlags, fIsLocal,
                          fType.clone(), nodeClone(fInitExpr));
  n->setLinkage(fLinkage);
  return cloneScope(this, std::move(n));
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
  return nullptr;
}


void
VardefNode::annotate(Annotator* annotator, std::shared_ptr<AptNode> nd)
{
  hr_invalid("this should never be called directly.  See Annotator::DefNode");
}


//----------------------------------------------------------------------------

ParamNode::ParamNode(const SrcPos& srcpos, const String& keyName,
                     const String& symbolName,
                     ParamFlags flags, const Type& type,
                     std::shared_ptr<AptNode> initExpr)
  : BindingNode(srcpos, symbolName, type, std::move(initExpr)),
    fKey(keyName),
    fFlags(flags)
{
  hr_assert(implies(fFlags == kNamedArg, !fKey.isEmpty()));
}


std::shared_ptr<AptNode>
ParamNode::clone() const
{
  return cloneScope(this, makeParamNode(fSrcPos, fKey, fSymbolName,
                                        fFlags, fType.clone(),
                                        nodeClone(fInitExpr)));
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

SlotdefNode::SlotdefNode(const SrcPos& srcpos, const String& symbolName,
                         unsigned int flags, const Type& type,
                         std::shared_ptr<AptNode> initExpr)
  : BindingNode(srcpos, symbolName, type, std::move(initExpr)),
    fFlags(flags)
{ }


std::shared_ptr<AptNode>
SlotdefNode::clone() const
{
  return cloneScope(this, makeSlotdefNode(fSrcPos, fSymbolName, fFlags,
                                          fType.clone(),
                                          nodeClone(fInitExpr)));
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


std::shared_ptr<AptNode>
ArrayNode::clone() const
{
  auto an = makeArrayNode(fSrcPos);
  copyNodes(&an->fChildren, &fChildren);
  return cloneScope(this, std::move(an));
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


std::shared_ptr<AptNode>
VectorNode::clone() const
{
  auto vect = makeVectorNode(fSrcPos);
  copyNodes(&vect->fChildren, &fChildren);
  return cloneScope(this, std::move(vect));
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


std::shared_ptr<AptNode>
DictNode::clone() const
{
  auto dict = makeDictNode(fSrcPos);
  copyNodes(&dict->fChildren, &fChildren);
  return cloneScope(this, std::move(dict));
}


void
DictNode::addPair(std::shared_ptr<AptNode> key,
                  std::shared_ptr<AptNode> value)
{
  hr_assert(key);
  hr_assert(value);

  appendNode(makeBinaryNode(key->srcpos(), key, kOpMapTo, std::move(value)));
}


DEF_RENDER(DictNode)
DEF_CODEGEN(DictNode)
DEF_ANNOTATE(DictNode)
DEF_TRAVERSE(DictNode)
DEF_TRANSFORM(DictNode)
DEF_TYPIFY(DictNode)


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(const SrcPos& srcpos,
                       std::shared_ptr<AptNode> left, OperatorType op,
                       std::shared_ptr<AptNode> right)
  : AptNode(srcpos),
    fLeft(std::move(left)),
    fRight(std::move(right)),
    fOp(op)
{
  hr_assert(fOp != kOpInvalid);
}


std::shared_ptr<AptNode>
BinaryNode::clone() const
{
  return cloneScope(this, makeBinaryNode(fSrcPos, nodeClone(fLeft), fOp,
                                         nodeClone(fRight)));
}


OperatorType
BinaryNode::op() const
{
  return fOp;
}


std::shared_ptr<AptNode>
BinaryNode::left() const
{
  return fLeft;
}


void
BinaryNode::setLeft(std::shared_ptr<AptNode> node)
{
  fLeft = std::move(node);
}


std::shared_ptr<AptNode>
BinaryNode::right() const
{
  return fRight;
}


void
BinaryNode::setRight(std::shared_ptr<AptNode> node)
{
  fRight = std::move(node);
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
                     std::shared_ptr<AptNode> base)
  : AptNode(srcpos),
    fBase(std::move(base)),
    fOp(op)
{ }


std::shared_ptr<AptNode>
UnaryNode::base() const
{
  return fBase;
}


void
UnaryNode::setBase(std::shared_ptr<AptNode> base)
{
  fBase = std::move(base);
}


UnaryOperatorType
UnaryNode::op() const
{
  return fOp;
}


std::shared_ptr<AptNode>
UnaryNode::clone() const
{
  return cloneScope(this, makeUnaryNode(fSrcPos, fOp, nodeClone(fBase)));
}


DEF_RENDER(UnaryNode)
DEF_CODEGEN(UnaryNode)
DEF_ANNOTATE(UnaryNode)
DEF_TRAVERSE(UnaryNode)
DEF_TRANSFORM(UnaryNode)
DEF_TYPIFY(UnaryNode)


//------------------------------------------------------------------------------

RangeNode::RangeNode(const SrcPos& srcpos,
                     std::shared_ptr<AptNode> from,
                     std::shared_ptr<AptNode> to,
                     std::shared_ptr<AptNode> by)
  : AptNode(srcpos),
    fFrom(std::move(from)),
    fTo(std::move(to)),
    fBy(std::move(by))
{ }


std::shared_ptr<AptNode>
RangeNode::clone() const
{
  return cloneScope(this, makeRangeNode(fSrcPos, nodeClone(fFrom),
                                        nodeClone(fTo),
                                        nodeClone(fBy)));
}


std::shared_ptr<AptNode>
RangeNode::from() const
{
  return fFrom;
}


void
RangeNode::setFrom(std::shared_ptr<AptNode> node)
{
  fFrom = std::move(node);
}


std::shared_ptr<AptNode>
RangeNode::to() const
{
  return fTo;
}


void
RangeNode::setTo(std::shared_ptr<AptNode> node)
{
  fTo = std::move(node);
}


std::shared_ptr<AptNode>
RangeNode::by() const
{
  return fBy;
}


void
RangeNode::setBy(std::shared_ptr<AptNode> node)
{
  fBy = std::move(node);
}


DEF_RENDER(RangeNode)
DEF_CODEGEN(RangeNode)
DEF_ANNOTATE(RangeNode)
DEF_TRAVERSE(RangeNode)
DEF_TRANSFORM(RangeNode)
DEF_TYPIFY(RangeNode)


//--------------------------------------------------------------------------

AssignNode::AssignNode(const SrcPos& srcpos,
                       std::shared_ptr<AptNode> lvalue,
                       std::shared_ptr<AptNode> rvalue)
  : AptNode(srcpos),
    fLValue(std::move(lvalue)),
    fRValue(std::move(rvalue))
{ }


std::shared_ptr<AptNode>
AssignNode::clone() const
{
  return cloneScope(this, makeAssignNode(fSrcPos, nodeClone(fLValue),
                                         nodeClone(fRValue)));
}


std::shared_ptr<AptNode>
AssignNode::lvalue() const
{
  return fLValue;
}


std::shared_ptr<AptNode>
AssignNode::rvalue() const
{
  return fRValue;
}


void
AssignNode::setLvalue(std::shared_ptr<AptNode> val)
{
  fLValue = std::move(val);
}


void
AssignNode::setRvalue(std::shared_ptr<AptNode> val)
{
  fRValue = std::move(val);
}


DEF_RENDER(AssignNode)
DEF_CODEGEN(AssignNode)
DEF_ANNOTATE(AssignNode)
DEF_TRAVERSE(AssignNode)
DEF_TRANSFORM(AssignNode)
DEF_TYPIFY(AssignNode)


//------------------------------------------------------------------------------

IfNode::IfNode(const SrcPos& srcpos,
               std::shared_ptr<AptNode> test,
               std::shared_ptr<AptNode> consequent,
               std::shared_ptr<AptNode> alternate)
  : AptNode(srcpos),
    fTest(std::move(test)),
    fConsequent(std::move(consequent)),
    fAlternate(std::move(alternate))
{ }


std::shared_ptr<AptNode>
IfNode::clone() const
{
  return cloneScope(this, makeIfNode(fSrcPos, nodeClone(fTest),
                                     nodeClone(fConsequent),
                                     nodeClone(fAlternate)));
}


std::shared_ptr<AptNode>
IfNode::test() const
{
  return fTest;
}


std::shared_ptr<AptNode>
IfNode::consequent() const
{
  return fConsequent;
}


std::shared_ptr<AptNode>
IfNode::alternate() const
{
  return fAlternate;
}


void
IfNode::setTest(std::shared_ptr<AptNode> node)
{
  fTest = std::move(node);
}


void
IfNode::setConsequent(std::shared_ptr<AptNode> node)
{
  fConsequent = std::move(node);
}


void
IfNode::setAlternate(std::shared_ptr<AptNode> node)
{
  fAlternate = std::move(node);
}


DEF_RENDER(IfNode)
DEF_CODEGEN(IfNode)
DEF_ANNOTATE(IfNode)
DEF_TRAVERSE(IfNode)
DEF_TRANSFORM(IfNode)
DEF_TYPIFY(IfNode)


//------------------------------------------------------------------------------

SelectNode::SelectNode(const SrcPos& srcpos,
                       std::shared_ptr<AptNode> test,
                       std::shared_ptr<AptNode> comparator)
  : AptNode(srcpos),
    fTest(std::move(test)),
    fComparator(std::move(comparator))
{
}


std::shared_ptr<AptNode>
SelectNode::clone() const
{
  auto newNode = makeSelectNode(fSrcPos, nodeClone(fTest),
                                nodeClone(fComparator));
  for (auto& mapping : fMappings) {
    newNode->fMappings.push_back(SelectMapping(
                                   copyNodes(mapping.fTestValues),
                                   nodeClone(mapping.fConsequent)));
  }

  return cloneScope(this, std::move(newNode));
}


void
SelectNode::addMapping(const NodeList& mappings,
                       std::shared_ptr<AptNode> consequent)
{
  fMappings.push_back(SelectMapping(mappings, std::move(consequent)));
}


void
SelectNode::addMapping(std::shared_ptr<AptNode> mapping,
                       std::shared_ptr<AptNode> consequent)
{
  fMappings.push_back(
    SelectMapping(std::vector<NodeList::value_type>{mapping},
                  std::move(consequent)));
}


void
SelectNode::addElseMapping(std::shared_ptr<AptNode> alternate)
{
  fMappings.push_back(SelectMapping(NodeList(), std::move(alternate)));
}


std::shared_ptr<AptNode>
SelectNode::test() const
{
  return fTest;
}


void
SelectNode::setTest(std::shared_ptr<AptNode> nd)
{
  fTest = std::move(nd);
}


std::shared_ptr<AptNode>
SelectNode::comparator() const
{
  return fComparator;
}


void
SelectNode::setComparator(std::shared_ptr<AptNode> nd)
{
  fComparator = std::move(nd);
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
SelectNode::setConsequentAt(size_t i, std::shared_ptr<AptNode> consq)
{
  fMappings[i].fConsequent = std::move(consq);
}


void
SelectNode::setTestValueAt(size_t i, size_t j, std::shared_ptr<AptNode> value)
{
  fMappings[i].fTestValues[j] = std::move(value);
}


SelectNode::SelectMapping::SelectMapping(const NodeList& values,
                                         std::shared_ptr<AptNode> consequent)
  : fTestValues(values),
    fConsequent(std::move(consequent))
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

MatchNode::MatchNode(const SrcPos& srcpos, std::shared_ptr<AptNode> expr)
  : AptNode(srcpos),
    fExpr(std::move(expr))
{
}


std::shared_ptr<AptNode>
MatchNode::clone() const
{
  auto newNode = makeMatchNode(fSrcPos, nodeClone(fExpr));
  for (auto& mapping : fMappings) {
    newNode->fMappings.push_back(MatchMapping(
                                   mapping.fSrcPos,
                                   mapping.fVarName,
                                   mapping.fMatchType.clone(),
                                   nodeClone(mapping.fConsequent)));
  }

  return cloneScope(this, std::move(newNode));
}


void
MatchNode::addMapping(const SrcPos& srcpos, const String& varName,
                      const Type& matchType,
                      std::shared_ptr<AptNode> consequent)
{
  fMappings.push_back(MatchMapping(srcpos, varName, matchType,
                                   std::move(consequent)));
}


std::shared_ptr<AptNode>
MatchNode::expr() const
{
  return fExpr;
}


void
MatchNode::setExpr(std::shared_ptr<AptNode> nd)
{
  fExpr = std::move(nd);
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
MatchNode::setConsequentAt(size_t i, std::shared_ptr<AptNode> consq)
{
  fMappings[i].fConsequent = std::move(consq);
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
                                      std::shared_ptr<AptNode> consequent)
  : fSrcPos(srcpos),
    fVarName(varName),
    fMatchType(matchType),
    fConsequent(std::move(consequent))
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
               const String& key, const NodeList& params,
               std::shared_ptr<AptNode> body)
  : ListNode(srcpos),
    fKey(key),
    fBody(std::move(body))
{
  fChildren.assign(params.begin(), params.end());
}


std::shared_ptr<AptNode>
OnNode::clone() const
{
  return cloneScope(this, makeOnNode(fSrcPos, fKey, copyNodes(fChildren),
                                     nodeClone(fBody)));
}


const String&
OnNode::key() const
{
  return fKey;
}


std::shared_ptr<AptNode>
OnNode::body() const
{
  return fBody;
}


void
OnNode::setBody(std::shared_ptr<AptNode> node)
{
  fBody = std::move(node);
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


std::shared_ptr<AptNode>
BlockNode::clone() const
{
  auto block = makeBlockNode(fSrcPos);
  copyNodes(&block->fChildren, &fChildren);
  return cloneScope(this, std::move(block));
}


DEF_RENDER(BlockNode)
DEF_CODEGEN(BlockNode)
DEF_ANNOTATE(BlockNode)
DEF_TRAVERSE(BlockNode)
DEF_TRANSFORM(BlockNode)
DEF_TYPIFY(BlockNode)


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos& srcpos, const NodeList& params,
                           const Type& retType, std::shared_ptr<AptNode> body)
  : ListNode(srcpos),
    fRetType(retType),
    fBody(std::move(body))
{
  fChildren.assign(params.begin(), params.end());
}


std::shared_ptr<AptNode>
FunctionNode::clone() const
{
  return cloneScope(this, makeFunctionNode(fSrcPos, copyNodes(fChildren),
                                           fRetType.clone(),
                                           nodeClone(fBody)));
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
  fRetType = std::move(type);
}


std::shared_ptr<AptNode>
FunctionNode::body() const
{
  return fBody;
}


void
FunctionNode::setBody(std::shared_ptr<AptNode> node)
{
  fBody = std::move(node);
}


size_t
FunctionNode::specializedParamsCount() const
{
  return std::count_if(params().begin(), params().end(),
                       [](const NodeList::value_type& nd) {
                         auto prm = dynamic_cast<ParamNode*>(nd.get());
                         return prm && prm->isSpecArg();
                       });
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
                         std::shared_ptr<AptNode> body)
  : FunctionNode(srcpos, params, retType, std::move(body)),
    fSym(sym),
    fFlags(flags)
{ }

std::shared_ptr<AptNode>
FuncDefNode::clone() const
{
  auto n = makeFuncDefNode(fSrcPos, fSym, fFlags, copyNodes(fChildren),
                           fRetType.clone(), nodeClone(fBody));
  n->setLinkage(fLinkage);
  return cloneScope(this, std::move(n));
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
  return nullptr;
}


void
FuncDefNode::annotate(Annotator* annotator, std::shared_ptr<AptNode> nd)
{
  hr_invalid("this should never be called directly.  See Annotator::DefNode");
}

DEF_RENDER(FuncDefNode)
DEF_TRAVERSE(FuncDefNode)
DEF_TRANSFORM(FuncDefNode)
DEF_TYPIFY(FuncDefNode)


//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base)
  : ListNode(srcpos),
    fBase(std::move(base))
{ }


std::shared_ptr<AptNode>
ApplyNode::clone() const
{
  auto apply = makeApplyNode(fSrcPos, nodeClone(fBase));
  copyNodes(&apply->fChildren, &fChildren);
  return cloneScope(this, std::move(apply));
}


std::shared_ptr<AptNode>
ApplyNode::base() const
{
  return fBase;
}


void
ApplyNode::setBase(std::shared_ptr<AptNode> node)
{
  fBase = std::move(node);
}


bool
ApplyNode::isSimpleCall() const
{
  auto sym = dynamic_cast<SymbolNode*>(base().get());
  return sym && sym->generics().empty();
}


String
ApplyNode::simpleCallName() const
{
  auto sym = dynamic_cast<SymbolNode*>(base().get());
  return (sym
          ? sym->name()
          : String());
}


DEF_RENDER(ApplyNode)
DEF_CODEGEN(ApplyNode)
DEF_ANNOTATE(ApplyNode)
DEF_TRAVERSE(ApplyNode)
DEF_TRANSFORM(ApplyNode)
DEF_TYPIFY(ApplyNode)


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos,
                       const String& key, std::shared_ptr<AptNode> value)
  : AptNode(srcpos),
    fKey(key),
    fValue(std::move(value))
{
  hr_assert(fValue);
}


std::shared_ptr<AptNode>
KeyargNode::clone() const
{
  return cloneScope(this, makeKeyargNode(fSrcPos, fKey, nodeClone(fValue)));
}


const String&
KeyargNode::key() const
{
  return fKey;
}


std::shared_ptr<AptNode>
KeyargNode::value() const
{
  return fValue;
}


void
KeyargNode::setValue(std::shared_ptr<AptNode> node)
{
  fValue = std::move(node);
}


DEF_RENDER(KeyargNode)
DEF_CODEGEN(KeyargNode)
DEF_ANNOTATE(KeyargNode)
DEF_TRAVERSE(KeyargNode)
DEF_TRANSFORM(KeyargNode)
DEF_TYPIFY(KeyargNode)


//----------------------------------------------------------------------------

WhileNode::WhileNode(const SrcPos& srcpos,
                     std::shared_ptr<AptNode> test,
                     std::shared_ptr<AptNode> body)
  : AptNode(srcpos),
    fTest(std::move(test)),
    fBody(std::move(body))
{ }


std::shared_ptr<AptNode>
WhileNode::clone() const
{
  return cloneScope(this, makeWhileNode(fSrcPos, nodeClone(fTest),
                                        nodeClone(fBody)));
}


std::shared_ptr<AptNode>
WhileNode::body() const
{
  return fBody;
}


void
WhileNode::setBody(std::shared_ptr<AptNode> node)
{
  fBody = std::move(node);
}


std::shared_ptr<AptNode>
WhileNode::test() const
{
  return fTest;
}


void
WhileNode::setTest(std::shared_ptr<AptNode> node)
{
  fTest = std::move(node);
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


std::shared_ptr<AptNode>
TypeDefNode::clone() const
{
  return cloneScope(this, makeTypeDefNode(fSrcPos, fTypeName, fIsClass,
                                          fIsa.clone(),
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
                   std::shared_ptr<AptNode> base,
                   const Type& type)
  : AptNode(srcpos, type),
    fBase(std::move(base))
{ }

std::shared_ptr<AptNode>
CastNode::base() const
{
  return fBase;
}


void
CastNode::setBase(std::shared_ptr<AptNode> node)
{
  fBase = std::move(node);
}


std::shared_ptr<AptNode>
CastNode::clone() const
{
  return cloneScope(this, makeCastNode(fSrcPos, nodeClone(fBase),
                                       fType.clone()));
}


DEF_RENDER(CastNode)
DEF_CODEGEN(CastNode)
DEF_ANNOTATE(CastNode)
DEF_TRAVERSE(CastNode)
DEF_TRANSFORM(CastNode)
DEF_TYPIFY(CastNode)


//--------------------------------------------------------------------------------

SlotRefNode::SlotRefNode(const SrcPos& srcpos,
                         std::shared_ptr<AptNode> base, const String& slotName)
  : AptNode(srcpos),
    fBase(std::move(base)),
    fSlotName(slotName)
{ }


std::shared_ptr<AptNode>
SlotRefNode::clone() const
{
  return cloneScope(this, makeSlotRefNode(fSrcPos, nodeClone(fBase), fSlotName));
}


std::shared_ptr<AptNode>
SlotRefNode::base() const
{
  return fBase;
}


void
SlotRefNode::setBase(std::shared_ptr<AptNode> base)
{
  fBase = std::move(base);
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
herschel::makeNodeList()
{
  return NodeList();
}


NodeList
herschel::makeNodeList(std::shared_ptr<AptNode> n1)
{
  NodeList nl;
  if (n1)
    nl.push_back(std::move(n1));
  return nl;
}


NodeList
herschel::makeNodeList(std::initializer_list<std::shared_ptr<AptNode>> l)
{
  NodeList nl;
  nl.insert(nl.end(), l.begin(), l.end());
  return nl;
}


NodeList&
herschel::appendNodes(NodeList& dst, const NodeList& nl)
{
  if (!nl.empty())
    dst.insert(dst.end(), nl.begin(), nl.end());
  return dst;
}


std::shared_ptr<AptNode>
herschel::singletonNodeListOrNull(const NodeList& nl)
{
  hr_assert(nl.size() < 2);
  if (nl.size() == 1)
    return nl[0];
  return nullptr;
}
