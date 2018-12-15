/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "ast.hpp"

//#include "annotate.hpp"
//#include "codegen.hpp"
#include "predefined.hpp"
#include "scope.hpp"
#include "strbuf.hpp"
//#include "transform.hpp"
//#include "traverse.hpp"
//#include "typify.hpp"
#include "utils.hpp"
#include "xmlrenderer.hpp"

//#include "llvm/IR/Value.h"

#include <cstring>
#include <map>
#include <string>


namespace herschel {


template <typename T>
std::shared_ptr<T> nodeClone(std::shared_ptr<T> node)
{
  if (node)
    return node->clone();
  return nullptr;
}


void copyNodes(NodeList* dst, const NodeList* src)
{
  for (auto& s : *src) {
    dst->push_back(nodeClone(s));
  }
}


NodeList copyNodes(const NodeList& src)
{
  NodeList dst;
  copyNodes(&dst, &src);
  return dst;
}


zstring convkind2str(TypeConvKind kind)
{
  switch (kind) {
  case kNoConv: return "no-conv";
  case kTypeCheckConv: return "type-check";
  case kAtom2PlainConv: return "atom2plain";
  case kPlain2AtomConv: return "plain2atom";
  }

  return "unknown-conv";
}


//----------------------------------------------------------------------------

AstNode::AstNode(const SrcPos& srcpos)
    : fSrcPos(srcpos)
    , fTypeConvKind(kNoConv)
    , fIsInTailPos(false)
    , fIsSingleTypeRequired(false)
{
}


AstNode::AstNode(const SrcPos& srcpos, const Type& type)
    : fSrcPos(srcpos)
    , fType(type)
    , fTypeConvKind(kNoConv)
    , fIsInTailPos(false)
    , fIsSingleTypeRequired(false)
{
}


AstNode::~AstNode() {}


const SrcPos& AstNode::srcpos() const
{
  return fSrcPos;
}


std::shared_ptr<Scope> AstNode::scope() const
{
  return fScope;
}


void AstNode::setScope(std::shared_ptr<Scope> scope)
{
  fScope = scope;
}


const Type& AstNode::type() const
{
  return fType;
}


void AstNode::setType(const Type& type)
{
  fType = type;
}


const Type& AstNode::dstType() const
{
  return fDstType;
}


void AstNode::setDstType(const Type& type)
{
  fDstType = type;
}


TypeConvKind AstNode::typeConv() const
{
  return fTypeConvKind;
}


void AstNode::setTypeConv(TypeConvKind typeConv)
{
  fTypeConvKind = typeConv;
}


bool AstNode::isInTailPos() const
{
  return fIsInTailPos;
}


void AstNode::setIsInTailPos(bool value)
{
  fIsInTailPos = value;
}


bool AstNode::isSingleTypeRequired() const
{
  return fIsSingleTypeRequired;
}


void AstNode::setIsSingleTypeRequired(bool value)
{
  fIsSingleTypeRequired = value;
}


//----------------------------------------------------------------------------

LoopAnnotatable::LoopAnnotatable()
    : fLoopId(0)
{
}


int LoopAnnotatable::loopId() const
{
  return fLoopId;
}


void LoopAnnotatable::setLoopId(int loopId)
{
  fLoopId = loopId;
}


//----------------------------------------------------------------------------

DelayTypeAnnotatable::DelayTypeAnnotatable()
    : fDelayTypeSpec(false)
{
}


bool DelayTypeAnnotatable::isTypeSpecDelayed() const
{
  return fDelayTypeSpec;
}


void DelayTypeAnnotatable::setTypeSpecDelayed(bool value)
{
  fDelayTypeSpec = value;
}


//----------------------------------------------------------------------------

const String& LinkableSymbol::linkage() const
{
  return fLinkage;
}


void LinkableSymbol::setLinkage(const String& linkage)
{
  fLinkage = linkage;
}


bool LinkableSymbol::hasCLinkage() const
{
  return fLinkage == String("C");
}


//----------------------------------------------------------------------------

ListNode::ListNode(const SrcPos& srcpos)
    : AstNode(srcpos)
{
}


NodeList& ListNode::children()
{
  return fChildren;
}


const NodeList& ListNode::children() const
{
  return fChildren;
}


void ListNode::appendNode(std::shared_ptr<AstNode> node)
{
  fChildren.push_back(node);
}


void ListNode::appendNodes(const NodeList& nodes)
{
  if (!nodes.empty())
    fChildren.insert(fChildren.end(), nodes.begin(), nodes.end());
}


//----------------------------------------------------------------------------

UndefNode::UndefNode()
    : AstNode(SrcPos())
{
}


std::shared_ptr<AstNode> UndefNode::clone() const
{
  return makeUndefNode();
}


//----------------------------------------------------------------------------

template <typename T>
std::shared_ptr<T> cloneScope(const T* src, std::shared_ptr<T> dst)
{
  dst->setScope(src->scope());
  dst->setType(src->type());
  dst->setDstType(src->dstType());
  dst->setTypeConv(src->typeConv());
  return dst;
}


StringNode::StringNode(const SrcPos& srcpos, const String& value)
    : AstNode(srcpos)
    , fValue(value)
{
}


std::shared_ptr<AstNode> StringNode::clone() const
{
  return cloneScope(this, makeStringNode(fSrcPos, fValue));
}


const String& StringNode::value() const
{
  return fValue;
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const SrcPos& srcpos, const String& value)
    : AstNode(srcpos)
    , fValue(value)
{
}


std::shared_ptr<AstNode> KeywordNode::clone() const
{
  return cloneScope(this, makeKeywordNode(fSrcPos, fValue));
}


const String& KeywordNode::value() const
{
  return fValue;
}


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const SrcPos& srcpos, const String& value)
    : AstNode(srcpos)
    , fValue(value)
    , fRefersTo(kFreeVar)
    , fIsShared(false)
{
}


SymbolNode::SymbolNode(const SrcPos& srcpos, const String& value,
                       const TypeVector& generics)
    : AstNode(srcpos)
    , fValue(value)
    , fGenerics(generics)
    , fRefersTo(kFreeVar)
    , fIsShared(false)
{
}


std::shared_ptr<AstNode> SymbolNode::clone() const
{
  auto newnd = makeSymbolNode(fSrcPos, fValue);
  newnd->setLinkage(linkage());
  return cloneScope(this, newnd);
}


void SymbolNode::setName(const String& nm)
{
  fValue = nm;
}


const String& SymbolNode::name() const
{
  return fValue;
}


const TypeVector& SymbolNode::generics() const
{
  return fGenerics;
}


std::string SymbolNode::string() const
{
  return std::string(StrHelper(fValue));
}


SymReferType SymbolNode::refersTo() const
{
  return fRefersTo;
}


void SymbolNode::setRefersTo(SymReferType type, bool isShared)
{
  fRefersTo = type;
  fIsShared = isShared;
}


bool SymbolNode::isShared() const
{
  return fIsShared;
}


//----------------------------------------------------------------------------

ArrayTypeNode::ArrayTypeNode(const SrcPos& srcpos, std::shared_ptr<AstNode> typeNode)
    : AstNode(srcpos)
    , fTypeNode(std::move(typeNode))
{
}


std::shared_ptr<AstNode> ArrayTypeNode::typeNode() const
{
  return fTypeNode;
}


std::shared_ptr<AstNode> ArrayTypeNode::clone() const
{
  return cloneScope(this, makeArrayTypeNode(fSrcPos, nodeClone(fTypeNode)));
}


//--------------------------------------------------------------------------

TypeNode::TypeNode(const SrcPos& srcpos, const Type& type)
    : AstNode(srcpos, type)
{
}


std::shared_ptr<AstNode> TypeNode::clone() const
{
  return cloneScope(this, makeTypeNode(fSrcPos, fType.clone()));
}


//----------------------------------------------------------------------------

BaseNumberNode::BaseNumberNode(const SrcPos& srcpos, bool isImaginary, const Type& type)
    : AstNode(srcpos, type)
    , fIsImaginary(isImaginary)
{
}


bool BaseNumberNode::isImaginary() const
{
  return fIsImaginary;
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int64_t value, bool isImaginary, const Type& type)
    : NumberNode<int64_t>(srcpos, value, isImaginary, type)
{
}


std::shared_ptr<AstNode> IntNode::clone() const
{
  return cloneScope(this, makeIntNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, double value, bool isImaginary, const Type& type)
    : NumberNode<double>(srcpos, value, isImaginary, type)
{
}


std::shared_ptr<AstNode> RealNode::clone() const
{
  return cloneScope(this, makeRealNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const SrcPos& srcpos, const Rational& value, bool isImaginary,
                           const Type& type)
    : NumberNode<Rational>(srcpos, value, isImaginary, type)
{
}


std::shared_ptr<AstNode> RationalNode::clone() const
{
  return cloneScope(this, makeRationalNode(fSrcPos, fValue, fIsImaginary, fType.clone()));
}


//----------------------------------------------------------------------------

CharNode::CharNode(const SrcPos& srcpos, Char value)
    : AstNode(srcpos)
    , fValue(value)
{
}


std::shared_ptr<AstNode> CharNode::clone() const
{
  return cloneScope(this, makeCharNode(fSrcPos, fValue));
}


Char CharNode::value() const
{
  return fValue;
}


//----------------------------------------------------------------------------

BoolNode::BoolNode(const SrcPos& srcpos, bool value)
    : AstNode(srcpos)
    , fValue(value)
{
}


std::shared_ptr<AstNode> BoolNode::clone() const
{
  return cloneScope(this, makeBoolNode(fSrcPos, fValue));
}


bool BoolNode::value() const
{
  return fValue;
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos)
    : ListNode(srcpos)
{
}


std::shared_ptr<AstNode> CompileUnitNode::clone() const
{
  auto node = makeCompileUnitNode(fSrcPos);
  copyNodes(&node->fChildren, &fChildren);
  return cloneScope(this, std::move(node));
}


//----------------------------------------------------------------------------

BaseDefNode::BaseDefNode(const SrcPos& srcpos, std::shared_ptr<AstNode> defined)
    : AstNode(srcpos)
    , fDefined(std::move(defined))
{
}


std::shared_ptr<AstNode> BaseDefNode::defNode() const
{
  return fDefined;
}


void BaseDefNode::setDefNode(std::shared_ptr<AstNode> val)
{
  fDefined = std::move(val);
}


//----------------------------------------------------------------------------

LetNode::LetNode(std::shared_ptr<AstNode> node)
    : BaseDefNode(node->srcpos(), std::move(node))
{
}


std::shared_ptr<AstNode> LetNode::clone() const
{
  return cloneScope(this, makeLetNode(nodeClone(fDefined)));
}


//----------------------------------------------------------------------------

DefNode::DefNode(std::shared_ptr<AstNode> node)
    : BaseDefNode(node->srcpos(), std::move(node))
{
}


std::shared_ptr<AstNode> DefNode::clone() const
{
  return cloneScope(this, makeDefNode(nodeClone(fDefined)));
}


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos, const String& symbolName, const Type& type,
                         std::shared_ptr<AstNode> initExpr)
    : AstNode(srcpos, type)
    , fSymbolName(symbolName)
    , fInitExpr(std::move(initExpr))
    , fAllocType(kAlloc_Local)
{
}


std::shared_ptr<AstNode> BindingNode::initExpr() const
{
  return fInitExpr;
}


void BindingNode::setAllocType(BindingAllocType allocType)
{
  fAllocType = allocType;
}


BindingAllocType BindingNode::allocType() const
{
  return fAllocType;
}


const String& BindingNode::name() const
{
  return fSymbolName;
}


//----------------------------------------------------------------------------

VardefNode::VardefNode(const SrcPos& srcpos, const String& symbolName, VardefFlags flags,
                       bool isLocal, const Type& type, std::shared_ptr<AstNode> init)
    : BindingNode(srcpos, symbolName, type, std::move(init))
    , fIsLocal(isLocal)
    , fFlags(flags)
{
  if (initExpr())
    initExpr()->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> VardefNode::clone() const
{
  auto n = makeVardefNode(fSrcPos, fSymbolName, fFlags, fIsLocal, fType.clone(),
                          nodeClone(fInitExpr));
  n->setLinkage(fLinkage);
  return cloneScope(this, std::move(n));
}


bool VardefNode::isConst() const
{
  return fFlags == kConstVar;
}


bool VardefNode::isConfig() const
{
  return fFlags == kConfigVar;
}


bool VardefNode::isEnum() const
{
  return fFlags == kEnumVar;
}


bool VardefNode::isLocal() const
{
  return fIsLocal;
}


VardefFlags VardefNode::flags() const
{
  return fFlags;
}


//----------------------------------------------------------------------------

ParamNode::ParamNode(const SrcPos& srcpos, const String& keyName,
                     const String& symbolName, ParamFlags flags, const Type& type,
                     std::shared_ptr<AstNode> init)
    : BindingNode(srcpos, symbolName, type, std::move(init))
    , fKey(keyName)
    , fFlags(flags)
{
  hr_assert(implies(fFlags == kNamedArg, !fKey.isEmpty()));

  if (initExpr())
    initExpr()->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> ParamNode::clone() const
{
  return cloneScope(this, makeParamNode(fSrcPos, fKey, fSymbolName, fFlags, fType.clone(),
                                        nodeClone(fInitExpr)));
}


ParamFlags ParamNode::flags() const
{
  return fFlags;
}


bool ParamNode::isPositional() const
{
  return (fFlags & kPosArg) != 0;
}


bool ParamNode::isRestArg() const
{
  return (fFlags & kRestArg) != 0;
}


bool ParamNode::isSpecArg() const
{
  return (fFlags & kSpecArg) != 0;
}


const String& ParamNode::key() const
{
  return fKey;
}


//----------------------------------------------------------------------------

SlotdefNode::SlotdefNode(const SrcPos& srcpos, const String& symbolName,
                         unsigned int flags, const Type& type,
                         std::shared_ptr<AstNode> initExpr)
    : BindingNode(srcpos, symbolName, type, std::move(initExpr))
    , fFlags(flags)
{
}


std::shared_ptr<AstNode> SlotdefNode::clone() const
{
  return cloneScope(this, makeSlotdefNode(fSrcPos, fSymbolName, fFlags, fType.clone(),
                                          nodeClone(fInitExpr)));
}


unsigned int SlotdefNode::flags() const
{
  return fFlags;
}


//----------------------------------------------------------------------------

ArrayNode::ArrayNode(const SrcPos& srcpos)
    : ListNode(srcpos)
{
}


std::shared_ptr<AstNode> ArrayNode::clone() const
{
  auto an = makeArrayNode(fSrcPos);
  copyNodes(&an->fChildren, &fChildren);
  return cloneScope(this, std::move(an));
}


//----------------------------------------------------------------------------

VectorNode::VectorNode(const SrcPos& srcpos)
    : ListNode(srcpos)
{
}


std::shared_ptr<AstNode> VectorNode::clone() const
{
  auto vect = makeVectorNode(fSrcPos);
  copyNodes(&vect->fChildren, &fChildren);
  return cloneScope(this, std::move(vect));
}


//----------------------------------------------------------------------------

DictNode::DictNode(const SrcPos& srcpos)
    : ListNode(srcpos)
{
}


std::shared_ptr<AstNode> DictNode::clone() const
{
  auto dict = makeDictNode(fSrcPos);
  copyNodes(&dict->fChildren, &fChildren);
  return cloneScope(this, std::move(dict));
}


void DictNode::addPair(std::shared_ptr<AstNode> key, std::shared_ptr<AstNode> value)
{
  hr_assert(key);
  hr_assert(value);

  appendNode(makeBinaryNode(key->srcpos(), key, kOpMapTo, std::move(value)));
}


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(const SrcPos& srcpos, std::shared_ptr<AstNode> left,
                       OperatorType op, std::shared_ptr<AstNode> right)
    : AstNode(srcpos)
    , fLeft(std::move(left))
    , fRight(std::move(right))
    , fOp(op)
{
  hr_assert(fOp != kOpInvalid);
}


std::shared_ptr<AstNode> BinaryNode::clone() const
{
  return cloneScope(this,
                    makeBinaryNode(fSrcPos, nodeClone(fLeft), fOp, nodeClone(fRight)));
}


OperatorType BinaryNode::op() const
{
  return fOp;
}


std::shared_ptr<AstNode> BinaryNode::left() const
{
  return fLeft;
}


void BinaryNode::setLeft(std::shared_ptr<AstNode> node)
{
  fLeft = std::move(node);
}


std::shared_ptr<AstNode> BinaryNode::right() const
{
  return fRight;
}


void BinaryNode::setRight(std::shared_ptr<AstNode> node)
{
  fRight = std::move(node);
}


bool BinaryNode::isMapTo() const
{
  return fOp == kOpMapTo;
}


//----------------------------------------------------------------------------

UnaryNode::UnaryNode(const SrcPos& srcpos, UnaryOperatorType op,
                     std::shared_ptr<AstNode> base)
    : AstNode(srcpos)
    , fBase(std::move(base))
    , fOp(op)
{
}


std::shared_ptr<AstNode> UnaryNode::base() const
{
  return fBase;
}


void UnaryNode::setBase(std::shared_ptr<AstNode> base)
{
  fBase = std::move(base);
}


UnaryOperatorType UnaryNode::op() const
{
  return fOp;
}


std::shared_ptr<AstNode> UnaryNode::clone() const
{
  return cloneScope(this, makeUnaryNode(fSrcPos, fOp, nodeClone(fBase)));
}


//------------------------------------------------------------------------------

RangeNode::RangeNode(const SrcPos& srcpos, std::shared_ptr<AstNode> from,
                     std::shared_ptr<AstNode> to, std::shared_ptr<AstNode> by)
    : AstNode(srcpos)
    , fFrom(std::move(from))
    , fTo(std::move(to))
    , fBy(std::move(by))
{
}


std::shared_ptr<AstNode> RangeNode::clone() const
{
  return cloneScope(
      this, makeRangeNode(fSrcPos, nodeClone(fFrom), nodeClone(fTo), nodeClone(fBy)));
}


std::shared_ptr<AstNode> RangeNode::from() const
{
  return fFrom;
}


void RangeNode::setFrom(std::shared_ptr<AstNode> node)
{
  fFrom = std::move(node);
}


std::shared_ptr<AstNode> RangeNode::to() const
{
  return fTo;
}


void RangeNode::setTo(std::shared_ptr<AstNode> node)
{
  fTo = std::move(node);
}


std::shared_ptr<AstNode> RangeNode::by() const
{
  return fBy;
}


void RangeNode::setBy(std::shared_ptr<AstNode> node)
{
  fBy = std::move(node);
}


//--------------------------------------------------------------------------

AssignNode::AssignNode(const SrcPos& srcpos, std::shared_ptr<AstNode> lvalue,
                       std::shared_ptr<AstNode> rvalue)
    : AstNode(srcpos)
    , fLValue(std::move(lvalue))
    , fRValue(std::move(rvalue))
{
  if (fRValue)
    fRValue->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> AssignNode::clone() const
{
  return cloneScope(this,
                    makeAssignNode(fSrcPos, nodeClone(fLValue), nodeClone(fRValue)));
}


std::shared_ptr<AstNode> AssignNode::lvalue() const
{
  return fLValue;
}


std::shared_ptr<AstNode> AssignNode::rvalue() const
{
  return fRValue;
}


void AssignNode::setLvalue(std::shared_ptr<AstNode> val)
{
  fLValue = std::move(val);
}


void AssignNode::setRvalue(std::shared_ptr<AstNode> val)
{
  fRValue = std::move(val);
}


//------------------------------------------------------------------------------

IfNode::IfNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
               std::shared_ptr<AstNode> consequent, std::shared_ptr<AstNode> alternate)
    : AstNode(srcpos)
    , fTest(std::move(test))
    , fConsequent(std::move(consequent))
    , fAlternate(std::move(alternate))
{
}


std::shared_ptr<AstNode> IfNode::clone() const
{
  return cloneScope(this, makeIfNode(fSrcPos, nodeClone(fTest), nodeClone(fConsequent),
                                     nodeClone(fAlternate)));
}


std::shared_ptr<AstNode> IfNode::test() const
{
  return fTest;
}


std::shared_ptr<AstNode> IfNode::consequent() const
{
  return fConsequent;
}


std::shared_ptr<AstNode> IfNode::alternate() const
{
  return fAlternate;
}


void IfNode::setTest(std::shared_ptr<AstNode> node)
{
  fTest = std::move(node);
}


void IfNode::setConsequent(std::shared_ptr<AstNode> node)
{
  fConsequent = std::move(node);
}


void IfNode::setAlternate(std::shared_ptr<AstNode> node)
{
  fAlternate = std::move(node);
}


//------------------------------------------------------------------------------

SelectNode::SelectNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
                       std::shared_ptr<AstNode> comparator)
    : AstNode(srcpos)
    , fTest(std::move(test))
    , fComparator(std::move(comparator))
{
}


std::shared_ptr<AstNode> SelectNode::clone() const
{
  auto newNode = makeSelectNode(fSrcPos, nodeClone(fTest), nodeClone(fComparator));
  for (auto& mapping : fMappings) {
    newNode->fMappings.push_back(
        SelectMapping(copyNodes(mapping.fTestValues), nodeClone(mapping.fConsequent)));
  }

  return cloneScope(this, std::move(newNode));
}


void SelectNode::addMapping(const NodeList& mappings, std::shared_ptr<AstNode> consequent)
{
  fMappings.push_back(SelectMapping(mappings, std::move(consequent)));
}


void SelectNode::addMapping(std::shared_ptr<AstNode> mapping,
                            std::shared_ptr<AstNode> consequent)
{
  fMappings.push_back(
      SelectMapping(std::vector<NodeList::value_type>{ mapping }, std::move(consequent)));
}


void SelectNode::addElseMapping(std::shared_ptr<AstNode> alternate)
{
  fMappings.push_back(SelectMapping(NodeList(), std::move(alternate)));
}


std::shared_ptr<AstNode> SelectNode::test() const
{
  return fTest;
}


void SelectNode::setTest(std::shared_ptr<AstNode> nd)
{
  fTest = std::move(nd);
}


std::shared_ptr<AstNode> SelectNode::comparator() const
{
  return fComparator;
}


void SelectNode::setComparator(std::shared_ptr<AstNode> nd)
{
  fComparator = std::move(nd);
}


SelectNode::SelectMappingVector& SelectNode::mappings()
{
  return fMappings;
}


size_t SelectNode::mappingCount() const
{
  return fMappings.size();
}


const SelectNode::SelectMapping& SelectNode::mappingAt(size_t i) const
{
  return fMappings[i];
}


void SelectNode::setConsequentAt(size_t i, std::shared_ptr<AstNode> consq)
{
  fMappings[i].fConsequent = std::move(consq);
}


void SelectNode::setTestValueAt(size_t i, size_t j, std::shared_ptr<AstNode> value)
{
  fMappings[i].fTestValues[j] = std::move(value);
}


SelectNode::SelectMapping::SelectMapping(const NodeList& values,
                                         std::shared_ptr<AstNode> consequent)
    : fTestValues(values)
    , fConsequent(std::move(consequent))
{
}


SelectNode::SelectMapping::SelectMapping(const SelectMapping& other)
    : fTestValues(other.fTestValues)
    , fConsequent(other.fConsequent)
{
}


//------------------------------------------------------------------------------

MatchNode::MatchNode(const SrcPos& srcpos, std::shared_ptr<AstNode> expr)
    : AstNode(srcpos)
    , fExpr(std::move(expr))
{
}


std::shared_ptr<AstNode> MatchNode::clone() const
{
  auto newNode = makeMatchNode(fSrcPos, nodeClone(fExpr));
  for (auto& mapping : fMappings) {
    newNode->fMappings.push_back(MatchMapping(mapping.fSrcPos, mapping.fVarName,
                                              mapping.fMatchType.clone(),
                                              nodeClone(mapping.fConsequent)));
  }

  return cloneScope(this, std::move(newNode));
}


void MatchNode::addMapping(const SrcPos& srcpos, const String& varName,
                           const Type& matchType, std::shared_ptr<AstNode> consequent)
{
  fMappings.push_back(MatchMapping(srcpos, varName, matchType, std::move(consequent)));
}


std::shared_ptr<AstNode> MatchNode::expr() const
{
  return fExpr;
}


void MatchNode::setExpr(std::shared_ptr<AstNode> nd)
{
  fExpr = std::move(nd);
}


MatchNode::MatchMappingVector& MatchNode::mappings()
{
  return fMappings;
}


size_t MatchNode::mappingCount() const
{
  return fMappings.size();
}


const MatchNode::MatchMapping& MatchNode::mappingAt(size_t i) const
{
  return fMappings[i];
}


void MatchNode::setConsequentAt(size_t i, std::shared_ptr<AstNode> consq)
{
  fMappings[i].fConsequent = std::move(consq);
}


MatchNode::MatchMapping::MatchMapping(const SrcPos& srcpos, const String& varName,
                                      const Type& matchType,
                                      std::shared_ptr<AstNode> consequent)
    : fSrcPos(srcpos)
    , fVarName(varName)
    , fMatchType(matchType)
    , fConsequent(std::move(consequent))
{
}


MatchNode::MatchMapping::MatchMapping(const MatchMapping& other)
    : fSrcPos(other.fSrcPos)
    , fVarName(other.fVarName)
    , fMatchType(other.fMatchType)
    , fConsequent(other.fConsequent)
{
}


//----------------------------------------------------------------------------

BlockNode::BlockNode(const SrcPos& srcpos)
    : ListNode(srcpos)
{
}


std::shared_ptr<AstNode> BlockNode::clone() const
{
  auto block = makeBlockNode(fSrcPos);
  copyNodes(&block->fChildren, &fChildren);
  return cloneScope(this, std::move(block));
}


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos& srcpos, const NodeList& params,
                           const Type& retType, std::shared_ptr<AstNode> body)
    : ListNode(srcpos)
    , fRetType(retType)
    , fBody(std::move(body))
{
  fChildren.assign(params.begin(), params.end());
}


std::shared_ptr<AstNode> FunctionNode::clone() const
{
  return cloneScope(this, makeFunctionNode(fSrcPos, copyNodes(fChildren),
                                           fRetType.clone(), nodeClone(fBody)));
}


const NodeList& FunctionNode::params() const
{
  return fChildren;
}


NodeList& FunctionNode::params()
{
  return fChildren;
}


const Type& FunctionNode::retType() const
{
  return fRetType;
}


void FunctionNode::setRetType(const Type& type)
{
  fRetType = std::move(type);
}


std::shared_ptr<AstNode> FunctionNode::body() const
{
  return fBody;
}


void FunctionNode::setBody(std::shared_ptr<AstNode> node)
{
  fBody = std::move(node);
}


size_t FunctionNode::specializedParamsCount() const
{
  return std::count_if(params().begin(), params().end(),
                       [](const NodeList::value_type& nd) {
                         auto prm = dynamic_cast<ParamNode*>(nd.get());
                         return prm && prm->isSpecArg();
                       });
}


bool FunctionNode::hasSpecializedParams() const
{
  return specializedParamsCount() > 0;
}


//----------------------------------------------------------------------------

FuncDefNode::FuncDefNode(const SrcPos& srcpos, const String& sym, unsigned int flags,
                         const NodeList& params, const Type& retType,
                         std::shared_ptr<AstNode> body)
    : FunctionNode(srcpos, params, retType, std::move(body))
    , fSym(sym)
    , fFlags(flags)
{
}

std::shared_ptr<AstNode> FuncDefNode::clone() const
{
  auto n = makeFuncDefNode(fSrcPos, fSym, fFlags, copyNodes(fChildren), fRetType.clone(),
                           nodeClone(fBody));
  n->setLinkage(fLinkage);
  return cloneScope(this, std::move(n));
}


bool FuncDefNode::isGeneric() const
{
  return (fFlags & kFuncIsGeneric) != 0;
}


bool FuncDefNode::isMethod() const
{
  return (fFlags & kFuncIsMethod) != 0;
}


bool FuncDefNode::isAbstract() const
{
  return (fFlags & kFuncIsAbstract) != 0;
}


const String& FuncDefNode::name() const
{
  return fSym;
}


bool FuncDefNode::isAppMain() const
{
  return name() == Names::kAppMain;
}


//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, std::shared_ptr<AstNode> baseNd)
    : ListNode(srcpos)
    , fBase(std::move(baseNd))
{
  if (base())
    base()->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> ApplyNode::clone() const
{
  auto apply = makeApplyNode(fSrcPos, nodeClone(fBase));
  copyNodes(&apply->fChildren, &fChildren);
  return cloneScope(this, std::move(apply));
}


std::shared_ptr<AstNode> ApplyNode::base() const
{
  return fBase;
}


void ApplyNode::setBase(std::shared_ptr<AstNode> node)
{
  fBase = std::move(node);
}


bool ApplyNode::isSimpleCall() const
{
  auto sym = dynamic_cast<SymbolNode*>(base().get());
  return sym && sym->generics().empty();
}


String ApplyNode::simpleCallName() const
{
  auto sym = dynamic_cast<SymbolNode*>(base().get());
  return (sym ? sym->name() : String());
}


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos, const String& key,
                       std::shared_ptr<AstNode> value)
    : AstNode(srcpos)
    , fKey(key)
    , fValue(std::move(value))
{
  hr_assert(fValue);
  if (fValue)
    fValue->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> KeyargNode::clone() const
{
  return cloneScope(this, makeKeyargNode(fSrcPos, fKey, nodeClone(fValue)));
}


const String& KeyargNode::key() const
{
  return fKey;
}


std::shared_ptr<AstNode> KeyargNode::value() const
{
  return fValue;
}


void KeyargNode::setValue(std::shared_ptr<AstNode> node)
{
  fValue = std::move(node);
}


//----------------------------------------------------------------------------

WhileNode::WhileNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
                     std::shared_ptr<AstNode> body)
    : AstNode(srcpos)
    , fTest(std::move(test))
    , fBody(std::move(body))
{
  if (fTest)
    fTest->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> WhileNode::clone() const
{
  return cloneScope(this, makeWhileNode(fSrcPos, nodeClone(fTest), nodeClone(fBody)));
}


std::shared_ptr<AstNode> WhileNode::body() const
{
  return fBody;
}


void WhileNode::setBody(std::shared_ptr<AstNode> node)
{
  fBody = std::move(node);
}


std::shared_ptr<AstNode> WhileNode::test() const
{
  return fTest;
}


void WhileNode::setTest(std::shared_ptr<AstNode> node)
{
  fTest = std::move(node);
}


//----------------------------------------------------------------------------

TypeDefNode::TypeDefNode(const SrcPos& srcpos, const String& typeName, bool isRecord,
                         const Type& isa, const NodeList& slots)
    : AstNode(srcpos)
    , fTypeName(typeName)
    , fIsRecord(isRecord)
    , fSlots(slots)
    , fIsa(isa)
{
}


std::shared_ptr<AstNode> TypeDefNode::clone() const
{
  return cloneScope(this, makeTypeDefNode(fSrcPos, fTypeName, fIsRecord, fIsa.clone(),
                                          copyNodes(fSlots)));
}


const String& TypeDefNode::name() const
{
  return fTypeName;
}


const Type& TypeDefNode::defType() const
{
  return fIsa;
}


bool TypeDefNode::isRecord() const
{
  return fIsRecord;
}


const NodeList& TypeDefNode::slots() const
{
  return fSlots;
}


NodeList& TypeDefNode::slots()
{
  return fSlots;
}


//----------------------------------------------------------------------------

CastNode::CastNode(const SrcPos& srcpos, std::shared_ptr<AstNode> base, const Type& type)
    : AstNode(srcpos, type)
    , fBase(std::move(base))
{
  if (fBase)
    fBase->setIsSingleTypeRequired(true);
}


std::shared_ptr<AstNode> CastNode::base() const
{
  return fBase;
}


void CastNode::setBase(std::shared_ptr<AstNode> node)
{
  fBase = std::move(node);
}


std::shared_ptr<AstNode> CastNode::clone() const
{
  return cloneScope(this, makeCastNode(fSrcPos, nodeClone(fBase), fType.clone()));
}


//--------------------------------------------------------------------------------

SlotRefNode::SlotRefNode(const SrcPos& srcpos, std::shared_ptr<AstNode> base,
                         const String& slotName)
    : AstNode(srcpos)
    , fBase(std::move(base))
    , fSlotName(slotName)
{
}


std::shared_ptr<AstNode> SlotRefNode::clone() const
{
  return cloneScope(this, makeSlotRefNode(fSrcPos, nodeClone(fBase), fSlotName));
}


std::shared_ptr<AstNode> SlotRefNode::base() const
{
  return fBase;
}


void SlotRefNode::setBase(std::shared_ptr<AstNode> base)
{
  fBase = std::move(base);
}


String SlotRefNode::slotName() const
{
  return fSlotName;
}


//--------------------------------------------------------------------------------

NodeList makeNodeList()
{
  return NodeList();
}


NodeList makeNodeList(std::shared_ptr<AstNode> n1)
{
  NodeList nl;
  if (n1)
    nl.push_back(std::move(n1));
  return nl;
}


NodeList makeNodeList(std::initializer_list<std::shared_ptr<AstNode>> l)
{
  NodeList nl;
  nl.insert(nl.end(), l.begin(), l.end());
  return nl;
}


NodeList& appendNodes(NodeList& dst, const NodeList& nl)
{
  if (!nl.empty())
    dst.insert(dst.end(), nl.begin(), nl.end());
  return dst;
}


std::shared_ptr<AstNode> singletonNodeListOrNull(const NodeList& nl)
{
  hr_assert(nl.size() < 2);
  if (nl.size() == 1)
    return nl[0];
  return nullptr;
}

}  // namespace herschel
