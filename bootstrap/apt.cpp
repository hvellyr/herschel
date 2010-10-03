/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "sysconf.h"

#include <string.h>

#include "apt.h"
#include "strbuf.h"
#include "codegen.h"
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
AptNode::codegen(CodeGenerator* generator)
{
  return NULL;
}


//----------------------------------------------------------------------------

StringNode::StringNode(const SrcPos& srcpos, const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


StringNode*
StringNode::clone() const
{
  return new StringNode(fSrcPos, fValue);
}


void
StringNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
StringNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const SrcPos& srcpos, const String& value)
  :AptNode(srcpos),
   fValue(value)
{
}


KeywordNode*
KeywordNode::clone() const
{
  return new KeywordNode(fSrcPos, fValue);
}


void
KeywordNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
KeywordNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const SrcPos& srcpos, const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


SymbolNode::SymbolNode(const SrcPos& srcpos, const String& value,
                       const TypeVector& generics)
  : AptNode(srcpos),
    fValue(value),
    fGenerics(generics)
{ }


SymbolNode*
SymbolNode::clone() const
{
  return new SymbolNode(fSrcPos, fValue);
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
SymbolNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

ArraySymbolNode::ArraySymbolNode(const SrcPos& srcpos, const String& value)
  : SymbolNode(srcpos, value)
{ }


ArraySymbolNode*
ArraySymbolNode::clone() const
{
  return new ArraySymbolNode(fSrcPos, fValue);
}


void
ArraySymbolNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ArraySymbolNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int value, bool isImaginary,
                 const Type& type)
  : NumberNode<int>(srcpos, value, isImaginary, type)
{
}


IntNode*
IntNode::clone() const
{
  return new IntNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


llvm::Value*
IntNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


void
IntNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, double value, bool isImaginary,
                   const Type& type)
  : NumberNode<double>(srcpos, value, isImaginary, type)
{
}


RealNode*
RealNode::clone() const
{
  return new RealNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


void
RealNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
RealNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new RationalNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


void
RationalNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
RationalNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

CharNode::CharNode(const SrcPos& srcpos, Char value)
  : AptNode(srcpos),
    fValue(value)
{ }


CharNode*
CharNode::clone() const
{
  return new CharNode(fSrcPos, fValue);
}


void
CharNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
CharNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

BoolNode::BoolNode(const SrcPos& srcpos, bool value)
  : AptNode(srcpos),
    fValue(value)
{ }


BoolNode*
BoolNode::clone() const
{
  return new BoolNode(fSrcPos, fValue);
}


void
BoolNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
BoolNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

UnitConstant::UnitConstant(const SrcPos& srcpos, AptNode* value,
                           const TypeUnit& unit)
  : AptNode(srcpos),
    fValue(value),
    fUnit(unit)
{
}


UnitConstant*
UnitConstant::clone() const
{
  return new UnitConstant(fSrcPos, nodeClone(fValue), fUnit);
}


void
UnitConstant::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
UnitConstant::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return node.release();
}


void
CompileUnitNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
CompileUnitNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new LetNode(nodeClone(fDefined.obj()));
}


void
LetNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
LetNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

DefNode::DefNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


DefNode*
DefNode::clone() const
{
  return new DefNode(nodeClone(fDefined.obj()));
}


void
DefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
DefNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos,
                         const String& symbolName, const Type& type,
                         AptNode* initExpr)
  : AptNode(srcpos),
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

VardefNode::VardefNode(const SrcPos& srcpos,
                       const String& symbolName, VardefFlags flags,
                       const Type& type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fFlags(flags)
{
}


VardefNode*
VardefNode::clone() const
{
  return new VardefNode(fSrcPos, fSymbolName, fFlags,
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
VardefNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new ParamNode(fSrcPos, fKey, fSymbolName, fFlags,
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


const String&
ParamNode::key() const
{
  return fKey;
}


llvm::Value*
ParamNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new SlotdefNode(fSrcPos, fSymbolName, fFlags,
                         fType.clone(), nodeClone(fInitExpr));
}


void
SlotdefNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
SlotdefNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

ArrayNode*
ArrayNode::clone() const
{
  Ptr<ArrayNode> an = new ArrayNode(fSrcPos);
  copyNodes(&an->fChildren, &fChildren);
  return an.release();
}


void
ArrayNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ArrayNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

VectorNode*
VectorNode::clone() const
{
  Ptr<VectorNode> vect = new VectorNode(fSrcPos);
  copyNodes(&vect->fChildren, &fChildren);
  return vect.release();
}


void
VectorNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
VectorNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

DictNode*
DictNode::clone() const
{
  Ptr<DictNode> dict = new DictNode(fSrcPos);
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

  appendNode(new BinaryNode(key->srcpos(), key, kOpMapTo, value));
}


llvm::Value*
DictNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new BinaryNode(fSrcPos,
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
BinaryNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

NegateNode::NegateNode(const SrcPos& srcpos, AptNode* base)
  : AptNode(srcpos),
    fBase(base)
{ }


NegateNode*
NegateNode::clone() const
{
  return new NegateNode(fSrcPos, nodeClone(fBase));
}


void
NegateNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
NegateNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new RangeNode(fSrcPos,
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
RangeNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new ThenWhileNode(fSrcPos,
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
ThenWhileNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new AssignNode(fSrcPos,
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
AssignNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new IfNode(fSrcPos,
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
IfNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//------------------------------------------------------------------------------

SelectNode::SelectNode(const SrcPos& srcpos, AptNode* test, AptNode* comparator)
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
SelectNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
MatchNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new OnNode(fSrcPos, fKey,
                    copyNodes(fParams),
                    nodeClone(fBody));
}


void
OnNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
OnNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return block.release();
}


void
BlockNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
BlockNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos& srcpos,
                           const NodeList& params,
                           const Type& retType,
                           AptNode* body)
  : AptNode(srcpos),
    fRetType(retType),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


FunctionNode*
FunctionNode::clone() const
{
  return new FunctionNode(fSrcPos, copyNodes(fParams),
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
FunctionNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new FuncDefNode(fSrcPos, fSym, fFlags, fParams,
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
FuncDefNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return apply.release();
}


void
ApplyNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
ApplyNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

KeyargNode::KeyargNode(const SrcPos& srcpos, const String& key, AptNode* value)
  : AptNode(srcpos),
    fKey(key),
    fValue(value)
{ }


KeyargNode*
KeyargNode::clone() const
{
  return new KeyargNode(fSrcPos, fKey, nodeClone(fValue));
}


void
KeyargNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
KeyargNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
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
  return new WhileNode(fSrcPos,
                       nodeClone(fTest),
                       nodeClone(fBody));
}


void
WhileNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
WhileNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


//----------------------------------------------------------------------------

TypeNode::TypeNode(const SrcPos& srcpos, const String& typeName,
                   bool isClass,
                   const Type& isa,
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


TypeNode*
TypeNode::clone() const
{
  return new TypeNode(fSrcPos, fTypeName, fIsClass, fIsa.clone(),
                      copyNodes(fParams),
                      copyNodes(fSlots),
                      copyNodes(fReqProtocol),
                      copyNodes(fOnExprs));
}


void
TypeNode::render(XmlRenderer* renderer) const
{
  renderer->renderNode(this);
}


llvm::Value*
TypeNode::codegen(CodeGenerator* generator)
{
  return generator->codegen(this);
}


