/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"
#include "strbuf.h"

using namespace heather;


static void
displayOpenTag(Port<Octet>* port, const char* tagName)
{
  if (tagName != NULL)
    display(port, String() + "<" + tagName + ">");
}


static void
displayOpenTagAttrs(Port<Octet>* port, const char* tagName,
                    const char* attrs)
{
  if (tagName != NULL) {
    display(port, String() + "<" + tagName);
    if (attrs != NULL)
      display(port, String() + " " + attrs + ">");
    else
      display(port, ">");
  }
}


static void
displayCloseTag(Port<Octet>* port, const char* tagName)
{
  if (tagName != NULL)
    heather::display(port, String() + "</" + tagName + ">");
}


static void
displayEmptyTag(Port<Octet>* port, const char* tagName)
{
  if (tagName != NULL && ::strlen(tagName) > 0)
    heather::display(port, String() + "<" + tagName + "/>");
}


static void
displayTag(Port<Octet>* port, const char* tagName, const String& value)
{
  displayOpenTag(port, tagName);
  display(port, xmlEncode(value));
  displayCloseTag(port, tagName);
}


static void
displayTagAttr(Port<Octet>* port, const char* tagName,
               const char* attrs,
               const String& value)
{
  displayOpenTagAttrs(port, tagName, attrs);
  display(port, xmlEncode(value));
  displayCloseTag(port, tagName);
}


static void
displayNode(Port<Octet>* port, const char* tagName, AptNode* node)
{
  if (node != NULL) {
    displayOpenTag(port, tagName);
    node->display(port);
    displayCloseTag(port, tagName);
  }
}


static void
displayNodeList(Port<Octet>* port,
                const char* tagName,
                const NodeList& nodelist)
{
  if (!nodelist.empty()) {
    displayOpenTag(port, tagName);

    for (NodeList::const_iterator it = nodelist.begin();
         it != nodelist.end();
         it++)
    {
      AptNode* n = (*it);
      if (n != NULL)
        n->display(port);
    }

    displayCloseTag(port, tagName);
  }
}


// static void
// displayStringList(Port<Octet>* port,
//                   const char* outerTagName, const char* tagName,
//                   const StringList& strlist)
// {
//   if (!strlist.empty())
//     displayOpenTag(port, outerTagName);

//   for (StringList::const_iterator it = strlist.begin();
//        it != strlist.end();
//        it++)
//   {
//     String str = (*it);
//     displayOpenTag(port, tagName);
//     display(port, str);
//     displayCloseTag(port, tagName);
//   }

//   if (!strlist.empty())
//     displayCloseTag(port, outerTagName);
// }


// static void
// displayStringStringMap(Port<Octet>* port,
//                        const char* outerTagName, const char* tagName,
//                        const char* firstPairTagName, const char* secPairTagName,
//                        const StringStringMap& strMap)
// {
//   if (!strMap.empty())
//     displayOpenTag(port, outerTagName);

//   for (StringStringMap::const_iterator it = strMap.begin();
//        it != strMap.end();
//        it++)
//   {
//     displayOpenTag(port, tagName);
//     displayTag(port, firstPairTagName, it->first);
//     displayTag(port, secPairTagName, it->second);
//     displayCloseTag(port, tagName);
//   }

//   if (!strMap.empty())
//     displayCloseTag(port, outerTagName);
// }


static void
displayType(Port<Octet>* port, const char* tagName, const Type& type)
{
  if (type.isDef()) {
    const char* attrs = "xmlns:ty='http://heather.eyestep.org/types'";
    displayOpenTagAttrs(port, tagName, attrs);
    display(port, type.toString());
    displayCloseTag(port, tagName);
  }
}


static void
displayTypeVector(Port<Octet>* port, const char* tagName, const TypeVector& types)
{
  if (!types.empty()) {
    const char* attrs = "xmlns:ty='http://heather.eyestep.org/types'";
    displayOpenTagAttrs(port, tagName, attrs);
    for (size_t i = 0; i < types.size(); i++)
      display(port, types[i].toString());
    displayCloseTag(port, tagName);
  }
}


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


//----------------------------------------------------------------------------

StringNode::StringNode(const SrcPos& srcpos, const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


void
StringNode::display(Port<Octet>* port) const
{
  displayTag(port, "str", fValue);
}


StringNode*
StringNode::clone() const
{
  return new StringNode(fSrcPos, fValue);
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const SrcPos& srcpos, const String& value)
  :AptNode(srcpos),
   fValue(value)
{
}


void
KeywordNode::display(Port<Octet>* port) const
{
  displayTag(port, "keyw", fValue);
}


KeywordNode*
KeywordNode::clone() const
{
  return new KeywordNode(fSrcPos, fValue);
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


void
SymbolNode::display(Port<Octet>* port) const
{
  if (fGenerics.empty())
    displayTag(port, "symbol", fValue);
  else {
    StringBuffer attrs;
    attrs << "nm='" << fValue << "'";

    displayOpenTagAttrs(port, "symbol", StrHelper(attrs.toString()));
    displayTypeVector(port, "gen", fGenerics);
    displayCloseTag(port, "symbol");
  }
}


SymbolNode*
SymbolNode::clone() const
{
  return new SymbolNode(fSrcPos, fValue);
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int value, bool isImaginary,
                 const Type& type)
  : NumberNode<int>(srcpos, value, isImaginary, type)
{
}


void
IntNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "ty='" << fType.typeName() << "'";
  if (fIsImaginary)
    attrs << " imag='t'";

  displayTagAttr(port, "int", StrHelper(attrs.toString()), String() + fValue);
}


IntNode*
IntNode::clone() const
{
  return new IntNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, double value, bool isImaginary,
                   const Type& type)
  : NumberNode<double>(srcpos, value, isImaginary, type)
{
}


void
RealNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "ty='" << fType.typeName() << "'";
  if (fIsImaginary)
    attrs << " imag='t'";

  displayTagAttr(port, "real", StrHelper(attrs.toString()), String() + fValue);
}


RealNode*
RealNode::clone() const
{
  return new RealNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const SrcPos& srcpos,
                           const Rational& value, bool isImaginary,
                           const Type& type)
  : NumberNode<Rational>(srcpos, value, isImaginary, type)
{
}


void
RationalNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "ty='" << fType.typeName() << "'";
  if (fIsImaginary)
    attrs << " imag='t'";

  String val = String() + fValue.numerator() + "/" + fValue.denominator();
  displayTagAttr(port, "real", StrHelper(attrs.toString()), val);
}


RationalNode*
RationalNode::clone() const
{
  return new RationalNode(fSrcPos, fValue, fIsImaginary, fType.clone());
}


//----------------------------------------------------------------------------

CharNode::CharNode(const SrcPos& srcpos, Char value)
  : AptNode(srcpos),
    fValue(value)
{ }


void
CharNode::display(Port<Octet>* port) const
{
  displayTag(port, "char", fromInt(int(fValue)));
}


CharNode*
CharNode::clone() const
{
  return new CharNode(fSrcPos, fValue);
}


//----------------------------------------------------------------------------

BoolNode::BoolNode(const SrcPos& srcpos, bool value)
  : AptNode(srcpos),
    fValue(value)
{ }


void
BoolNode::display(Port<Octet>* port) const
{
  displayEmptyTag(port, (fValue ? "true" : "false"));
}


BoolNode*
BoolNode::clone() const
{
  return new BoolNode(fSrcPos, fValue);
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{}


void
CompileUnitNode::display(Port<Octet>* port) const
{
  displayNodeList(port, "compile-unit", fChildren);
}


CompileUnitNode*
CompileUnitNode::clone() const
{
  Ptr<CompileUnitNode> node = new CompileUnitNode(fSrcPos);
  copyNodes(&node->fChildren, &fChildren);
  return node.release();
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
LetNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "let");
  displayNode(port, NULL, fDefined);
  displayCloseTag(port, "let");
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
DefNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "def");
  displayNode(port, NULL, fDefined);
  displayCloseTag(port, "def");
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
VardefNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;

  attrs << "sym='" << fSymbolName << "'";

  switch (fFlags) {
  case kNormalVar:
    break;
  case kFluidVar:
    attrs << " type='fluid'";
    break;
  case kConstVar:
    attrs << " type='const'";
    break;
  case kConfigVar:
    attrs << " type='config'";
    break;
  }

  displayOpenTagAttrs(port, "vardef", StrHelper(attrs.toString()));

  displayType(port, "type", fType);
  displayNode(port, "init", fInitExpr);

  displayCloseTag(port, "vardef");
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
ParamNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;

  attrs << "sym='" << fSymbolName << "'";

  switch (fFlags) {
  case kPosArg:
    attrs << " type='pos'";
    break;
  case kSpecArg:
    attrs << " type='spec'";
    break;
  case kNamedArg:
    attrs << " type='key' key='" << fKey << "'";
    break;
  case kRestArg:
    attrs << " type='rest'";
    break;
  }

  displayOpenTagAttrs(port, "param", StrHelper(attrs.toString()));

  displayType(port, "type", fType);
  displayNode(port, "init", fInitExpr);

  displayCloseTag(port, "param");
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
SlotdefNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;

  attrs << "sym='" << fSymbolName << "'";

  if ((fFlags & kTransientSlot) != 0) {
    attrs << " transient='t'";
  }
  if ((fFlags & kReadonlySlot) != 0) {
    attrs << " readonly='t'";
  }
  if ((fFlags & kObservableSlot) != 0) {
    attrs << " observable='t'";
  }

  if ((fFlags & kPublicSlot) != 0) {
    attrs << " viz='public'";
  }
  else if ((fFlags & kOuterSlot) != 0) {
    attrs << " viz='outer'";
  }
  else if ((fFlags & kInnerSlot) != 0) {
    attrs << " viz='inner'";
  }

  displayOpenTagAttrs(port, "slot", StrHelper(attrs.toString()));
  displayType(port, "type", fType);
  displayNode(port, "init", fInitExpr);
  displayCloseTag(port, "slot");
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
ArrayNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "array");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "array");
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
VectorNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "vector");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "vector");
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
DictNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "dict");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "dict");
}


void
DictNode::addPair(AptNode* key, AptNode* value)
{
  assert(key != NULL);
  assert(value != NULL);

  appendNode(new BinaryNode(key->srcpos(), key, kOpMapTo, value));
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


static const char*
operatorName(OperatorType type)
{
  switch (type) {
  case kOpAppend:       return "++";
  case kOpAs:           return "as";
  case kOpAssign:       return "=";
  case kOpBitAnd:       return "AND";
  case kOpBitOr:        return "OR";
  case kOpBitXor:       return "XOR";
  case kOpBy:           return "by";
  case kOpCompare:      return "<=>";
  case kOpDivide:       return "/";
  case kOpEqual:        return "==";
  case kOpExponent:     return "**";
  case kOpFold:         return "%";
  case kOpGreater:      return ">";
  case kOpGreaterEqual: return ">=";
  case kOpIn:           return "in";
  case kOpIsa:          return "isa";
  case kOpLess:         return "<";
  case kOpLessEqual:    return "<=";
  case kOpLogicalAnd:   return "and";
  case kOpLogicalOr:    return "or";
  case kOpMapTo:        return "->";
  case kOpMinus:        return "-";
  case kOpMod:          return "mod";
  case kOpMultiply:     return "*";
  case kOpPlus:         return "+";
  case kOpRange:        return "..";
  case kOpShiftLeft:    return "<<";
  case kOpShiftRight:   return ">>";
  case kOpUnequal:      return "<>";
  case kOpThen:         return "then";
  case kOpWhile:        return "while";

  case kOpInvalid:
    assert(0);
  }

  return NULL;
}


void
BinaryNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "op='" << xmlEncode(operatorName(fOp)) << "'";
  displayOpenTagAttrs(port, "binary", StrHelper(attrs.toString()));
  displayNode(port, NULL, fLeft);
  displayNode(port, NULL, fRight);
  displayCloseTag(port, "binary");
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
NegateNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "neg");
  displayNode(port, NULL, fBase);
  displayCloseTag(port, "neg");
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
RangeNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "range");
  displayNode(port, NULL, fFrom);
  displayNode(port, NULL, fTo);
  displayNode(port, NULL, fBy);
  displayCloseTag(port, "range");
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
ThenWhileNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "then-while");
  displayNode(port, NULL, fFirst);
  displayNode(port, NULL, fStep);
  displayNode(port, NULL, fTest);
  displayCloseTag(port, "then-while");
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


void
AssignNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "assign");
  displayNode(port, NULL, fLValue);
  displayNode(port, NULL, fRValue);
  displayCloseTag(port, "assign");
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
IfNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "if");
  displayNode(port, "test", fTest);
  displayNode(port, "then", fConsequent);
  displayNode(port, "else", fAlternate);
  displayCloseTag(port, "if");
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
SelectNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "select");
  displayNode(port, "test", fTest);
  displayNode(port, "comp", fComparator);
  for (size_t i = 0; i < fMappings.size(); i++) {
    if (fMappings[i].fTestValues.empty()) {
      displayNode(port, "alternate", fMappings[i].fConsequent);
    }
    else {
      displayOpenTag(port, "map");
      displayOpenTag(port, "values");
      for (size_t j = 0; j < fMappings[i].fTestValues.size(); j++) {
        displayNode(port, NULL, fMappings[i].fTestValues[j]);
      }
      displayCloseTag(port, "values");
      displayNode(port, "cons", fMappings[i].fConsequent);
      displayCloseTag(port, "map");
    }
  }
  displayCloseTag(port, "select");
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
MatchNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "match");
  displayNode(port, "test", fExpr);
  for (size_t i = 0; i < fMappings.size(); i++) {
    StringBuffer attrs;
    if (!fMappings[i].fVarName.isEmpty())
      attrs << "nm='" << xmlEncode(fMappings[i].fVarName) << "'";
    displayOpenTagAttrs(port, "map", StrHelper(attrs.toString()));
    displayType(port, "type", fMappings[i].fMatchType);
    displayNode(port, "cons", fMappings[i].fConsequent);
    displayCloseTag(port, "map");
  }
  displayCloseTag(port, "match");
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
OnNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "key='" << fKey << "'";
  displayOpenTagAttrs(port, "on", StrHelper(attrs.toString()));
  displayNodeList(port, "params", fParams);
  displayNode(port, "body", fBody);
  displayCloseTag(port, "on");
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
BlockNode::display(Port<Octet>* port) const
{
  displayNodeList(port, "block", fChildren);
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
FunctionNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "function");
  displayNodeList(port, "params", fParams);
  displayType(port, "rettype", fRetType);
  displayNode(port, "body", fBody);
  displayCloseTag(port, "function");
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
FuncDefNode::display(Port<Octet>* port) const
{
  const char* tag = isGeneric() ? "method" : "func";

  StringBuffer attrs;
  attrs << "sym='" << fSym << "'";

  if (isAbstract())
    attrs << " abstract='true'";

  displayOpenTagAttrs(port, tag, StrHelper(attrs.toString()));
  displayNodeList(port, "params", fParams);
  displayType(port, "rettype", fRetType);
  displayNode(port, "body", fBody);
  displayCloseTag(port, tag);
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
ApplyNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "apply");
  displayNode(port, NULL, fBase);
  displayOpenTag(port, "args");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "args");
  displayCloseTag(port, "apply");
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
KeyargNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << "key='" << fKey << "'";

  displayOpenTagAttrs(port, "arg", StrHelper(attrs.toString()));
  displayNode(port, NULL, fValue);
  displayCloseTag(port, "arg");
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
WhileNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "while");
  displayNode(port, "test", fTest);
  displayNode(port, "body", fBody);
  displayCloseTag(port, "while");
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
TypeNode::display(Port<Octet>* port) const
{
  const char* tagName = fIsClass ? "class" : "type";

  StringBuffer attrs;
  attrs << "nm='" << fTypeName << "'";

  displayOpenTagAttrs(port, tagName, StrHelper(attrs.toString()));
  displayNodeList(port, "params", fParams);
  displayNodeList(port, "slots", fSlots);
  displayNodeList(port, "on", fOnExprs);
  displayNodeList(port, "proto", fReqProtocol);
  displayType(port, "isa", fIsa);
  displayCloseTag(port, tagName);
}
