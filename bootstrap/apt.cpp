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


static void
displayStringList(Port<Octet>* port,
                  const char* outerTagName, const char* tagName,
                  const StringList& strlist)
{
  if (!strlist.empty())
    displayOpenTag(port, outerTagName);

  for (StringList::const_iterator it = strlist.begin();
       it != strlist.end();
       it++)
  {
    String str = (*it);
    displayOpenTag(port, tagName);
    display(port, str);
    displayCloseTag(port, tagName);
  }

  if (!strlist.empty())
    displayCloseTag(port, outerTagName);
}


static void
displayStringStringMap(Port<Octet>* port,
                       const char* outerTagName, const char* tagName,
                       const char* firstPairTagName, const char* secPairTagName,
                       const StringStringMap& strMap)
{
  if (!strMap.empty())
    displayOpenTag(port, outerTagName);

  for (StringStringMap::const_iterator it = strMap.begin();
       it != strMap.end();
       it++)
  {
    displayOpenTag(port, tagName);
    displayTag(port, firstPairTagName, it->first);
    displayTag(port, secPairTagName, it->second);
    displayCloseTag(port, tagName);
  }

  if (!strMap.empty())
    displayCloseTag(port, outerTagName);
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


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const SrcPos& srcpos, const String& value)
  : AptNode(srcpos),
    fValue(value)
{
}


void
SymbolNode::display(Port<Octet>* port) const
{
  displayTag(port, "symbol", fValue);
}


//----------------------------------------------------------------------------

IntNode::IntNode(const SrcPos& srcpos, int value, bool isImaginary)
  : NumberNode<int>(srcpos, value, isImaginary)
{
}


void
IntNode::display(Port<Octet>* port) const
{
  if (fIsImaginary)
    displayTagAttr(port, "int", "imag='true'", String() + fValue);
  else
    displayTag(port, "int", String() + fValue);
}


//----------------------------------------------------------------------------

RealNode::RealNode(const SrcPos& srcpos, double value, bool isImaginary)
  : NumberNode<double>(srcpos, value, isImaginary)
{
}


void
RealNode::display(Port<Octet>* port) const
{
  if (fIsImaginary)
    displayTagAttr(port, "real", "imag='true'", String() + fValue);
  else
    displayTag(port, "real", String() + fValue);
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const SrcPos& srcpos,
                           const Rational& value, bool isImaginary)
  : NumberNode<Rational>(srcpos, value, isImaginary)
{
}


void
RationalNode::display(Port<Octet>* port) const
{
  String val = String() + fValue.numerator() + "/" + fValue.denominator();
  if (fIsImaginary)
    displayTagAttr(port, "rational", "imag='true'", val);
  else
    displayTag(port, "rational", val);
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


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode(const SrcPos& srcpos)
  : AptNode(srcpos)
{}


void
CompileUnitNode::display(Port<Octet>* port) const
{
  displayNodeList(port, "compile-unit", fChildren);
}


//----------------------------------------------------------------------------

ModuleNode::ModuleNode(const SrcPos& srcpos,
                       const String& modName, const String& publicId)
  : AptNode(srcpos),
    fModName(modName),
    fPublicId(publicId)
{
}


void
ModuleNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "module");

  displayTag(port, "mod-name", fModName);
  displayTag(port, "public-id", fPublicId);

  displayNodeList(port, "defines", fChildren);

  displayCloseTag(port, "module");
}


//----------------------------------------------------------------------------

ExportNode::ExportNode(const SrcPos& srcpos,
                       VizType viz,
                       bool isFinal,
                       const StringList& symbols)
  : AptNode(srcpos),
    fViz(viz),
    fIsFinal(isFinal)
{
  fSymbols.assign(symbols.begin(), symbols.end());
}


const char* ExportNode::vizAttr(VizType viz) const
{
  switch (viz) {
  case kPrivate:
    return "";
  case kInner:
    return "viz='inner'";
  case kOuter:
    return "viz='outer'";
  case kPublic:
    return "viz='public'";
  }
  return "";
}


void
ExportNode::display(Port<Octet>* port) const
{
  StringBuffer attrs;
  attrs << vizAttr(fViz);
  if (fIsFinal)
    attrs << " final='true'";

  displayOpenTagAttrs(port, "export", StrHelper(attrs.toString()));

  displayStringList(port, "symbols", "sym", fSymbols);
  displayCloseTag(port, "export");
}


//----------------------------------------------------------------------------

ImportNode::ImportNode(const SrcPos& srcpos,
                       const String& codeFile,
                       const StringStringMap& renames)
  : AptNode(srcpos),
    fCodeFile(codeFile)
{
  fRenames.insert(renames.begin(), renames.end());
}


void
ImportNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "import");
  displayTag(port, "file", fCodeFile);
  displayStringStringMap(port, "renames", "rename", "from", "to", fRenames);
  displayCloseTag(port, "import");
}


//----------------------------------------------------------------------------

BaseDefNode::BaseDefNode(const SrcPos& srcpos, AptNode* defined)
  : AptNode(srcpos),
    fDefined(defined)
{ }


LetNode::LetNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


void
LetNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "let");
  displayNode(port, NULL, fDefined);
  displayCloseTag(port, "let");
}


DefNode::DefNode(AptNode* node)
  : BaseDefNode(node->srcpos(), node)
{ }


void
DefNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "def");
  displayNode(port, NULL, fDefined);
  displayCloseTag(port, "def");
}


//----------------------------------------------------------------------------

BindingNode::BindingNode(const SrcPos& srcpos,
                         const String& symbolName, AptNode* type,
                         AptNode* initExpr)
  : AptNode(srcpos),
    fSymbolName(symbolName),
    fType(type),
    fInitExpr(initExpr)
{ }


//----------------------------------------------------------------------------

VardefNode::VardefNode(const SrcPos& srcpos,
                       const String& symbolName, VardefFlags flags,
                       AptNode* type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fFlags(flags)
{
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

  displayNode(port, "type", fType);
  displayNode(port, "init", fInitExpr);

  displayCloseTag(port, "vardef");
}


//----------------------------------------------------------------------------

ParamNode::ParamNode(const SrcPos& srcpos,
                     const String& keyName,
                     const String& symbolName, ParamFlags flags,
                     AptNode* type, AptNode* initExpr)
  : BindingNode(srcpos, symbolName, type, initExpr),
    fKey(keyName),
    fFlags(flags)
{
  assert(heaImplies(fFlags == kNamedArg, !fKey.isEmpty()));
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

  displayNode(port, "type", fType);
  displayNode(port, "init", fInitExpr);

  displayCloseTag(port, "param");
}


//----------------------------------------------------------------------------

void
ArrayNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "array");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "array");
}


//----------------------------------------------------------------------------

void
VectorNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "vector");
  displayNodeList(port, NULL, fChildren);
  displayCloseTag(port, "vector");
}


//----------------------------------------------------------------------------

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
{ }


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


//------------------------------------------------------------------------------

RangeNode::RangeNode(const SrcPos& srcpos,
                     AptNode* from, AptNode* to, AptNode* by)
  : AptNode(srcpos),
    fFrom(from),
    fTo(to),
    fBy(by)
{ }


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


//------------------------------------------------------------------------------

OnNode::OnNode(const SrcPos& srcpos,
               const String& key, const NodeList& params, AptNode* body)
  : AptNode(srcpos),
    fKey(key),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
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


void
BlockNode::display(Port<Octet>* port) const
{
  displayNodeList(port, "block", fChildren);
}


//----------------------------------------------------------------------------

FunctionNode::FunctionNode(const SrcPos& srcpos,
                           const NodeList& params,
                           AptNode* retType,
                           AptNode* body)
  : AptNode(srcpos),
    fRetType(retType),
    fBody(body)
{
  fParams.assign(params.begin(), params.end());
}


void
FunctionNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "function");
  displayNodeList(port, "params", fParams);
  displayNode(port, "body", fBody);
  displayCloseTag(port, "function");
}


//----------------------------------------------------------------------------

FuncDefNode::FuncDefNode(const SrcPos& srcpos,
                         const String& sym,
                         unsigned int flags,
                         const NodeList& params,
                         AptNode* retType,
                         AptNode* body)
  : FunctionNode(srcpos, params, retType, body),
    fSym(sym),
    fFlags(flags)
{
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


//----------------------------------------------------------------------------

ApplyNode::ApplyNode(const SrcPos& srcpos, AptNode* base)
  : AptNode(srcpos),
    fBase(base)
{ }


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

void
WhileNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "while");
  displayNode(port, "test", fTest);
  displayNode(port, "body", fBody);
  displayCloseTag(port, "while");
}
