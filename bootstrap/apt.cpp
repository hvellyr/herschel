/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"

using namespace heather;


static void
displayOpenTag(Port<Octet>* port, const char* tagName)
{
  if (tagName != NULL)
    heather::display(port, String() + "<" + tagName + ">");
}


static void
displayCloseTag(Port<Octet>* port, const char* tagName)
{
  if (tagName != NULL)
    heather::display(port, String() + "</" + tagName + ">");
}


static void
displayTag(Port<Octet>* port, const char* tagName, const String& value)
{
  displayOpenTag(port, tagName);
  display(port, value);
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


//----------------------------------------------------------------------------

StringNode::StringNode(const String& value)
  : fValue(value)
{
}


void
StringNode::display(Port<Octet>* port) const
{
  displayTag(port, "str", fValue);
}


//----------------------------------------------------------------------------

KeywordNode::KeywordNode(const String& value)
  :fValue(value)
{
}


void
KeywordNode::display(Port<Octet>* port) const
{
  displayTag(port, "keyw", fValue);
}


//----------------------------------------------------------------------------

SymbolNode::SymbolNode(const String& value)
  :fValue(value)
{
}


void
SymbolNode::display(Port<Octet>* port) const
{
  displayTag(port, "symbol", fValue);
}


//----------------------------------------------------------------------------

IntNode::IntNode(int value)
  : NumberNode<int>(value)
{
}


void
IntNode::display(Port<Octet>* port) const
{
  displayTag(port, "int", String() + fValue);
}


//----------------------------------------------------------------------------

RealNode::RealNode(double value)
  : NumberNode<double>(value)
{
}


void
RealNode::display(Port<Octet>* port) const
{
  displayTag(port, "real", String() + fValue);
}


//----------------------------------------------------------------------------

RationalNode::RationalNode(const Rational& value)
  : NumberNode<Rational>(value)
{
}


void
RationalNode::display(Port<Octet>* port) const
{
  displayTag(port, "rational", ( String() + fValue.numerator()
                                 + "/" + fValue.denominator()) );
}


//----------------------------------------------------------------------------

CharNode::CharNode(Char value)
  : fValue(value)
{ }


void
CharNode::display(Port<Octet>* port) const
{
  displayTag(port, "char", fromInt(int(fValue)));
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode()
{}


void
CompileUnitNode::display(Port<Octet>* port) const
{
  displayNodeList(port, "compile-unit", fChildren);
}


//----------------------------------------------------------------------------

ModuleNode::ModuleNode(const String& modName, const String& publicId,
                       bool isModule)
  : fIsModule(isModule),
    fModName(modName),
    fPublicId(publicId)
{
}


void
ModuleNode::display(Port<Octet>* port) const
{
  const char* tagName = fIsModule ? "module" : "interface";

  displayOpenTag(port, tagName);

  displayTag(port, "mod-name", fModName);
  displayTag(port, "public-id", fPublicId);

  displayNodeList(port, "defines", fChildren);

  displayCloseTag(port, tagName);
}


//----------------------------------------------------------------------------

ExportNode::ExportNode(const std::list<String>& flags,
                       const std::list<String>& symbols)
{
  fFlags.assign(flags.begin(), flags.end());
  fSymbols.assign(symbols.begin(), symbols.end());
}


void
ExportNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "export");
  displayStringList(port, "flags", "flag", fFlags);
  displayStringList(port, "symbols", "sym", fSymbols);
  displayCloseTag(port, "export");
}


//----------------------------------------------------------------------------

ImportNode::ImportNode(const String& codeFile,
                       const StringStringMap& renames)
  : fCodeFile(codeFile)
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

VardefNode::VardefNode(const String& symbolName, VardefFlags flags,
                       AptNode* type, AptNode* initExpr)
  : fSymbolName(symbolName),
    fFlags(flags),
    fType(type),
    fInitExpr(initExpr)
{
}


void
VardefNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "vardef");

  switch (fFlags) {
  case kNoFlags:
    break;
  case kIsFluid:
    heather::display(port, "<is-fluid/>");
    break;
  case kIsConst:
    heather::display(port, "<is-const/>");
    break;
  }

  displayTag(port, "sym", fSymbolName);

  displayNode(port, "type", fType);
  displayNode(port, "init", fInitExpr);

  displayCloseTag(port, "vardef");
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


//----------------------------------------------------------------------------

BinaryNode::BinaryNode(AptNode* left, BinOperatorType op, AptNode* right)
  : fLeft(left),
    fRight(right),
    fOp(op)
{ }


BinOperatorType
BinaryNode::op() const
{
  return fOp;
}


AptNode*
BinaryNode::left() const
{
  return fLeft;
}


AptNode*
BinaryNode::right() const
{
  return fRight;
}


bool
BinaryNode::isMapTo() const
{
  return fOp == kOpMapTo;
}


static const char*
operatorName(BinOperatorType type)
{
  switch (type) {
  case kOpPlus:         return "+";
  case kOpMinus:        return "-";
  case kOpDivide:       return "/";
  case kOpMultiply:     return "*";
  case kOpExponent:     return "**";
  case kOpFold:         return "%";
  case kOpCompare:      return "<=>";
  case kOpEqual:        return "==";
  case kOpUnequal:      return "<>";
  case kOpLess:         return "<";
  case kOpLessEqual:    return "<=";
  case kOpGreater:      return ">";
  case kOpGreaterEqual: return ">=";
  case kOpMapTo:        return "->";
  case kOpIn:           return "in";
  case kOpMod:          return "mod";
  case kOpIsa:          return "isa";
  case kOpAs:           return "as";
  case kOpLogicalAnd:   return "and";
  case kOpLogicalOr:    return "or";
  case kOpBitAnd:       return "AND";
  case kOpBitOr:        return "OR";
  case kOpBitXor:       return "XOR";
  case kOpShiftLeft:    return "<<";
  case kOpShiftRight:   return ">>";
  }

  return NULL;
}


void
BinaryNode::display(Port<Octet>* port) const
{
  displayOpenTag(port, "binary");
  displayNode(port, NULL, fLeft);
  heather::display(port, operatorName(fOp));
  displayNode(port, NULL, fRight);
  displayCloseTag(port, "binary");
}
