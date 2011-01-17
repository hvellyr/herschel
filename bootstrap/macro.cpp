/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "refcountable.h"
#include "ptr.h"
#include "token.h"
#include "macro.h"
#include "strbuf.h"


using namespace herschel;


//------------------------------------------------------------------------------

SyntaxTreeNode::SyntaxTreeNode()
  : fHasEndPattern(false)
{ }


SyntaxTreeNode*
SyntaxTreeNode::findNode(const Token& token) const
{
  if (token == kMacroParam ||
      token == kMacroParamAsStr) {
    for (NodeMap::const_iterator it = fNodes.begin();
         it != fNodes.end();
         it++)
    {
      if (it->first == kMacroParam ||
          it->first == kMacroParamAsStr)
        return it->second.obj();
      else if (it->first == token)
        return it->second.obj();
    }
  }
  else {
    NodeMap::const_iterator it = fNodes.find(token);
    if (it != fNodes.end())
      return it->second.obj();
  }
  return NULL;
}


SyntaxTreeNode*
SyntaxTreeNode::findMacroParam(Token* macroParam) const
{
  for (NodeMap::const_iterator it = fNodes.begin();
       it != fNodes.end();
       it++)
  {
    if (it->first == kMacroParam ||
        it->first == kMacroParamAsStr) {
      *macroParam = it->first;
      return it->second.obj();
    }
  }

  return NULL;
}


void
SyntaxTreeNode::setNode(const Token& token, SyntaxTreeNode* node)
{
  NodeMap::iterator it = fNodes.find(token);
  if (it != fNodes.end())
    it->second = node;
  else
    fNodes.insert(std::make_pair(token, node));
}


void
SyntaxTreeNode::setEndNode(const TokenVector& replacement)
{
  fHasEndPattern = true;
  fEndReplacement = replacement;
}


bool
SyntaxTreeNode::hasEndSet() const
{
  return fHasEndPattern;
}


const TokenVector&
SyntaxTreeNode::replacement() const
{
  assert(fHasEndPattern);
  return fEndReplacement;
}


String
SyntaxTreeNode::toString() const
{
  StringBuffer buf;

  buf << "<st:node>";
  for (NodeMap::const_iterator it = fNodes.begin();
       it != fNodes.end();
       it++)
  {
    buf << "<st:map><st:tok>" << it->first.toString()
        << "</st:tok>" << it->second->toString()
        << "</st:map>";
  }

  if (hasEndSet()) {
    buf << "<st:end>";
    for (TokenVector::const_iterator it = fEndReplacement.begin();
         it != fEndReplacement.end();
         it++)
    {
      buf << "<st:tok>" << it->toString() << "</st:tok>";
    }
    buf << "</st:end>";
  }
  buf << "</st:node>";

  return buf.toString();
}


//------------------------------------------------------------------------------

SyntaxTreeNode*
SyntaxTable::rootNode() const
{
  return findPattern(String(""));
}


SyntaxTreeNode*
SyntaxTable::findPattern(const String& name) const
{
  PatternMap::const_iterator it = fItems.find(name);
  if (it != fItems.end())
    return it->second.obj();
  return NULL;
}

void
SyntaxTable::setPattern(const String& name, SyntaxTreeNode* node)
{
  PatternMap::iterator it = fItems.find(name);
  if (it != fItems.end())
    it->second = node;
  else
    fItems.insert(std::make_pair(name, node));
}


void
SyntaxTable::mixinPatternPart(SyntaxTreeNode* patternTree,
                              const TokenVector& pattern,
                              const TokenVector& rplcmnt)
{
  Ptr<SyntaxTreeNode> node = patternTree;
  for (TokenVector::const_iterator srcit = pattern.begin();
       srcit != pattern.end();
       srcit++)
  {
    Token patternToken = *srcit;
    Ptr<SyntaxTreeNode> step = node->findNode(patternToken);
    if (step == NULL) {
      step = new SyntaxTreeNode();
      node->setNode(patternToken, step);
    }
    node = step;
  }

  assert(node != NULL);
  node->setEndNode(rplcmnt);
}


void
SyntaxTable::mixinPattern(const String& macroName,
                          const TokenVector& pattern,
                          const TokenVector& rplcmnt)
{
  SyntaxTreeNode* patternTree = findPattern(macroName);
  if (patternTree == NULL) {
    patternTree = new SyntaxTreeNode();
    setPattern(macroName, patternTree);
  }

  mixinPatternPart(patternTree, pattern, rplcmnt);
}


SyntaxTable*
SyntaxTable::compile(const String& patternName,
                     const MacroPatternVector& patterns)
{
  Ptr<SyntaxTable> st = new SyntaxTable();

  for (MacroPatternVector::const_iterator it = patterns.begin();
       it != patterns.end();
       it++)
  {
    TokenVector pattern = it->fPattern;
    TokenVector rplcmnt = it->fReplacement;

    st->mixinPattern(patternName, pattern, rplcmnt);
  }

  return st.release();
}


String
SyntaxTable::toString() const
{
  StringBuffer buf;

  buf << "<st:table xmlns:st='http://herschel.eyestep.org/syntax-table'>";
  for (PatternMap::const_iterator it = fItems.begin();
       it != fItems.end();
       it++)
  {
    buf << "<st:pattern><st:name>" << it->first << "</st:name>"
        << it->second->toString()
        << "</st:pattern>";
  }
  buf << "</st:table>";

  return buf.toString();
}



//------------------------------------------------------------------------------

String
herschel::toString(MacroType type)
{
  switch (type) {
  case kMacro_Invalid:  return String("--invalid--");
  case kMacro_Any:      return String("Any");
  case kMacro_Def:      return String("Def");
  case kMacro_On:       return String("On");
  case kMacro_Stmt:     return String("Stmt");
  case kMacro_Function: return String("Function");
  }

  return String("???");
}


//------------------------------------------------------------------------------

MacroParamType
herschel::macroParamType(const Token& token, String* paramName)
{
  assert(token == kMacroParam ||
         token == kMacroParamAsStr);

  String str = token.idValue();
  String name;
  String type;

  if (str.split(':', name, type) >= 0) {
    *paramName = name;
    if (type == String("expr"))
      return kMacro_expr;
    else if (type == String("name"))
      return kMacro_name;
    else if (type == String("param"))
      return kMacro_param;
    else if (type == String("pos-param"))
      return kMacro_posParam;
    else if (type == String("named-param"))
      return kMacro_namedParam;
    else if (type == String("rest-param"))
      return kMacro_restParam;
    else if (type == String("paramlist"))
      return kMacro_paramlist;
    else if (type == String("body"))
      return kMacro_body;
  }
  return kMacro_unknown;
}
