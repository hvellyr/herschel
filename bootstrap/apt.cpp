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
displayNodeList(Port<Octet>* port,
                const char* tagName,
                const NodeList& nodelist)
{
  if (!nodelist.empty())
    displayOpenTag(port, tagName);

  for (NodeList::const_iterator it = nodelist.begin();
       it != nodelist.end();
       it++)
  {
    AptNode* n = (*it);
    if (n != NULL)
      n->display(port);
  }

  if (!nodelist.empty())
    displayCloseTag(port, tagName);
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

IntNode::IntNode(int value)
  : fValue(value)
{
}


void
IntNode::display(Port<Octet>* port) const
{
  displayTag(port, "int", String() + fValue);
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
