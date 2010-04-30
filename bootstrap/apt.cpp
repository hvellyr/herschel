/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"

using namespace heather;


//----------------------------------------------------------------------------

void
AptNode::appendNode(AptNode* node)
{
  fChildren.push_back(node);
}


void
AptNode::displayNodeList(Port<Octet>* port,
                         const char* tagName,
                         const NodeList& nodelist) const
{
  if (tagName != NULL && !nodelist.empty())
    heather::display(port, String() + "<" + tagName + ">");
  for (NodeList::const_iterator it = nodelist.begin();
       it != nodelist.end();
       it++)
  {
    AptNode* n = (*it);
    if (n != NULL)
      n->display(port);
  }

  if (tagName != NULL && !nodelist.empty())
    heather::display(port, String() + "</" + tagName + ">");
}


//----------------------------------------------------------------------------

StringNode::StringNode(const String& value)
  : fValue(value)
{
}


void
StringNode::display(Port<Octet>* port) const
{
  heather::display(port, "<str>");
  heather::display(port, fValue);
  heather::display(port, "</str>");
}


//----------------------------------------------------------------------------

IntNode::IntNode(int value)
  : fValue(value)
{
}


void
IntNode::display(Port<Octet>* port) const
{
  heather::display(port, "<int value='");
  heather::display(port, String() + fValue);
  heather::display(port, "'/>");
}


//----------------------------------------------------------------------------

CompileUnitNode::CompileUnitNode()
{}


void
CompileUnitNode::display(Port<Octet>* port) const
{
  heather::display(port, "<compile-unit>");
  displayNodeList(port, NULL, fChildren);
  heather::display(port, "</compile-unit>");
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

  heather::display(port, String() + "<" + tagName + ">");
  heather::display(port, "<mod-name>");
  heather::display(port, fModName);
  heather::display(port, "</mod-name>");
  heather::display(port, "<public-id>");
  heather::display(port, fPublicId);
  heather::display(port, "</public-id>");

  displayNodeList(port, "defines", fChildren);

  heather::display(port, String() + "</" + tagName + ">");
}
