/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include "pass.h"
#include "str.h"
#include "symbol.h"


using namespace heather;

//----------------------------------------------------------------------------

AbstractPass::AbstractPass()
{
}


AbstractPass::AbstractPass(Parser* parser, Scope* scope)
  : fScope(scope),
    fParser(parser)
{
}


String
AbstractPass::currentModuleName() const
{
  return fCurrentModuleName;
}


void
AbstractPass::pushModule(const String& name)
{
  fModuleNameStack.push_front(fCurrentModuleName);

  fCurrentModuleName = qualifyId(fCurrentModuleName, name);
}


void
AbstractPass::popModule()
{
  fCurrentModuleName = fModuleNameStack.front();
  fModuleNameStack.pop_front();
}


