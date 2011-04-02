/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include "pass.h"
#include "str.h"
#include "symbol.h"


using namespace herschel;

//----------------------------------------------------------------------------

AbstractPass::AbstractPass()
{
}


AbstractPass::AbstractPass(Compiler* compiler, Scope* scope)
  : fScope(scope),
    fCompiler(compiler)
{
}


String
AbstractPass::currentModuleName() const
{
  return fCurrentModuleName;
}


void
AbstractPass::pushModule(const String& name, bool setName)
{
  fModuleNameStack.push_front(fCurrentModuleName);

  fCurrentModuleName = setName ? name : qualifyId(fCurrentModuleName, name);
}


void
AbstractPass::popModule()
{
  fCurrentModuleName = fModuleNameStack.front();
  fModuleNameStack.pop_front();
}


Scope*
AbstractPass::scope()
{
  return fScope;
}
