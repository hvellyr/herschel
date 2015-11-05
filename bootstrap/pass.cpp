/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include "pass.h"
#include "str.h"
#include "symbol.h"


using namespace herschel;

//----------------------------------------------------------------------------

AbstractPass::AbstractPass(Compiler& compiler, std::shared_ptr<Scope> scope)
  : fScope(std::move(scope)),
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


std::shared_ptr<Scope>
AbstractPass::scope()
{
  return fScope;
}
