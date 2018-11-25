/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "abstrpass.hpp"

#include "str.hpp"
#include "symbol.hpp"


namespace herschel {

AbstractPass::AbstractPass(Compiler& compiler, std::shared_ptr<Scope> scope)
    : fScope(std::move(scope))
    , fCompiler(compiler)
{
}


String AbstractPass::currentModuleName() const
{
  return fCurrentModuleName;
}


void AbstractPass::pushModule(const String& name, bool setName)
{
  fModuleNameStack.push_front(fCurrentModuleName);

  fCurrentModuleName = setName ? name : qualifyId(fCurrentModuleName, name);
}


void AbstractPass::popModule()
{
  fCurrentModuleName = fModuleNameStack.front();
  fModuleNameStack.pop_front();
}


std::shared_ptr<Scope> AbstractPass::scope()
{
  return fScope;
}

}  // namespace herschel
