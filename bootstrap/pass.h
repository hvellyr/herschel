/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <list>

#include "compiler.h"
#include "scope.h"
#include "str.h"


namespace herschel
{
  class AbstractPass
  {
  public:
    AbstractPass(Compiler& compiler, std::shared_ptr<Scope> scope);

    String currentModuleName() const;
    void pushModule(const String& name, bool setName);
    void popModule();

    std::shared_ptr<Scope> scope();

    friend class ModuleHelper;
    class ModuleHelper
    {
    public:
      ModuleHelper(AbstractPass* pass, const String& name,
                   bool setName = false)
        : fPass(pass)
      {
        fPass->pushModule(name, setName);
      }

      ~ModuleHelper()
      {
        fPass->popModule();
      }

      AbstractPass* fPass;
    };

  protected:
    String            fCurrentModuleName;
    std::list<String> fModuleNameStack;

    std::shared_ptr<Scope> fScope;
    Compiler&         fCompiler;
  };

} // namespace
