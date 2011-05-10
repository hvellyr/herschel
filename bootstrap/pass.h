/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_pass_h
#define bootstrap_pass_h

#include <list>

#include "compiler.h"
#include "ptr.h"
#include "refcountable.h"
#include "scope.h"
#include "str.h"


namespace herschel
{
  class AbstractPass : public RefCountable
  {
  public:
    AbstractPass();
    AbstractPass(Compiler* compiler, Scope* scope);

    String currentModuleName() const;
    void pushModule(const String& name, bool setName);
    void popModule();

    Scope* scope();

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

    Ptr<Scope>        fScope;
    Ptr<Compiler>     fCompiler;
  };
};                              // namespace

#endif                          // bootstrap_pass_h
