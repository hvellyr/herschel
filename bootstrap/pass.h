/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass_h
#define bootstrap_pass_h

#include <list>

#include "parser.h"
#include "ptr.h"
#include "refcountable.h"
#include "scope.h"
#include "str.h"


namespace heather
{
  class AbstractPass : public RefCountable
  {
  public:
    AbstractPass();
    AbstractPass(Parser* parser, Scope* scope);

    String currentModuleName() const;
    void pushModule(const String& name);
    void popModule();


    friend class ModuleHelper;
    class ModuleHelper
    {
    public:
      ModuleHelper(AbstractPass* pass, const String& name)
        : fPass(pass)
      {
        fPass->pushModule(name);
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
    Ptr<Parser>       fParser;
  };
};                              // namespace

#endif                          // bootstrap_pass_h
