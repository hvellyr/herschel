/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_scope_h
#define bootstrap_scope_h

#include "common.h"

#include <map>

#include "ptr.h"
#include "exception.h"
#include "refcountable.h"
#include "srcpos.h"
#include "str.h"
#include "type.h"
#include "parsertypes.h"


namespace heather
{
  class Macro;
  class AptNode;

  
  //--------------------------------------------------------------------------

  class Scope : public RefCountable
  {
  public:
    Scope();
    Scope(Scope* parent);

    Scope* parent() const;


    //! Check whether a given symbol \p name is registered in this scope.  If
    //! so return the \p srcpos where it was defined first.
    bool hasName(const String& name, SrcPos* srcpos) const;

    //! Like hasName() but checks in the current scope only.  If doAutoMatch
    //! is true the function maps a non-qualified \name to a solitary
    //! available definition from any namespace.
    bool hasNameLocal(const String& name, SrcPos* srcpos,
                      bool doAutoMatch) const;


    bool checkForRedefinition(const SrcPos& srcpos,
                              const String& sym) const;

    void dumpDebug() const;

    //-------- types

    void registerType(const SrcPos& srcpos,
                      const String& name, const Type& type);

    const Type& lookupType(const String& name, bool showAmbiguousSymDef) const;

    //! Lookup a type by another type.  If \p type is not a typeref, it is
    //! returned as is.  If the type looked up is parametrized, it is fill by
    //! type parameters as found in \p type.
    Type lookupType(const Type& type) const;

    //! Normalize a (complete) \p type using \p refType are using reference.
    //! I.e. type generics in \p type are set using the args from \p refType.
    //! If \p refType refers to an array type the result will be an array
    //! also.
    //!
    //! Throws an TypeRefMatchException if \p refType's type parameters
    //! does not match the number of generics expected in \p type.
    Type normalizeType(const Type& type, const Type& refType) const;


    //-------- macros

    void registerMacro(const SrcPos& srcpos,
                       const String& name, Macro* macro);
    const Macro* lookupMacro(const SrcPos& srcpos,
                             const String& name, bool showAmbiguousSymDef) const;


    //-------- functions

    void registerFunction(const SrcPos& srcpos,
                          const String& name, AptNode* node);
    const AptNode* lookupFunction(const String& name, bool showAmbiguousSymDef) const;


    //-------- variables

    void registerVar(const SrcPos& srcpos,
                     const String& name, AptNode* macro);
    const AptNode* lookupVar(const String& name, bool showAmbiguousSymDef) const;



    //-------- register export symbols

    bool shouldExportSymbol(const String& sym) const;
    VizType exportSymbolVisibility(const String& sym) const;
    bool exportSymbolIsFinal(const String& sym) const;
    void registerSymbolForExport(const String& sym, VizType viz, bool asFinal);

    void exportSymbols(Scope* dstScope);

    //-------- global defs

    enum ScopeItemKind
    {
      kScopeItem_type,
      kScopeItem_function,
      kScopeItem_variable,
      kScopeItem_macro
    };

    class ScopeItem : public RefCountable
    {
    public:
      ScopeItem(const SrcPos& srcpos)
        : fSrcPos(srcpos)
      { }


      const SrcPos& srcpos() const
      {
        return fSrcPos;
      }


      virtual ScopeItemKind kind() const = 0;

    protected:
      SrcPos fSrcPos;
    };

  private:
    void registerScopeItem(const String& name, ScopeItem* item);
    const ScopeItem* lookupItemLocal(const SrcPos& srcpos,
                                     const String& name, bool showError) const;
    const ScopeItem* lookupItem(const SrcPos& srcpos,
                                const String& name, bool showError) const;

    VizType reduceVizType(VizType in);

    const Scope::ScopeItem* lookupItemLocalImpl(const SrcPos& srcpos,
                                                const String& name,
                                                bool showError,
                                                bool doAutoMatch) const;

    //-------- data members

    typedef std::map<String, std::map<String, Ptr<ScopeItem> > > ScopeMap;
    
    ScopeMap   fMap;
    Ptr<Scope> fParent;

    struct VisibilityPair
    {
      VizType fViz;
      bool    fIsFinal;
    };

    typedef std::map<String, VisibilityPair> VizMap;
    VizMap fVisibility;
  };


  //--------------------------------------------------------------------------

  class ScopeHelper
  {
  public:
    ScopeHelper(Ptr<Scope>& scope)
      : fScopeLoc(scope),
        fPrevScope(scope)
    {
      fScopeLoc = new Scope(fScopeLoc);
    }

    ~ScopeHelper()
    {
      Scope* scope = fScopeLoc;
      while (scope != NULL && scope != fPrevScope) {
        Scope* parent = scope->parent();
        if (parent != NULL)
          scope->exportSymbols(parent);
        scope = parent;
      }
      assert(scope == fPrevScope);
      fScopeLoc = scope;
    }

  private:
    Ptr<Scope>& fScopeLoc;
    Ptr<Scope> fPrevScope;
  };
};                              // namespace

#endif                          // bootstrap_scope_h
