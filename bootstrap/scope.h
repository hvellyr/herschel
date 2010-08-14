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
    enum ScopeDomain
    {
      kNormal,
      kChar,
      kUnit,

      kMaxScopeDomain
    };

    struct ScopeName
    {
      ScopeName()
        : fDomain(kNormal)
      { }

      ScopeName(ScopeDomain domain, const String& name)
        : fDomain(domain),
          fName(name)
      { }

      ScopeName(const ScopeName& other)
        : fDomain(other.fDomain),
          fName(other.fName)
      { }

      ScopeName& operator=(const ScopeName& other)
      {
        fDomain = other.fDomain;
        fName = other.fName;
        return *this;
      }

      bool operator==(const ScopeName& other) const
      {
        return ( fDomain == other.fDomain &&
                 fName == other.fName );
      }

      bool operator<(const ScopeName& other) const
      {
        if (fDomain == other.fDomain)
          return fName < other.fName;
        return fDomain < other.fDomain;
      }


      ScopeDomain fDomain;
      String      fName;
    };


    Scope();
    Scope(Scope* parent);

    Scope* parent() const;


    //! Check whether a given symbol \p name is registered in this scope.  If
    //! so return the \p srcpos where it was defined first.
    bool hasName(ScopeDomain domain, const String& name,
                 SrcPos* srcpos) const;

    //! Like hasName() but checks in the current scope only.  If doAutoMatch
    //! is true the function maps a non-qualified \name to a solitary
    //! available definition from any namespace.
    bool hasNameLocal(ScopeDomain domain, const String& name,
                      SrcPos* srcpos,
                      bool doAutoMatch) const;


    bool checkForRedefinition(const SrcPos& srcpos,
                              ScopeDomain domain,
                              const String& sym) const;

    void dumpDebug() const;

    bool hasScopeForFileLocal(const String& absPath) const;
    bool hasScopeForFile(const String& absPath) const;
    void addImportedScope(const String& absPath, Scope* scope);


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

    //! register a unit \p unitName defined in terms of \p baseUnit, refering
    //! to the type \p baseType.  The unit \p unitName can be computed into \p
    //! baseUnit by \p transformFunc.  If \p transformFunc is NULL this is a
    //! base unit.
    void registerUnit(const SrcPos& srcpos,
                      const String& unitName, const String& baseUnit,
                      const Type& baseType,
                      AptNode* transformFunc);

    TypeUnit lookupUnit(const String& name, bool showAmbiguousSymDef) const;

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

    bool shouldExportSymbol(const ScopeName& sym) const;
    VizType exportSymbolVisibility(const ScopeName& sym) const;
    bool exportSymbolIsFinal(const ScopeName& sym) const;
    void registerSymbolForExport(ScopeDomain domain, const String& sym,
                                 VizType viz, bool asFinal);

    void exportSymbols(Scope* dstScope, bool propagateOuter) const;
    void propagateImportedScopes(Scope* dstScope) const;


    //-------- global defs

    enum ScopeItemKind
    {
      kScopeItem_type,
      kScopeItem_function,
      kScopeItem_variable,
      kScopeItem_macro,
      kScopeItem_unit
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
    void registerScopeItem(const ScopeName& name, ScopeItem* item);
    const ScopeItem* lookupItemLocal(const SrcPos& srcpos,
                                     const ScopeName& name,
                                     bool showError) const;
    const ScopeItem* lookupItem(const SrcPos& srcpos,
                                const ScopeName& name,
                                bool showError) const;

    VizType reduceVizType(VizType in) const;

    const Scope::ScopeItem* lookupItemLocalImpl(const SrcPos& srcpos,
                                                const ScopeName& name,
                                                bool showError,
                                                bool doAutoMatch) const;

    void dumpDebugImpl() const;

    void exportAllSymbols(Scope* dstScope, bool propagateOuter) const;

    //-------- data members

    typedef std::map<String, Ptr<ScopeItem> > BaseScopeMap;
    typedef std::map<ScopeName, BaseScopeMap> NsScopeMap;
    typedef std::map<String, Ptr<Scope> > ImportedScope;

    NsScopeMap fMap;
    Ptr<Scope> fParent;

    struct VisibilityPair
    {
      VizType fViz;
      bool    fIsFinal;
    };

    typedef std::map<ScopeName, VisibilityPair> VizMap;
    VizMap fVisibility;

    ImportedScope fImportedScopes;
  };


  //--------------------------------------------------------------------------

  class ScopeHelper
  {
  public:
    ScopeHelper(Ptr<Scope>& scope,
                bool doExport,
                bool isInnerScope)
      : fScopeLoc(scope),
        fPrevScope(scope),
        fDoExport(doExport),
        fIsInnerScope(isInnerScope)
    {
      fScopeLoc = new Scope(fScopeLoc);
    }

    ~ScopeHelper()
    {
      Scope* scope = fScopeLoc;
      while (scope != NULL && scope != fPrevScope) {
        Scope* parent = scope->parent();
        // printf("Export from %p to %p\n", scope, parent);

        if (parent != NULL && fDoExport) {
          scope->exportSymbols(parent, fIsInnerScope);

          if (!fIsInnerScope && parent == fPrevScope) {
            // fprintf(stderr, "propagate imported scopes\n");
            scope->propagateImportedScopes(parent);
          }
        }
        scope = parent;
      }
      assert(scope == fPrevScope);
      fScopeLoc = scope;
    }

  private:
    Ptr<Scope>& fScopeLoc;
    Ptr<Scope> fPrevScope;
    bool fDoExport;
    bool fIsInnerScope;
  };
};                              // namespace

#endif                          // bootstrap_scope_h
