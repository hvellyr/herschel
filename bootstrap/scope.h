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

  enum ScopeLevel
  {
    kScopeL_CompileUnit,
    kScopeL_Module,
    kScopeL_Function,
    kScopeL_Local,
  };

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


    Scope(ScopeLevel level);
    Scope(ScopeLevel level, Scope* parent);

    Scope* parent() const;
    ScopeLevel scopeLevel() const;


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

    void dumpDebug(bool recursive = false) const;

    bool hasScopeForFileLocal(const String& absPath) const;
    bool hasScopeForFile(const String& absPath) const;
    void addImportedScope(const String& absPath, Scope* scope);


    //-------- types

    void registerType(const SrcPos& srcpos,
                      const String& name, const Type& type);

    const Type& lookupType(const String& name, bool showAmbiguousSymDef) const;

    Type lookupType(const Type& type) const;

    //! Lookup a type by another type.  If \p type is not a typeref, it is
    //! returned as is.  If the type looked up is parametrized, it is fill by
    //! type parameters as found in \p type.
    Type lookupType_unused(const Type& type) const;

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

    const AptNode* lookupVarOrFunc(const String& name,
                                   bool showAmbiguousSymDef) const;

    bool isVarInOuterFunction(const String& name) const;

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
    struct LookupResult
    {
      LookupResult()
        : fItem(NULL),
          fInOuterFunc(false)
      { }

      LookupResult(const ScopeItem* item, bool inOuterFunc)
        : fItem(item),
          fInOuterFunc(inOuterFunc)
      { }

      LookupResult(const LookupResult& other)
        : fItem(other.fItem),
          fInOuterFunc(other.fInOuterFunc)
      { }

      LookupResult& operator=(const LookupResult& other)
      {
        fItem = other.fItem;
        fInOuterFunc = other.fInOuterFunc;
        return *this;
      }

      const ScopeItem* fItem;
      bool             fInOuterFunc;
    };

    LookupResult lookupItemLocal(const SrcPos& srcpos,
                                 const ScopeName& name,
                                 bool showError) const;
    LookupResult lookupItem(const SrcPos& srcpos,
                            const ScopeName& name,
                            bool showError) const;

    VizType reduceVizType(VizType in) const;

    LookupResult lookupItemLocalImpl(const SrcPos& srcpos,
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
    ScopeLevel    fLevel;
  };


  //--------------------------------------------------------------------------

  class ScopeHelper
  {
  public:
    ScopeHelper(Ptr<Scope>& scope,
                bool doExport,
                bool isInnerScope,
                ScopeLevel level)
      : fScopeLoc(scope),
        fPrevScope(scope),
        fDoExport(doExport),
        fIsInnerScope(isInnerScope)
    {
      fScopeLoc = new Scope(level, fScopeLoc);
    }

    ~ScopeHelper()
    {
      unrollScopes(fScopeLoc, fPrevScope, fDoExport, fIsInnerScope);
    }


    static void unrollScopes(Ptr<Scope>& scopeLoc, Scope* prevScope,
                             bool doExport, bool isInnerScope)
    {
      Scope* scope = scopeLoc;
      while (scope != NULL && scope != prevScope) {
        Scope* parent = scope->parent();
        // printf("Export from %p to %p\n", scope, parent);

        if (parent != NULL && doExport) {
          scope->exportSymbols(parent, isInnerScope);

          if (!isInnerScope && parent == prevScope) {
            // fprintf(stderr, "propagate imported scopes\n");
            scope->propagateImportedScopes(parent);
          }
        }
        scope = parent;
      }

      assert(scope == prevScope);
      scopeLoc = scope;
    }

  private:
    Ptr<Scope>& fScopeLoc;
    Ptr<Scope> fPrevScope;
    bool fDoExport;
    bool fIsInnerScope;
  };
};                              // namespace

#endif                          // bootstrap_scope_h
