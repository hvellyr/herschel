/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_scope_h
#define bootstrap_scope_h

#include "common.h"

#include "ptr.h"
#include "exception.h"
#include "refcountable.h"
#include "srcpos.h"
#include "str.h"
#include "type.h"
#include "parsertypes.h"

#include <map>
#include <memory>
#include <set>


namespace herschel
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

  class Scope
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
    Scope(ScopeLevel level, std::shared_ptr<Scope> parent);

    std::shared_ptr<Scope> parent() const;
    ScopeLevel scopeLevel() const;
    static const char* scopeLevelName(ScopeLevel level);
    const char* scopeLevelName() const;


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
    void addImportedScope(const String& absPath, std::shared_ptr<Scope> scope);


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

    //! Try to normalize \p type.  If it is an alias lookup the refered type.
    Type normalizeType(const Type& type);

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
    const std::set<String>& attachedExportSymbols(const ScopeName& sym) const;

    void registerSymbolForExport(ScopeDomain domain, const String& sym,
                                 VizType viz, bool asFinal);
    void attachSymbolForExport(ScopeDomain domain, const String& sym,
                               const String& attachedSym);

    void exportSymbols(std::shared_ptr<Scope> dstScope, bool propagateOuter) const;
    void propagateImportedScopes(std::shared_ptr<Scope> dstScope) const;


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

    void exportAllSymbols(std::shared_ptr<Scope> dstScope, bool propagateOuter) const;
    void exportAttachedSymbols(std::shared_ptr<Scope> dstScope,
                               const ScopeName& fullKey, VizType vizType,
                               bool isFinal) const;


    //-------- data members

    typedef std::map<String, Ptr<ScopeItem> > BaseScopeMap;
    typedef std::map<ScopeName, BaseScopeMap> NsScopeMap;
    using ImportedScope = std::map<String, std::shared_ptr<Scope>>;
    typedef std::set<String>                  AttachedSymbols;

    NsScopeMap fMap;
    std::shared_ptr<Scope> fParent;

    struct VisibilityPair
    {
      VizType fViz;
      bool    fIsFinal;
      AttachedSymbols fAttachedSymbols;
    };

    typedef std::map<ScopeName, VisibilityPair> VizMap;
    VizMap fVisibility;

    ImportedScope fImportedScopes;
    ScopeLevel    fLevel;
  };

  inline std::shared_ptr<Scope> makeScope(ScopeLevel level)
  {
    return std::make_shared<Scope>(level);
  }

  inline std::shared_ptr<Scope> makeScope(ScopeLevel level,
                                          std::shared_ptr<Scope> parent)
  {
    return std::make_shared<Scope>(level, std::move(parent));
  }


  //--------------------------------------------------------------------------

  class ScopeHelper
  {
  public:
    ScopeHelper(std::shared_ptr<Scope>& scope,
                bool doExport,
                bool isInnerScope,
                ScopeLevel level)
      : fScopeLoc(scope),
        fPrevScope(scope),
        fDoExport(doExport),
        fIsInnerScope(isInnerScope)
    {
      fScopeLoc = makeScope(level, fScopeLoc);
    }

    ~ScopeHelper()
    {
      unrollScopes(fScopeLoc, fPrevScope, fDoExport, fIsInnerScope);
    }


    static void unrollScopes(std::shared_ptr<Scope>& scopeLoc,
                             std::shared_ptr<Scope> prevScope,
                             bool doExport, bool isInnerScope)
    {
      std::shared_ptr<Scope> scope = scopeLoc;
      while (scope && scope != prevScope) {
        auto parent = scope->parent();
        // printf("Export from %p to %p\n", scope, parent);

        if (parent && doExport) {
          scope->exportSymbols(parent, isInnerScope);

          if (!isInnerScope && parent == prevScope) {
            // fprintf(stderr, "propagate imported scopes\n");
            scope->propagateImportedScopes(parent);
          }
        }
        scope = parent;
      }

      hr_assert(scope == prevScope);
      scopeLoc = scope;
    }

  private:
    std::shared_ptr<Scope>& fScopeLoc;
    std::shared_ptr<Scope> fPrevScope;
    bool fDoExport;
    bool fIsInnerScope;
  };
};                              // namespace

#endif                          // bootstrap_scope_h
