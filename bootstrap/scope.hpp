/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "exception.hpp"
#include "log.hpp"
#include "parsertypes.hpp"
#include "srcpos.hpp"
#include "str.hpp"
#include "type.hpp"

#include <map>
#include <memory>
#include <set>
#include <tuple>


namespace herschel {
class Macro;
class AstNode;
class FunctionNode;

using NodeList = std::vector<std::shared_ptr<AstNode>>;

enum ScopeLevel {
  kScopeL_CompileUnit,
  kScopeL_Library,
  kScopeL_Module,
  kScopeL_Function,
  kScopeL_Local,
};

class Scope {
public:
  enum ScopeDomain {
    kNormal,
    kChar,

    kMaxScopeDomain
  };

  struct ScopeName {
    ScopeName()
        : fDomain(kNormal)
    {
    }

    ScopeName(ScopeDomain domain, const String& name)
        : fDomain(domain)
        , fName(name)
    {
    }

    ScopeName(const ScopeName& other)
        : fDomain(other.fDomain)
        , fName(other.fName)
    {
    }

    ScopeName& operator=(const ScopeName& other)
    {
      fDomain = other.fDomain;
      fName = other.fName;
      return *this;
    }

    bool operator==(const ScopeName& other) const
    {
      return (fDomain == other.fDomain && fName == other.fName);
    }

    bool operator<(const ScopeName& other) const
    {
      if (fDomain == other.fDomain)
        return fName < other.fName;
      return fDomain < other.fDomain;
    }


    ScopeDomain fDomain;
    String fName;
  };


  Scope(ScopeLevel level);
  Scope(ScopeLevel level, std::shared_ptr<Scope> parent);

  std::shared_ptr<Scope> parent() const;
  ScopeLevel scopeLevel() const;
  static zstring scopeLevelName(ScopeLevel level);
  zstring scopeLevelName() const;


  //! Check whether a given symbol \p name is registered in this scope.  If
  //! so return the \p srcpos where it was defined first.
  bool hasName(ScopeDomain domain, const String& name, SrcPos* srcpos) const;

  //! Like hasName() but checks in the current scope only.  If doAutoMatch
  //! is true the function maps a non-qualified \name to a solitary
  //! available definition from any namespace.
  bool hasNameLocal(ScopeDomain domain, const String& name, SrcPos* srcpos,
                    bool doAutoMatch) const;

  //! Like hasName() but checks in the current scope only and handles
  //! function overloading.  If @p doAutoMatch is true the function
  //! maps a non-qualified @p name to a solitary available definition
  //! from any namespace.
  bool hasFunctionNameLocal(ScopeDomain domain, const String& name, SrcPos* srcpos,
                            bool doAutoMatch) const;

  bool checkForRedefinition(const SrcPos& srcpos, ScopeDomain domain,
                            const String& sym) const;
  bool checkForFunctionRedefinition(const SrcPos& srcpos, ScopeDomain domain,
                                    const String& sym) const;

  void dumpDebug(bool recursive = false) const;

  bool hasScopeForFileLocal(const String& absPath) const;
  bool hasScopeForFile(const String& absPath) const;
  void addImportedScope(const String& absPath, std::shared_ptr<Scope> scope);


  //-------- types

  void registerType(const SrcPos& srcpos, const String& name, const Type& type,
                    const NodeList& slotDefs = {});

  const Type& lookupType(const String& name, bool showAmbiguousSymDef) const;
  std::tuple<Type, NodeList> lookupTypeWithSlotParams(const String& name,
                                                      bool showAmbiguousSymDef) const;

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

  //-------- macros

  void registerMacro(const SrcPos& srcpos, const String& name,
                     std::shared_ptr<Macro> macro);
  const Macro* lookupMacro(const SrcPos& srcpos, const String& name,
                           bool showAmbiguousSymDef) const;

  //-------- functions

  void registerFunction(const SrcPos& srcpos, const String& name,
                        std::shared_ptr<AstNode> node);
  std::shared_ptr<FunctionNode> lookupFunction(const String& name,
                                               bool showAmbiguousSymDef) const;


  //-------- variables

  void registerVar(const SrcPos& srcpos, const String& name,
                   std::shared_ptr<AstNode> node);
  const AstNode* lookupVar(const SrcPos& srcpos, const String& name,
                           bool showAmbiguousSymDef) const;

  const AstNode* lookupVarOrFunc(const SrcPos& srcpos, const String& name,
                                 bool showAmbiguousSymDef) const;

  struct FunctionLookup {
    FunctionLookup() = default;
    FunctionLookup(String name, std::shared_ptr<FunctionNode> node)
        : fName(std::move(name))
        , fNode(std::move(node))
    {
    }

    explicit operator bool() const { return fNode != nullptr; }

    String fName;
    std::shared_ptr<FunctionNode> fNode;
  };

  FunctionLookup
  lookupBestFunctionOverload(const String& name,
                             const std::vector<FunctionParameter>& argTypes,
                             const SrcPos& srcpos, bool showAmbiguousSymDef) const;

  bool isVarInOuterFunction(const String& name) const;

  //-------- register export symbols

  bool shouldExportSymbol(const ScopeName& sym) const;
  VizType exportSymbolVisibility(const ScopeName& sym) const;
  bool exportSymbolIsFinal(const ScopeName& sym) const;
  const std::set<String>& attachedExportSymbols(const ScopeName& sym) const;

  void registerSymbolForExport(ScopeDomain domain, const String& sym, VizType viz,
                               bool asFinal);
  void attachSymbolForExport(ScopeDomain domain, const String& sym,
                             const String& attachedSym);

  void exportSymbols(std::shared_ptr<Scope> dstScope, bool propagateIntern) const;
  void propagateImportedScopes(std::shared_ptr<Scope> dstScope) const;


  //-------- global defs

  enum ScopeItemKind {
    kScopeItem_type = 1 << 0,
    kScopeItem_function = 1 << 1,
    kScopeItem_variable = 1 << 2,
    kScopeItem_macro = 1 << 3,
  };

  static const ScopeItemKind kScopeItem_any = ScopeItemKind(
      kScopeItem_type | kScopeItem_function | kScopeItem_variable | kScopeItem_macro);

  class ScopeItem {
  public:
    ScopeItem(const SrcPos& srcpos)
        : fSrcPos(srcpos)
    {
    }

    virtual ~ScopeItem() {}

    const SrcPos& srcpos() const { return fSrcPos; }

    virtual ScopeItemKind kind() const = 0;

  protected:
    SrcPos fSrcPos;
  };

private:
  void registerScopeItem(const ScopeName& name, std::shared_ptr<ScopeItem> item);

  struct LookupResult {
    LookupResult()
        : fItem(nullptr)
        , fInOuterFunc(false)
    {
    }

    LookupResult(ScopeItem* item, bool inOuterFunc)
        : fItem(item)
        , fInOuterFunc(inOuterFunc)
    {
    }

    LookupResult(const LookupResult& other)
        : fItem(other.fItem)
        , fInOuterFunc(other.fInOuterFunc)
    {
    }

    LookupResult& operator=(const LookupResult& other)
    {
      fItem = other.fItem;
      fInOuterFunc = other.fInOuterFunc;
      return *this;
    }

    ScopeItem* fItem;
    bool fInOuterFunc;
  };

  LookupResult lookupItem(const SrcPos& srcpos, const ScopeName& name, bool showError,
                          ScopeItemKind filterKind) const;

  VizType reduceVizType(VizType in) const;

  LookupResult lookupItemLocalImpl(const SrcPos& srcpos, const ScopeName& name,
                                   bool showError, ScopeItemKind filterKind,
                                   bool doAutoMatch) const;

  void dumpDebugImpl(int level) const;

  void exportAllSymbols(std::shared_ptr<Scope> dstScope, bool propagateIntern) const;
  void exportAttachedSymbols(std::shared_ptr<Scope> dstScope, const ScopeName& fullKey,
                             VizType vizType, bool isFinal) const;

  FunctionLookup
  lookupBestFunctionOverloadLocalImpl(const SrcPos& srcpos, const ScopeName& name,
                                      const std::vector<FunctionParameter>& argTypes,
                                      bool showError) const;

  //-------- data members

  using BaseScopeMap = std::map<String, std::shared_ptr<ScopeItem>>;
  using NsScopeMap = std::map<ScopeName, BaseScopeMap>;
  using ImportedScope = std::map<String, std::shared_ptr<Scope>>;
  using AttachedSymbols = std::set<String>;

  NsScopeMap fMap;
  std::shared_ptr<Scope> fParent;

  struct VisibilityPair {
    VizType fViz;
    bool fIsFinal;
    AttachedSymbols fAttachedSymbols;
  };

  using VizMap = std::map<ScopeName, VisibilityPair>;
  VizMap fVisibility;

  ImportedScope fImportedScopes;
  ScopeLevel fLevel;
};

inline std::shared_ptr<Scope> makeScope(ScopeLevel level)
{
  return std::make_shared<Scope>(level);
}

inline std::shared_ptr<Scope> makeScope(ScopeLevel level, std::shared_ptr<Scope> parent)
{
  return std::make_shared<Scope>(level, std::move(parent));
}


//--------------------------------------------------------------------------

class ScopeGuard {
public:
  ScopeGuard(std::shared_ptr<Scope>& scope, std::shared_ptr<Scope> newScope)
      : fScopeLoc(scope)
      , fPrevScope(scope)
  {
    fScopeLoc = newScope;
  }

  ~ScopeGuard() { reset(); }

  void setScope(std::shared_ptr<Scope> newScope) { fScopeLoc = newScope; }

  void reset() { fScopeLoc = fPrevScope; }

  std::shared_ptr<Scope>& fScopeLoc;
  std::shared_ptr<Scope> fPrevScope;
};


class ScopeHelper {
public:
  ScopeHelper(std::shared_ptr<Scope>& scope, bool doExport, bool isInnerScope,
              bool doPropagateIntern, ScopeLevel level)
      : fScopeLoc(scope)
      , fPrevScope(scope)
      , fDoExport(doExport)
      , fIsInnerScope(isInnerScope)
      , fDoPropagateIntern(doPropagateIntern)
  {
    fScopeLoc = makeScope(level, fScopeLoc);
  }

  ~ScopeHelper()
  {
    unrollScopes(fScopeLoc, fPrevScope, fDoExport, fIsInnerScope, fDoPropagateIntern);
  }


  static void unrollScopes(std::shared_ptr<Scope>& scopeLoc,
                           std::shared_ptr<Scope> prevScope, bool doExport,
                           bool isInnerScope, bool doPropagateIntern)
  {
    std::shared_ptr<Scope> scope = scopeLoc;
    while (scope && scope != prevScope) {
      auto parent = scope->parent();

      if (parent && doExport) {
        scope->exportSymbols(parent, doPropagateIntern);

        if (!isInnerScope && parent == prevScope) {
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
  bool fDoPropagateIntern;
};
}  // namespace herschel
