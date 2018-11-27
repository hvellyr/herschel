/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "scope.hpp"

#include "macro.hpp"
#include "strbuf.hpp"
#include "typectx.hpp"
#include "ast.hpp"
#include "errcodes.hpp"
#include "log.hpp"
#include "symbol.hpp"
#include "utils.hpp"


namespace herschel {

static Type sInvalidType;


class TypeScopeItem : public Scope::ScopeItem {
public:
  TypeScopeItem(const SrcPos& srcpos, const Type& type)
      : ScopeItem(srcpos)
      , fType(type)
  {
  }


  Scope::ScopeItemKind kind() const override { return Scope::kScopeItem_type; }

  const Type& type() const { return fType; }

  //-------- data members

  Type fType;
};


//--------------------------------------------------------------------------

class MacroScopeItem : public Scope::ScopeItem {
public:
  MacroScopeItem(const SrcPos& srcpos, std::shared_ptr<Macro> macro)
      : ScopeItem(srcpos)
      , fMacro(macro)
  {
  }


  Scope::ScopeItemKind kind() const override { return Scope::kScopeItem_macro; }

  const Macro* macro() const { return fMacro.get(); }

  //-------- data members

  std::shared_ptr<Macro> fMacro;
};


//--------------------------------------------------------------------------

class NodeScopeItem : public Scope::ScopeItem {
public:
  NodeScopeItem(const SrcPos& srcpos, Scope::ScopeItemKind kind,
                std::shared_ptr<AstNode> node)
      : ScopeItem(srcpos)
      , fKind(kind)
      , fNode(node)
  {
  }


  Scope::ScopeItemKind kind() const override { return fKind; }

  const AstNode* node() const { return fNode.get(); }

  //-------- data members

  Scope::ScopeItemKind fKind;
  std::shared_ptr<AstNode> fNode;
};


//------------------------------------------------------------------------------

Scope::Scope(ScopeLevel level)
    : fLevel(level)
{
  hr_assert(level == kScopeL_CompileUnit);
}


Scope::Scope(ScopeLevel level, std::shared_ptr<Scope> parent)
    : fParent(std::move(parent))
    , fLevel(level)
{
  hr_assert(implies(level > kScopeL_CompileUnit, fParent));
}


std::shared_ptr<Scope> Scope::parent() const
{
  return fParent;
}


ScopeLevel Scope::scopeLevel() const
{
  return fLevel;
}


void Scope::registerScopeItem(const ScopeName& name, std::shared_ptr<ScopeItem> item)
{
  hr_assert(item);

  auto result = lookupItemLocalImpl(SrcPos(), name, K(showError), !K(doAutoMatch));
  if (result.fItem) {
    errorf(item->srcpos(), E_SymbolRedefined, "redefinition of symbol '%s'",
           (zstring)StrHelper(name.fName));
    errorf(result.fItem->srcpos(), E_SymbolRedefined, "symbol was defined here");

    return;
  }

  ScopeName base(name.fDomain, herschel::baseName(name.fName));
  String ns(herschel::nsName(name.fName));

  NsScopeMap::iterator it = fMap.find(base);
  if (it != fMap.end())
    it->second.insert(std::make_pair(ns, item));
  else
    fMap[base].insert(std::make_pair(ns, item));
}


Scope::LookupResult Scope::lookupItemLocal(const SrcPos& srcpos, const ScopeName& name,
                                           bool showError) const
{
  return lookupItemLocalImpl(srcpos, name, showError, K(doAutoMatch));
}


Scope::LookupResult Scope::lookupItemLocalImpl(const SrcPos& srcpos,
                                               const ScopeName& name, bool showError,
                                               bool doAutoMatch) const
{
  ScopeName base(name.fDomain, herschel::baseName(name.fName));
  ScopeName ns(name.fDomain, herschel::nsName(name.fName));

  NsScopeMap::const_iterator it = fMap.find(base);
  if (it != fMap.end()) {
    if (doAutoMatch && !isQualified(name.fName)) {
      if (it->second.size() == 1) {
        return LookupResult(it->second.begin()->second.get(), !K(inOuterFunc));
      }
      else if (showError) {
        errorf(srcpos, E_AmbiguousSym, "ambiguous symbol '%s' usage",
               (zstring)StrHelper(base.fName));
        for (BaseScopeMap::const_iterator vit = it->second.begin();
             vit != it->second.end(); vit++) {
          String fullKey = qualifyId(vit->first, it->first.fName);
          errorf(vit->second->srcpos(), E_AmbiguousSym, "symbol '%s' was defined here",
                 (zstring)StrHelper(fullKey));
        }
      }
    }
    else {
      BaseScopeMap::const_iterator vit = it->second.find(ns.fName);
      if (vit != it->second.end()) {
        return LookupResult(vit->second.get(), !K(inOuterFunc));
      }
    }
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end(); it++) {
    auto lv = it->second->lookupItemLocalImpl(srcpos, name, showError, doAutoMatch);
    if (lv.fItem)
      return lv;
  }


  return LookupResult();
}


Scope::LookupResult Scope::lookupItem(const SrcPos& srcpos, const ScopeName& name,
                                      bool showError) const
{
  const Scope* scope = this;
  bool crossedFuncLevel = false;

  while (scope) {
    auto lv = scope->lookupItemLocalImpl(srcpos, name, showError, K(doAutoMatch));
    if (lv.fItem)
      return LookupResult(lv.fItem, crossedFuncLevel);

    if (scope->scopeLevel() == kScopeL_Function)
      crossedFuncLevel = true;
    scope = scope->parent().get();
  }

  return LookupResult();
}


bool Scope::hasName(ScopeDomain domain, const String& name, SrcPos* srcpos) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(domain, name), !K(showError));
  if (lv.fItem) {
    *srcpos = lv.fItem->srcpos();
    return true;
  }
  return false;
}


bool Scope::hasNameLocal(ScopeDomain domain, const String& name, SrcPos* srcpos,
                         bool doAutoMatch) const
{
  auto lv =
      lookupItemLocalImpl(SrcPos(), ScopeName(domain, name), !K(showError), doAutoMatch);
  if (lv.fItem) {
    *srcpos = lv.fItem->srcpos();
    return true;
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end(); it++) {
    if (it->second->hasNameLocal(domain, name, srcpos, doAutoMatch))
      return true;
  }

  return false;
}


bool Scope::checkForRedefinition(const SrcPos& srcpos, ScopeDomain domain,
                                 const String& sym) const
{
  SrcPos firstSrcpos;
  if (hasNameLocal(domain, sym, &firstSrcpos, !K(doAutoMatch))) {
    errorf(srcpos, E_Redefinition, "Redefinition of '%s'.", (zstring)StrHelper(sym));
    errorf(firstSrcpos, E_Redefinition, "'%s' previously defined here.",
           (zstring)StrHelper(sym));
    return true;
  }

  return false;
}


bool Scope::hasScopeForFileLocal(const String& absPath) const
{
  ImportedScope::const_iterator it = fImportedScopes.find(absPath);
  if (it != fImportedScopes.end())
    return true;

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end(); it++) {
    if (it->second->hasScopeForFile(absPath))
      return true;
  }

  return false;
}


bool Scope::hasScopeForFile(const String& absPath) const
{
  const Scope* scope = this;

  while (scope) {
    if (scope->hasScopeForFileLocal(absPath))
      return true;
    scope = scope->parent().get();
  }

  return false;
}


void Scope::addImportedScope(const String& absPath, std::shared_ptr<Scope> scope)
{
  auto it = fImportedScopes.find(absPath);
  hr_assert(it == fImportedScopes.end());
  fImportedScopes.insert(std::make_pair(absPath, scope));
}


//..........................................................................

void Scope::registerType(const SrcPos& srcpos, const String& name, const Type& type)
{
  hr_assert(!type.isArray());
  hr_assert(type.isDef());

  registerScopeItem(ScopeName(kNormal, name),
                    std::make_shared<TypeScopeItem>(srcpos, type));
}


const Type& Scope::lookupType(const String& name, bool showAmbiguousSymDef) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(kNormal, name), showAmbiguousSymDef);
  if (lv.fItem && lv.fItem->kind() == kScopeItem_type)
    return dynamic_cast<const TypeScopeItem*>(lv.fItem)->type();

  return sInvalidType;
}


Type Scope::normalizeType(const Type& type)
{
  if (type.isRef()) {
    Type referedType = lookupType(type.typeName(), K(showAmbiguousSymDef));
    if (referedType.isDef()) {
      if (referedType.isAlias())
        return normalizeType(referedType, type);

      // we normally don't want to have full types here (these would lead to
      // unnecessary data expansion and possible issues with recursive types).
      // Rewrite the typeref to have the fully qualified type name
      return Type::makeTypeRef(referedType.typeName(), type);
    }
  }
  else if (type.isAlias())
    return type.aliasReplaces();

  return type;
}


Type Scope::normalizeType(const Type& type, const Type& refType) const
{
  Type baseType = type;
  Type returnType = type;

  if (type.isArray()) {
    baseType = type.arrayRootType();
  }
  else if (type.isRef()) {
    baseType = type;
  }
  else if (type.isAlias()) {
    baseType = type.aliasReplaces();
    baseType.setIsValueType(refType.isValueType());

    if (refType.hasConstraints()) {
      if (baseType.hasConstraints())
        throw TypeConstraintsConflictException(
            refType, String("Alias and using type ref has conflicting type constraints"));
      else
        baseType = baseType.setConstraints(refType.constraints());
    }

    returnType = baseType;
  }

  hr_assert(baseType.isDef());

  if (type.generics().size() != refType.generics().size())
    throw TypeRefMatchException(
        refType, (StringBuffer()
                  << "Requires " << fromInt(type.generics().size())
                  << " type arguments, found " << fromInt(refType.generics().size()))
                     .toString());

  if (type.hasGenerics()) {
    TypeCtx localCtx;
    for (size_t i = 0; i < type.generics().size(); i++) {
      Type gen = type.generics()[i];
      hr_assert(gen.isRef());

      String genName = gen.typeName();
      Type genReplacement = refType.generics()[i];
      localCtx.registerType(genName, genReplacement);
    }

    returnType = baseType.replaceGenerics(localCtx);
  }

  if (refType.isArray())
    return refType.rebase(returnType);

  return returnType;
}


//! lookup a type via another type.
Type Scope::lookupType(const Type& type) const
{
  if (type.isArray()) {
    return Type::makeArray(lookupType(type.arrayBaseType()), type.arraySizeIndicator(),
                           type.isValueType());
  }
  else if (type.isType() || type.isRecord()) {
    // TODO: something to be done here?
    return type;
  }
  else if (type.isRef()) {
    Type resolvedType = lookupType(type.typeName(), K(showAmbiguousSymDef));
    if (resolvedType.isDef()) {
      if (resolvedType.isOpen())
        return normalizeType(resolvedType, type);
      return resolvedType;
    }
  }
  else if (type.isFunction()) {
    // TODO: something to be done here?
    return type;
  }
  else if (type.isUnion()) {
    TypeVector types;
    for (size_t i = 0; i < type.unionTypes().size(); ++i) {
      types.push_back(lookupType(type.unionTypes()[i]));
    }
    return Type::makeUnion(types, type.isValueType());
  }
  else if (type.isSequence()) {
    TypeVector types;
    for (size_t i = 0; i < type.seqTypes().size(); ++i) {
      types.push_back(lookupType(type.seqTypes()[i]));
    }
    return Type::makeSeq(types, type.isValueType());
  }

  return Type();
}


Type Scope::lookupType_unused(const Type& type) const
{
  Type baseType;
  if (type.isArray()) {
    baseType = type.arrayRootType();
  }
  else if (type.isRef()) {
    baseType = type;
  }

  if (baseType.isDef()) {
    Type fullType = lookupType(baseType.typeName(), K(showAmbiguousSymDef));
    if (fullType.isDef())
      return normalizeType(fullType, type);
  }

  return type;
}


//..............................................................................

void Scope::registerMacro(const SrcPos& srcpos, const String& name,
                          std::shared_ptr<Macro> macro)
{
  registerScopeItem(ScopeName(kNormal, name),
                    std::make_shared<MacroScopeItem>(srcpos, macro));
}


const Macro* Scope::lookupMacro(const SrcPos& srcpos, const String& name,
                                bool showAmbiguousSymDef) const
{
  auto lv = lookupItem(srcpos, ScopeName(kNormal, name), showAmbiguousSymDef);
  if (lv.fItem && lv.fItem->kind() == kScopeItem_macro)
    return dynamic_cast<const MacroScopeItem*>(lv.fItem)->macro();
  return nullptr;
}


//............................................................................

void Scope::registerFunction(const SrcPos& srcpos, const String& name,
                             std::shared_ptr<AstNode> node)
{
  registerScopeItem(ScopeName(kNormal, name),
                    std::make_shared<NodeScopeItem>(srcpos, kScopeItem_function, node));
}


const AstNode* Scope::lookupFunction(const String& name, bool showAmbiguousSymDef) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(kNormal, name), showAmbiguousSymDef);
  if (lv.fItem && lv.fItem->kind() == kScopeItem_function)
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return nullptr;
}


//............................................................................

void Scope::registerVar(const SrcPos& srcpos, const String& name,
                        std::shared_ptr<AstNode> node)
{
  registerScopeItem(ScopeName(kNormal, name),
                    std::make_shared<NodeScopeItem>(srcpos, kScopeItem_variable, node));
}


const AstNode* Scope::lookupVar(const String& name, bool showAmbiguousSymDef) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(kNormal, name), showAmbiguousSymDef);
  if (lv.fItem && lv.fItem->kind() == kScopeItem_variable)
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return nullptr;
}


const AstNode* Scope::lookupVarOrFunc(const String& name, bool showAmbiguousSymDef) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(kNormal, name), showAmbiguousSymDef);
  if (lv.fItem && (lv.fItem->kind() == kScopeItem_variable ||
                   lv.fItem->kind() == kScopeItem_function))
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return nullptr;
}


bool Scope::isVarInOuterFunction(const String& name) const
{
  auto lv = lookupItem(SrcPos(), ScopeName(kNormal, name), !K(showError));
  return lv.fItem && lv.fInOuterFunc;
}


zstring Scope::scopeLevelName(ScopeLevel level)
{
  switch (level) {
  case kScopeL_CompileUnit: return "compile-unit";
  case kScopeL_Module: return "module";
  case kScopeL_Function: return "function";
  case kScopeL_Local: return "local";
  };
  return "???";
}


zstring Scope::scopeLevelName() const
{
  return scopeLevelName(scopeLevel());
}

void Scope::dumpDebug(bool recursive) const
{
  fprintf(stderr, "[------- Scope Dump [%p] - %s ----------------------\n", this,
          scopeLevelName(scopeLevel()));
  dumpDebugImpl();
  if (recursive) {
    Scope* sc0 = parent().get();
    while (sc0) {
      fprintf(stderr, "----- [%p] - %s -----\n", sc0, scopeLevelName(sc0->scopeLevel()));
      sc0->dumpDebugImpl();
      sc0 = sc0->parent().get();
    }
  }
  fprintf(stderr, "]------- Scope Dump [%p] ----------------------\n", this);
}


void Scope::dumpDebugImpl() const
{
  for (NsScopeMap::const_iterator it = fMap.begin(); it != fMap.end(); it++) {
    for (BaseScopeMap::const_iterator vit = it->second.begin(); vit != it->second.end();
         vit++) {
      String key = qualifyId(vit->first, it->first.fName);
      fprintf(stderr, "%s\n", (zstring)StrHelper(key));
    }
  }

  if (!fImportedScopes.empty()) {
    fprintf(stderr, "--- attached scopes: ----\n");
    for (ImportedScope::const_iterator it = fImportedScopes.begin();
         it != fImportedScopes.end(); it++) {
      fprintf(stderr, "[ATTACHED: %s]\n", (zstring)StrHelper(it->first));
      it->second->dumpDebugImpl();
    }
  }
}


bool Scope::shouldExportSymbol(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  return (it != fVisibility.end() && it->second.fViz != kUnset);
}


VizType Scope::exportSymbolVisibility(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end() && it->second.fViz != kUnset)
    return it->second.fViz;
  return kPrivate;
}


bool Scope::exportSymbolIsFinal(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end())
    return it->second.fIsFinal;
  return false;
}


const std::set<String>& Scope::attachedExportSymbols(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end())
    return it->second.fAttachedSymbols;

  static std::set<String> emptySet;
  return emptySet;
}


void Scope::registerSymbolForExport(ScopeDomain domain, const String& sym, VizType viz,
                                    bool isFinal)
{
  VizMap::iterator it = fVisibility.find(ScopeName(domain, sym));
  if (it == fVisibility.end()) {
    VisibilityPair vp;
    vp.fViz = viz;
    vp.fIsFinal = isFinal;
    fVisibility.insert(std::make_pair(ScopeName(domain, sym), vp));
  }
  else {
    bool registerAttachedSymbols = it->second.fViz == kUnset;

    it->second.fViz = viz;
    it->second.fIsFinal = isFinal;

    if (registerAttachedSymbols) {
      for (AttachedSymbols::iterator ait = it->second.fAttachedSymbols.begin();
           ait != it->second.fAttachedSymbols.end(); ait++) {
        registerSymbolForExport(domain, *ait, viz, isFinal);
      }
    }
  }
}


void Scope::attachSymbolForExport(ScopeDomain domain, const String& sym,
                                  const String& attachedSym)
{
  auto it = fVisibility.find(ScopeName(domain, sym));
  if (it == fVisibility.end()) {
    VisibilityPair vp;
    vp.fViz = kUnset;
    vp.fIsFinal = false;
    vp.fAttachedSymbols.insert(attachedSym);
    fVisibility.insert(std::make_pair(ScopeName(domain, sym), vp));
  }
  else {
    it->second.fAttachedSymbols.insert(attachedSym);
    registerSymbolForExport(domain, attachedSym, it->second.fViz, it->second.fIsFinal);
  }
}


VizType Scope::reduceVizType(VizType in) const
{
  switch (in) {
  case kUnset:
  case kPrivate: return kPrivate;
  case kInner: return kPrivate;
  case kOuter: return kOuter;
  case kPublic: return kPublic;
  }
  hr_invalid("");
  return kPrivate;
}


void Scope::exportAttachedSymbols(std::shared_ptr<Scope> dstScope,
                                  const ScopeName& fullKey, VizType vizType,
                                  bool isFinal) const
{
  const auto& attachedSyms = attachedExportSymbols(fullKey);
  for (const auto& attachedSym : attachedSyms) {
    dstScope->attachSymbolForExport(fullKey.fDomain, fullKey.fName, attachedSym);
  }
}


void Scope::exportAllSymbols(std::shared_ptr<Scope> dstScope, bool propagateOuter) const
{
  // export all
  VizType vizAllType = exportSymbolVisibility(ScopeName(kNormal, String("*")));
  if (vizAllType != kPrivate && (propagateOuter || vizAllType != kOuter)) {
    VizType reducedVizType = reduceVizType(vizAllType);
    bool isFinal = exportSymbolIsFinal(ScopeName(kNormal, String("*")));

    for (NsScopeMap::const_iterator it = fMap.begin(); it != fMap.end(); it++) {
      for (BaseScopeMap::const_iterator vit = it->second.begin(); vit != it->second.end();
           vit++) {
        ScopeName fullKey(it->first.fDomain, qualifyId(vit->first, it->first.fName));

        if (!shouldExportSymbol(fullKey) ||
            (vizAllType == exportSymbolVisibility(fullKey) &&
             isFinal == exportSymbolIsFinal(fullKey))) {
          dstScope->registerScopeItem(fullKey, vit->second);

          if (reducedVizType != kPrivate) {
            dstScope->registerSymbolForExport(fullKey.fDomain, fullKey.fName,
                                              reducedVizType, isFinal);
            exportAttachedSymbols(dstScope, fullKey, reducedVizType, isFinal);
          }
        }
      }
    }
  }
}


void Scope::exportSymbols(std::shared_ptr<Scope> dstScope, bool propagateOuter) const
{
  if (shouldExportSymbol(ScopeName(kNormal, String("*")))) {
    exportAllSymbols(dstScope, propagateOuter);
  }
  else {
    // selective export
    for (NsScopeMap::const_iterator it = fMap.begin(); it != fMap.end(); it++) {
      for (BaseScopeMap::const_iterator vit = it->second.begin(); vit != it->second.end();
           vit++) {
        ScopeName fullKey(it->first.fDomain, qualifyId(vit->first, it->first.fName));

        VizType vizType = exportSymbolVisibility(fullKey);
        if (vizType != kPrivate && (propagateOuter || vizType != kOuter)) {
          VizType reducedVizType = reduceVizType(vizType);
          bool isFinal = exportSymbolIsFinal(fullKey);

          dstScope->registerScopeItem(fullKey, vit->second);
          if (reducedVizType != kPrivate) {
            dstScope->registerSymbolForExport(fullKey.fDomain, fullKey.fName,
                                              reducedVizType, isFinal);
            exportAttachedSymbols(dstScope, fullKey, reducedVizType, isFinal);
          }
        }
      }
    }
  }
}


void Scope::propagateImportedScopes(std::shared_ptr<Scope> dstScope) const
{
  hr_assert(this != dstScope.get());

  for (const auto& impScope : fImportedScopes) {
    if (!dstScope->hasScopeForFileLocal(impScope.first))
      dstScope->addImportedScope(impScope.first, impScope.second);
  }
}

}  // namespace herschel
