/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "macro.h"
#include "scope.h"
#include "strbuf.h"
#include "typectx.h"
#include "apt.h"
#include "errcodes.h"
#include "log.h"
#include "symbol.h"


using namespace herschel;

static Type sInvalidType;


//------------------------------------------------------------------------------

namespace herschel
{
  //--------------------------------------------------------------------------

  class TypeScopeItem : public Scope::ScopeItem
  {
  public:
    TypeScopeItem(const SrcPos& srcpos, const Type& type)
      : ScopeItem(srcpos),
        fType(type)
    { }


    virtual Scope::ScopeItemKind kind() const
    {
      return Scope::kScopeItem_type;
    }

    const Type& type() const
    {
      return fType;
    }

    //-------- data members

    Type fType;
  };


  class UnitScopeItem : public Scope::ScopeItem
  {
  public:
    UnitScopeItem(const SrcPos& srcpos, const TypeUnit& unit,
                  AptNode* transformFunc)
      : ScopeItem(srcpos),
        fUnit(unit),
        fTransformFunc(transformFunc)
    { }

    virtual Scope::ScopeItemKind kind() const
    {
      return Scope::kScopeItem_unit;
    }

    const TypeUnit& unit() const
    {
      return fUnit;
    }

    const Type& type() const
    {
      return fUnit.effType();
    }

    const String& unitName() const
    {
      return fUnit.name();
    }

    //-------- data members

    TypeUnit fUnit;
    Ptr<AptNode> fTransformFunc;
  };


  //--------------------------------------------------------------------------

  class MacroScopeItem : public Scope::ScopeItem
  {
  public:
    MacroScopeItem(const SrcPos& srcpos, Macro* macro)
      : ScopeItem(srcpos),
        fMacro(macro)
    { }


    virtual Scope::ScopeItemKind kind() const
    {
      return Scope::kScopeItem_macro;
    }

    const Macro* macro() const
    {
      return fMacro;
    }

    //-------- data members

    Ptr<Macro> fMacro;
  };


  //--------------------------------------------------------------------------

  class NodeScopeItem : public Scope::ScopeItem
  {
  public:
    NodeScopeItem(const SrcPos& srcpos,
                  Scope::ScopeItemKind kind, AptNode* node)
      : ScopeItem(srcpos),
        fKind(kind),
        fNode(node)
    { }


    virtual Scope::ScopeItemKind kind() const
    {
      return fKind;
    }

    const AptNode* node() const
    {
      return fNode;
    }

    //-------- data members

    Scope::ScopeItemKind fKind;
    Ptr<AptNode>         fNode;
  };
};


//------------------------------------------------------------------------------

Scope::Scope(ScopeLevel level)
  : fLevel(level)
{
  hr_assert(level == kScopeL_CompileUnit);
}


Scope::Scope(ScopeLevel level, Scope* parent)
  : fParent(parent),
    fLevel(level)
{
  hr_assert(implies(level > kScopeL_CompileUnit, parent != NULL));
}


Scope*
Scope::parent() const
{
  return fParent;
}


ScopeLevel
Scope::scopeLevel() const
{
  return fLevel;
}


void
Scope::registerScopeItem(const ScopeName& name, ScopeItem* item)
{
  hr_assert(item != NULL);
  hr_assert(lookupItemLocalImpl(SrcPos(), name,
                                !K(showError), !K(doAutoMatch)).fItem == NULL);

  ScopeName base(name.fDomain, herschel::baseName(name.fName));
  String ns(herschel::nsName(name.fName));

  NsScopeMap::iterator it = fMap.find(base);
  if (it != fMap.end())
    it->second.insert(std::make_pair(ns, item));
  else
    fMap[base].insert(std::make_pair(ns, item));
}


Scope::LookupResult
Scope::lookupItemLocal(const SrcPos& srcpos,
                       const ScopeName& name, bool showError) const
{
  return lookupItemLocalImpl(srcpos, name, showError, K(doAutoMatch));
}


Scope::LookupResult
Scope::lookupItemLocalImpl(const SrcPos& srcpos,
                           const ScopeName& name, bool showError,
                           bool doAutoMatch) const
{
  ScopeName base(name.fDomain, herschel::baseName(name.fName));
  ScopeName ns(name.fDomain, herschel::nsName(name.fName));

  NsScopeMap::const_iterator it = fMap.find(base);
  if (it != fMap.end()) {
    if (doAutoMatch && !isQualified(name.fName)) {
      if (it->second.size() == 1) {
        return LookupResult(it->second.begin()->second.obj(),
                            !K(inOuterFunc));
      }
      else if (showError) {
        errorf(srcpos, E_AmbiguousSym, "ambiguous symbol '%s' usage",
               (const char*)StrHelper(base.fName));
        for (BaseScopeMap::const_iterator vit = it->second.begin();
             vit != it->second.end();
             vit++)
        {
          String fullKey = qualifyId(vit->first, it->first.fName);
          errorf(vit->second->srcpos(), E_AmbiguousSym,
                 "symbol '%s' was defined here",
                 (const char*)StrHelper(fullKey));
        }
      }
    }
    else {
      BaseScopeMap::const_iterator vit = it->second.find(ns.fName);
      if (vit != it->second.end()) {
        return LookupResult(vit->second.obj(), !K(inOuterFunc));
      }
    }
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    LookupResult lv = it->second->lookupItemLocalImpl(srcpos, name,
                                                      showError,
                                                      doAutoMatch);
    if (lv.fItem != NULL)
      return lv;
  }


  return LookupResult();
}


Scope::LookupResult
Scope::lookupItem(const SrcPos& srcpos,
                  const ScopeName& name, bool showError) const
{
  const Scope* scope = this;
  bool crossedFuncLevel = false;

  while (scope != NULL) {
    LookupResult lv = scope->lookupItemLocalImpl(srcpos, name,
                                                 showError, K(doAutoMatch));
    if (lv.fItem != NULL)
      return LookupResult(lv.fItem, crossedFuncLevel);

    if (scope->scopeLevel() == kScopeL_Function)
      crossedFuncLevel = true;
    scope = scope->parent();
  }

  return LookupResult();
}


bool
Scope::hasName(ScopeDomain domain, const String& name, SrcPos* srcpos) const
{
  LookupResult lv = lookupItem(SrcPos(), ScopeName(domain, name),
                               !K(showError));
  if (lv.fItem != NULL) {
    *srcpos = lv.fItem->srcpos();
    return true;
  }
  return false;
}


bool
Scope::hasNameLocal(ScopeDomain domain, const String& name, SrcPos* srcpos,
                    bool doAutoMatch) const
{
  LookupResult lv = lookupItemLocalImpl(SrcPos(), ScopeName(domain, name),
                                        !K(showError), doAutoMatch);
  if (lv.fItem != NULL) {
    *srcpos = lv.fItem->srcpos();
    return true;
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    if (it->second->hasNameLocal(domain, name, srcpos, doAutoMatch))
      return true;
  }

  return false;
}


bool
Scope::checkForRedefinition(const SrcPos& srcpos,
                            ScopeDomain domain,
                            const String& sym) const
{
  SrcPos firstSrcpos;
  if (hasNameLocal(domain, sym, &firstSrcpos, !K(doAutoMatch))) {
    errorf(srcpos, E_Redefinition,
           "Redefinition of '%s'.", (const char*)StrHelper(sym));
    errorf(firstSrcpos, E_Redefinition,
           "'%s' previously defined here.",
           (const char*)StrHelper(sym));
    return true;
  }

  return false;
}


bool
Scope::hasScopeForFileLocal(const String& absPath) const
{
  ImportedScope::const_iterator it = fImportedScopes.find(absPath);
  if (it != fImportedScopes.end())
    return true;

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    if (it->second->hasScopeForFile(absPath))
      return true;
  }

  return false;
}


bool
Scope::hasScopeForFile(const String& absPath) const
{
  const Scope* scope = this;

  while (scope != NULL) {
    if (scope->hasScopeForFileLocal(absPath))
      return true;
    scope = scope->parent();
  }

  return false;
}


void
Scope::addImportedScope(const String& absPath, Scope* scope)
{
  ImportedScope::iterator it = fImportedScopes.find(absPath);
  hr_assert(it == fImportedScopes.end());
  fImportedScopes.insert(std::make_pair(absPath, scope));
}


//..........................................................................

void
Scope::registerType(const SrcPos& srcpos,
                    const String& name, const Type& type)
{
  hr_assert(!type.isArray());
  hr_assert(type.isDef());

  registerScopeItem(ScopeName(kNormal, name),
                    new TypeScopeItem(srcpos, type));
}


const Type&
Scope::lookupType(const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kNormal, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && lv.fItem->kind() == kScopeItem_type)
    return dynamic_cast<const TypeScopeItem*>(lv.fItem)->type();

  return sInvalidType;
}


TypeUnit
Scope::lookupUnit(const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kUnit, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && lv.fItem->kind() == kScopeItem_unit)
    return dynamic_cast<const UnitScopeItem*>(lv.fItem)->unit();

  return TypeUnit();
}


Type
Scope::normalizeType(const Type& type)
{
  if (type.isRef()) {
    Type referedType = lookupType(type.typeName(), K(showAmbiguousSymDef));
    if (referedType.isDef()) {
      if (referedType.isAlias())
        return normalizeType(referedType, type);

      // we normally don't want to have full types here (these would lead to
      // unnecessary data expansion and possible issues with recursive types).
      // Rewrite the typeref to have the fully qualified type name
      return Type::newTypeRef(referedType.typeName(), type);
    }
  }
  else if (type.isAlias())
    return type.aliasReplaces();

  return type;
}


Type
Scope::normalizeType(const Type& type, const Type& refType) const
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
          refType,
          String("Alias and using type ref has conflicting type constraints"));
      else
        baseType = baseType.setConstraints(refType.constraints());
    }

    returnType = baseType;
  }

  hr_assert(baseType.isDef());

  if (type.generics().size() != refType.generics().size())
    throw TypeRefMatchException(refType,
                                ( StringBuffer()
                                  << "Requires "
                                  << fromInt(type.generics().size())
                                  << " type arguments, found "
                                  << fromInt(refType.generics().size())).toString());

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
Type
Scope::lookupType(const Type& type) const
{
  if (type.isArray()) {
    return Type::newArray(lookupType(type.arrayBaseType()),
                          type.arraySizeIndicator(),
                          type.isValueType());
  }
  else if (type.isType() || type.isClass()) {
    // TODO: something to be done here?
    return type;
  }
  else if (type.isMeasure()) {
    return Type::newMeasure(type.typeName(),
                            lookupType(type.measureBaseType()),
                            type.measureUnit());
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
    return Type::newUnion(types, type.isValueType());
  }
  else if (type.isSequence()) {
    TypeVector types;
    for (size_t i = 0; i < type.seqTypes().size(); ++i) {
      types.push_back(lookupType(type.seqTypes()[i]));
    }
    return Type::newSeq(types, type.isValueType());
  }

  return Type();
}


Type
Scope::lookupType_unused(const Type& type) const
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


void
Scope::registerUnit(const SrcPos& srcpos,
                    const String& unitName, const String& baseUnit,
                    const Type& baseType,
                    AptNode* transformFunc)
{
  registerScopeItem(ScopeName(kUnit, unitName),
                    new UnitScopeItem(srcpos,
                                      TypeUnit(unitName, baseUnit, baseType),
                                      transformFunc));
}


//..............................................................................

void
Scope::registerMacro(const SrcPos& srcpos,
                     const String& name, Macro* macro)
{
  registerScopeItem(ScopeName(kNormal, name),
                    new MacroScopeItem(srcpos, macro));
}


const Macro*
Scope::lookupMacro(const SrcPos& srcpos,
                   const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(srcpos,
                               ScopeName(kNormal, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && lv.fItem->kind() == kScopeItem_macro)
    return dynamic_cast<const MacroScopeItem*>(lv.fItem)->macro();
  return NULL;
}


//............................................................................

void
Scope::registerFunction(const SrcPos& srcpos,
                        const String& name, AptNode* node)
{
  registerScopeItem(ScopeName(kNormal, name),
                    new NodeScopeItem(srcpos, kScopeItem_function, node));
}


const AptNode*
Scope::lookupFunction(const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kNormal, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && lv.fItem->kind() == kScopeItem_function)
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return NULL;
}


//............................................................................

void
Scope::registerVar(const SrcPos& srcpos,
                   const String& name, AptNode* node)
{
  registerScopeItem(ScopeName(kNormal, name),
                    new NodeScopeItem(srcpos, kScopeItem_variable, node));
}


const AptNode*
Scope::lookupVar(const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kNormal, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && lv.fItem->kind() == kScopeItem_variable)
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return NULL;
}


const AptNode*
Scope::lookupVarOrFunc(const String& name, bool showAmbiguousSymDef) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kNormal, name),
                               showAmbiguousSymDef);
  if (lv.fItem != NULL && ( lv.fItem->kind() == kScopeItem_variable ||
                            lv.fItem->kind() == kScopeItem_function ))
    return dynamic_cast<const NodeScopeItem*>(lv.fItem)->node();
  return NULL;
}


bool
Scope::isVarInOuterFunction(const String& name) const
{
  LookupResult lv = lookupItem(SrcPos(),
                               ScopeName(kNormal, name),
                               !K(showError));
  return lv.fItem != NULL && lv.fInOuterFunc;
}


const char*
Scope::scopeLevelName(ScopeLevel level)
{
  switch (level) {
  case kScopeL_CompileUnit: return "compile-unit";
  case kScopeL_Module:      return "module";
  case kScopeL_Function:    return "function";
  case kScopeL_Local:       return "local";
  };
  return "???";
}


const char*
Scope::scopeLevelName() const
{
  return scopeLevelName(scopeLevel());
}

void
Scope::dumpDebug(bool recursive) const
{
  fprintf(stderr, "[------- Scope Dump [%p] - %s ----------------------\n", this,
          scopeLevelName(scopeLevel()));
  dumpDebugImpl();
  if (recursive) {
    Scope* sc0 = parent();
    while (sc0) {
      fprintf(stderr, "----- [%p] - %s -----\n", sc0, scopeLevelName(sc0->scopeLevel()));
      sc0->dumpDebugImpl();
      sc0 = sc0->parent();
    }
  }
  fprintf(stderr, "]------- Scope Dump [%p] ----------------------\n", this);
}


void
Scope::dumpDebugImpl() const
{
  for (NsScopeMap::const_iterator it = fMap.begin();
       it != fMap.end();
       it++)
  {
    for (BaseScopeMap::const_iterator vit = it->second.begin();
         vit != it->second.end();
         vit++)
    {
      String key = qualifyId(vit->first, it->first.fName);
      fprintf(stderr, "%s\n", (const char*)StrHelper(key));
    }
  }

  if (!fImportedScopes.empty()) {
    fprintf(stderr, "--- attached scopes: ----\n");
    for (ImportedScope::const_iterator it = fImportedScopes.begin();
         it != fImportedScopes.end();
         it++)
    {
      fprintf(stderr, "[ATTACHED: %s]\n", (const char*)StrHelper(it->first));
      it->second->dumpDebugImpl();
    }
  }
}


bool
Scope::shouldExportSymbol(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  return (it != fVisibility.end() && it->second.fViz != kUnset);
}


VizType
Scope::exportSymbolVisibility(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end() && it->second.fViz != kUnset)
    return it->second.fViz;
  return kPrivate;
}


bool
Scope::exportSymbolIsFinal(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end())
    return it->second.fIsFinal;
  return false;
}


const std::set<String>& 
Scope::attachedExportSymbols(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end())
    return it->second.fAttachedSymbols;

  static std::set<String> emptySet;
  return emptySet;
}


void
Scope::registerSymbolForExport(ScopeDomain domain, const String& sym,
                               VizType viz, bool isFinal)
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
           ait != it->second.fAttachedSymbols.end();
           ait++)
      {
        registerSymbolForExport(domain, *ait, viz, isFinal);
      }
    }
  }
}


void
Scope::attachSymbolForExport(ScopeDomain domain, const String& sym,
                             const String& attachedSym)
{
  VizMap::iterator it = fVisibility.find(ScopeName(domain, sym));
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


VizType
Scope::reduceVizType(VizType in) const
{
  switch (in) {
  case kUnset:
  case kPrivate:
    return kPrivate;
  case kInner:
    return kPrivate;
  case kOuter:
    return kOuter;
  case kPublic:
    return kPublic;
  }
  hr_invalid("");
  return kPrivate;
}


void
Scope::exportAttachedSymbols(Scope* dstScope,
                             const ScopeName& fullKey, VizType vizType,
                             bool isFinal) const
{
  const AttachedSymbols& attachedSyms = attachedExportSymbols(fullKey);
  for (AttachedSymbols::const_iterator ait = attachedSyms.begin();
       ait != attachedSyms.end();
       ait++)
  {
    String attachedSym = *ait;
    dstScope->attachSymbolForExport(fullKey.fDomain, fullKey.fName,
                                    attachedSym);
  }
}


void
Scope::exportAllSymbols(Scope* dstScope, bool propagateOuter) const
{
  // export all
  VizType vizAllType = exportSymbolVisibility(ScopeName(kNormal, String("*")));
  if (vizAllType != kPrivate && (propagateOuter || vizAllType != kOuter)) {
    VizType reducedVizType = reduceVizType(vizAllType);
    bool isFinal = exportSymbolIsFinal(ScopeName(kNormal, String("*")));

    for (NsScopeMap::const_iterator it = fMap.begin();
         it != fMap.end();
         it++)
    {
      for (BaseScopeMap::const_iterator vit = it->second.begin();
           vit != it->second.end();
           vit++)
      {
        ScopeName fullKey(it->first.fDomain,
                          qualifyId(vit->first, it->first.fName));

        if (!shouldExportSymbol(fullKey) ||
            ( vizAllType == exportSymbolVisibility(fullKey) &&
              isFinal == exportSymbolIsFinal(fullKey) ))
        {
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


void
Scope::exportSymbols(Scope* dstScope, bool propagateOuter) const
{
  if (shouldExportSymbol(ScopeName(kNormal, String("*"))))
  {
    exportAllSymbols(dstScope, propagateOuter);
  }
  else
  {
    // selective export
    for (NsScopeMap::const_iterator it = fMap.begin();
         it != fMap.end();
         it++)
    {
      for (BaseScopeMap::const_iterator vit = it->second.begin();
           vit != it->second.end();
           vit++)
      {
        ScopeName fullKey(it->first.fDomain,
                          qualifyId(vit->first, it->first.fName));

        VizType vizType = exportSymbolVisibility(fullKey);
        if (vizType != kPrivate &&
            (propagateOuter || vizType != kOuter)) {
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


void
Scope::propagateImportedScopes(Scope* dstScope) const
{
  hr_assert(this != dstScope);

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    if (!dstScope->hasScopeForFileLocal(it->first))
      dstScope->addImportedScope(it->first, it->second);
  }
}


//============================================================================

#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>
#include <iostream>

SUITE(Scope)
{
  TEST(lookupType)
  {
    SrcPos sp;
    TypeVector generics;
    generics.push_back(Type::newTypeRef(String("Char"), K(isValue)));
    TypeConstVector constraints;
    Type t0 = Type::newTypeRef(String("Foo"), generics, constraints,
                               K(isValue));

    Ptr<Scope> s0 = new Scope(kScopeL_CompileUnit);
    Type t1 = s0->lookupType_unused(t0);
    // printf("%s\n", (const char*)StrHelper(t1.toString()));
    CHECK(t1.isDef());
  }
}

#endif  // #if defined(UNITTESTS)

