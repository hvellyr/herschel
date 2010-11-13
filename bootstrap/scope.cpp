/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
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


using namespace heather;

static Type sInvalidType;


//------------------------------------------------------------------------------

namespace heather
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
  assert(level == kScopeL_CompileUnit);
}


Scope::Scope(ScopeLevel level, Scope* parent)
  : fParent(parent),
    fLevel(level)
{
  assert(heaImplies(level > kScopeL_CompileUnit, parent != NULL));
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
  assert(item != NULL);
  assert(lookupItemLocalImpl(SrcPos(), name, false, false).fItem == NULL);

  ScopeName base(name.fDomain, heather::baseName(name.fName));
  String ns(heather::nsName(name.fName));

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
  return lookupItemLocalImpl(srcpos, name, showError, true);
}


Scope::LookupResult
Scope::lookupItemLocalImpl(const SrcPos& srcpos,
                           const ScopeName& name, bool showError,
                           bool doAutoMatch) const
{
  ScopeName base(name.fDomain, heather::baseName(name.fName));
  ScopeName ns(name.fDomain, heather::nsName(name.fName));

  // fprintf(stderr, "Look for '%s'\n", (const char*)StrHelper(name));

  NsScopeMap::const_iterator it = fMap.find(base);
  if (it != fMap.end()) {
    if (doAutoMatch && !isQualified(name.fName)) {
      if (it->second.size() == 1) {
        // fprintf(stderr, " ... found something single\n");
        return LookupResult(it->second.begin()->second.obj(), false);
      }
      else if (showError) {
        errorf(srcpos, E_AmbiguousSym,
               "ambiguous symbol '%s' usage", (const char*)StrHelper(base.fName));
        for (BaseScopeMap::const_iterator vit = it->second.begin();
             vit != it->second.end();
             vit++)
        {
          String fullKey = qualifyId(vit->first, it->first.fName);
          errorf(vit->second->srcpos(), E_AmbiguousSym,
                 "symbol '%s' was defined here", (const char*)StrHelper(fullKey));
        }
      }
    }
    else {
      BaseScopeMap::const_iterator vit = it->second.find(ns.fName);
      if (vit != it->second.end()) {
        // fprintf(stderr, " ... found something special\n");
        return LookupResult(vit->second.obj(), false);
      }
    }
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    // fprintf(stderr, "Search for '%s' in '%s'\n",
    //         (const char*)StrHelper(name), (const char*)StrHelper(it->first));
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
                                                 showError, true);
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
  LookupResult lv = lookupItem(SrcPos(), ScopeName(domain, name), false);
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
                                        false, doAutoMatch);
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
  if (hasNameLocal(domain, sym, &firstSrcpos, false)) {
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
  assert(it == fImportedScopes.end());
  fImportedScopes.insert(std::make_pair(absPath, scope));
}


//..........................................................................

void
Scope::registerType(const SrcPos& srcpos,
                    const String& name, const Type& type)
{
  assert(!type.isArray());
  assert(type.isDef());

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
    returnType = baseType;
  }

  assert(baseType.isDef());

  if (!type.generics().size() == refType.generics().size())
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
      assert(gen.isRef());

      String genName = gen.typeName();
      localCtx.registerType(genName, refType.generics()[i]);
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
    Type resolvedType = lookupType(type.typeName(), true);
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
    Type fullType = lookupType(baseType.typeName(), true);
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
                               false);
  return lv.fItem != NULL && lv.fInOuterFunc;
}


void
Scope::dumpDebug() const
{
  fprintf(stderr, "[------- Scope Dump [%p] ----------------------\n", this);
  dumpDebugImpl();
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
  return (it != fVisibility.end());
}


VizType
Scope::exportSymbolVisibility(const ScopeName& sym) const
{
  VizMap::const_iterator it = fVisibility.find(sym);
  if (it != fVisibility.end())
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


void
Scope::registerSymbolForExport(ScopeDomain domain, const String& sym,
                               VizType viz, bool isFinal)
{
  VisibilityPair vp;
  vp.fViz = viz;
  vp.fIsFinal = isFinal;
  fVisibility.insert(std::make_pair(ScopeName(domain, sym), vp));
}


VizType
Scope::reduceVizType(VizType in) const
{
  switch (in) {
  case kPrivate:
    return kPrivate;
  case kInner:
    return kPrivate;
  case kOuter:
    return kOuter;
  case kPublic:
    return kPublic;
  }
  assert(0);
  return kPrivate;
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
          if (reducedVizType != kPrivate)
            dstScope->registerSymbolForExport(fullKey.fDomain, fullKey.fName,
                                              reducedVizType, isFinal);
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
          if (reducedVizType != kPrivate)
            dstScope->registerSymbolForExport(fullKey.fDomain, fullKey.fName,
                                              reducedVizType, isFinal);
        }
      }
    }
  }
}


void
Scope::propagateImportedScopes(Scope* dstScope) const
{
  assert(this != dstScope);

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
    generics.push_back(Type::newTypeRef(String("Char"), true));
    TypeConstVector constraints;
    Type t0 = Type::newTypeRef(String("Foo"), generics, constraints, true);

    Ptr<Scope> s0 = new Scope(kScopeL_CompileUnit);
    Type t1 = s0->lookupType_unused(t0);
    // printf("%s\n", (const char*)StrHelper(t1.toString()));
    CHECK(t1.isDef());
  }
}

#endif  // #if defined(UNITTESTS)

