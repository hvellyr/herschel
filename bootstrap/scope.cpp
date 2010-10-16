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

Scope::Scope()
{ }


Scope::Scope(Scope* parent)
 : fParent(parent)
{
}


Scope*
Scope::parent() const
{
  return fParent;
}


void
Scope::registerScopeItem(const ScopeName& name, ScopeItem* item)
{
  assert(item != NULL);
  assert(lookupItemLocalImpl(SrcPos(), name, false, false) == NULL);

  ScopeName base(name.fDomain, heather::baseName(name.fName));
  String ns(heather::nsName(name.fName));

  NsScopeMap::iterator it = fMap.find(base);
  if (it != fMap.end())
    it->second.insert(std::make_pair(ns, item));
  else
    fMap[base].insert(std::make_pair(ns, item));
}


const Scope::ScopeItem*
Scope::lookupItemLocal(const SrcPos& srcpos,
                       const ScopeName& name, bool showError) const
{
  return lookupItemLocalImpl(srcpos, name, showError, true);
}


const Scope::ScopeItem*
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
        return it->second.begin()->second.obj();
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
        return vit->second.obj();
      }
    }
  }

  for (ImportedScope::const_iterator it = fImportedScopes.begin();
       it != fImportedScopes.end();
       it++)
  {
    // fprintf(stderr, "Search for '%s' in '%s'\n",
    //         (const char*)StrHelper(name), (const char*)StrHelper(it->first));
    const ScopeItem* si = it->second->lookupItemLocalImpl(srcpos, name,
                                                          showError,
                                                          doAutoMatch);
    if (si != NULL)
      return si;
  }


  return NULL;
}


const Scope::ScopeItem*
Scope::lookupItem(const SrcPos& srcpos,
                  const ScopeName& name, bool showError) const
{
  const Scope* scope = this;

  while (scope != NULL) {
    const ScopeItem* si = scope->lookupItemLocalImpl(srcpos, name,
                                                     showError, true);
    if (si != NULL)
      return si;
    scope = scope->parent();
  }

  return NULL;
}


bool
Scope::hasName(ScopeDomain domain, const String& name, SrcPos* srcpos) const
{
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(domain, name), false);
  if (si != NULL) {
    *srcpos = si->srcpos();
    return true;
  }
  return false;
}


bool
Scope::hasNameLocal(ScopeDomain domain, const String& name, SrcPos* srcpos,
                    bool doAutoMatch) const
{
  const ScopeItem* si = lookupItemLocalImpl(SrcPos(),
                                            ScopeName(domain, name), false,
                                            doAutoMatch);
  if (si != NULL) {
    *srcpos = si->srcpos();
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
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(kNormal, name),
                                   showAmbiguousSymDef);
  if (si != NULL && si->kind() == kScopeItem_type)
    return dynamic_cast<const TypeScopeItem*>(si)->type();

  return sInvalidType;
}


TypeUnit
Scope::lookupUnit(const String& name, bool showAmbiguousSymDef) const
{
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(kUnit, name),
                                   showAmbiguousSymDef);
  if (si != NULL && si->kind() == kScopeItem_unit)
    return dynamic_cast<const UnitScopeItem*>(si)->unit();

  return TypeUnit();
}


Type
Scope::normalizeType(const Type& type, const Type& refType) const
{
  Type baseType;
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


Type
Scope::lookupType(const Type& type) const
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
  const ScopeItem* si = lookupItem(srcpos,
                                   ScopeName(kNormal, name),
                                   showAmbiguousSymDef);
  if (si != NULL && si->kind() == kScopeItem_macro)
    return dynamic_cast<const MacroScopeItem*>(si)->macro();
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
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(kNormal, name),
                                   showAmbiguousSymDef);
  if (si != NULL && si->kind() == kScopeItem_function)
    return dynamic_cast<const NodeScopeItem*>(si)->node();
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
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(kNormal, name),
                                   showAmbiguousSymDef);
  if (si != NULL && si->kind() == kScopeItem_variable)
    return dynamic_cast<const NodeScopeItem*>(si)->node();
  return NULL;
}


const AptNode*
Scope::lookupVarOrFunc(const String& name, bool showAmbiguousSymDef) const
{
  const ScopeItem* si = lookupItem(SrcPos(),
                                   ScopeName(kNormal, name),
                                   showAmbiguousSymDef);
  if (si != NULL && ( si->kind() == kScopeItem_variable ||
                      si->kind() == kScopeItem_function ))
    return dynamic_cast<const NodeScopeItem*>(si)->node();
  return NULL;
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
