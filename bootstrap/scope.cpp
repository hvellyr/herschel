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

    Type fType;
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
Scope::registerScopeItem(const String& name, ScopeItem* item)
{
  assert(item != NULL);
  assert(fMap.find(name) == fMap.end());

  fMap.insert(std::make_pair(name, item));
}


const Scope::ScopeItem*
Scope::lookupItemLocal(const String& name) const
{
  ScopeMap::const_iterator it = fMap.find(name);
  if (it != fMap.end())
    return it->second.obj();

  return NULL;
}


const Scope::ScopeItem*
Scope::lookupItem(const String& name) const
{
  const Scope* scope = this;

  while (scope != NULL) {
    const ScopeItem* si = scope->lookupItemLocal(name);
    if (si != NULL)
      return si;
    scope = scope->parent();
  }

  return NULL;
}


bool
Scope::hasName(const String& name, SrcPos* srcpos) const
{
  const ScopeItem* si = lookupItem(name);
  if (si != NULL) {
    *srcpos = si->srcpos();
    return true;
  }
  return false;
}


bool
Scope::hasNameLocal(const String& name, SrcPos* srcpos) const
{
  const ScopeItem* si = lookupItemLocal(name);
  if (si != NULL) {
    *srcpos = si->srcpos();
    return true;
  }
  return false;
}


bool
Scope::checkForRedefinition(const SrcPos& srcpos,
                            const String& sym) const
{
  SrcPos firstSrcpos;
  if (hasNameLocal(sym, &firstSrcpos)) {
    errorf(srcpos, E_Redefinition,
           "Redefinition of '%s'.", (const char*)StrHelper(sym));
    errorf(firstSrcpos, E_Redefinition,
           "'%s' previously defined here.",
           (const char*)StrHelper(sym));
    return true;
  }

  return false;
}


//..........................................................................

void
Scope::registerType(const SrcPos& srcpos,
                    const String& name, const Type& type)
{
  assert(!type.isArray());
  assert(type.isDef());

  registerScopeItem(name, new TypeScopeItem(srcpos, type));
}


const Type&
Scope::lookupType(const String& name) const
{
  const ScopeItem* si = lookupItem(name);
  if (si != NULL && si->kind() == kScopeItem_type)
    return dynamic_cast<const TypeScopeItem*>(si)->type();

  return sInvalidType;
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
    Type fullType = lookupType(baseType.typeName());
    if (fullType.isDef())
      return normalizeType(fullType, type);
  }

  return type;
}


//..............................................................................

void
Scope::registerMacro(const SrcPos& srcpos,
                     const String& name, Macro* macro)
{
  registerScopeItem(name, new MacroScopeItem(srcpos, macro));
}


const Macro*
Scope::lookupMacro(const String& name) const
{
  const ScopeItem* si = lookupItem(name);
  if (si != NULL && si->kind() == kScopeItem_macro)
    return dynamic_cast<const MacroScopeItem*>(si)->macro();
  return NULL;
}


//............................................................................

void
Scope::registerFunction(const SrcPos& srcpos,
                        const String& name, AptNode* node)
{
  registerScopeItem(name,
                    new NodeScopeItem(srcpos, kScopeItem_function, node));
}


const AptNode*
Scope::lookupFunction(const String& name) const
{
  const ScopeItem* si = lookupItem(name);
  if (si != NULL && si->kind() == kScopeItem_function)
    return dynamic_cast<const NodeScopeItem*>(si)->node();
  return NULL;
}


//............................................................................

void
Scope::registerVar(const SrcPos& srcpos,
                   const String& name, AptNode* node)
{
  registerScopeItem(name,
                    new NodeScopeItem(srcpos, kScopeItem_variable, node));
}


const AptNode*
Scope::lookupVar(const String& name) const
{
  const ScopeItem* si = lookupItem(name);
  if (si != NULL && si->kind() == kScopeItem_variable)
    return dynamic_cast<const NodeScopeItem*>(si)->node();
  return NULL;
}
