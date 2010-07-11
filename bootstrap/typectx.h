/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_typectx_h
#define bootstrap_typectx_h

#include "common.h"

#include <map>

#include "ptr.h"
#include "exception.h"
#include "refcountable.h"
#include "str.h"
#include "type.h"


namespace heather
{
  class TypeRefMatchException : public Exception
  {
  public:
    TypeRefMatchException(const Type& type, const String& str)
      : Exception(str)
    { }
  };


  class TypeConstraintsConflictException : public Exception
  {
  public:
    TypeConstraintsConflictException(const Type& type, const String& str)
      : Exception(str)
    { }
  };


  class TypeCtx : public RefCountable
  {
  public:
    TypeCtx();
    TypeCtx(TypeCtx* parent);

    TypeCtx* parent() const;

    void registerType(const String& name, const Type& type);

    bool hasType(const String& name) const;

    const Type& lookupType(const String& name) const;

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

  private:
    const Type& lookupTypeLocal(const String& name) const;

    //-------- data members

    typedef std::map<String, Type> TypeMap;
    
    TypeMap      fMap;
    Ptr<TypeCtx> fParent;
  };


  class TypeCtxHelper
  {
  public:
    TypeCtxHelper(Ptr<TypeCtx>& typeCtx)
      : fTypeCtxLoc(typeCtx)
    {
      fTypeCtxLoc = new TypeCtx(fTypeCtxLoc);
    }

    ~TypeCtxHelper()
    {
      fTypeCtxLoc = fTypeCtxLoc->parent();
    }

  private:
    Ptr<TypeCtx>& fTypeCtxLoc;
  };
};                              // namespace

#endif                          // bootstrap_typectx_h
