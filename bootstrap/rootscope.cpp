/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "ptr.h"
#include "rootscope.h"
#include "scope.h"
#include "srcpos.h"
#include "type.h"


//----------------------------------------------------------------------------

using namespace heather;


Scope*
heather::type::newRootScope()
{
  Ptr<Scope> root = new Scope(kScopeL_CompileUnit);

  TypeVector generics;

  //-------- lang|Any
  root->registerType(SrcPos(), Type::kAnyTypeName, Type::newAny(true));

  //-------- lang|Object
  root->registerType(SrcPos(), Type::kObjectTypeName,
                     Type::newType(Type::kObjectTypeName, generics, Type()));

  //-------- lang|Bool
  root->registerType(SrcPos(), Type::kBoolTypeName,
                     Type::newType(Type::kBoolTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));


  //------------------------------
  // Number types

  //-------- Number, Int, Real, Rational, Complex
  root->registerType(SrcPos(), Type::kNumberTypeName,
                     Type::newType(Type::kNumberTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));

  root->registerType(SrcPos(), Type::kIntTypeName,
                     Type::newType(Type::kIntTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(SrcPos(), Type::kRealTypeName,
                     Type::newType(Type::kRealTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(SrcPos(), Type::kRationalTypeName,
                     Type::newType(Type::kRationalTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(SrcPos(), Type::kComplexTypeName,
                     Type::newType(Type::kComplexTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));


  //------------------------------
  // String and symbols
  root->registerType(SrcPos(), Type::kStringTypeName,
                     Type::newType(Type::kStringTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));

  root->registerType(SrcPos(), Type::kKeywordTypeName,
                     Type::newType(Type::kKeywordTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));


  return root.release();
}

