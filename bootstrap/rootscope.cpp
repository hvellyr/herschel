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
#include "apt.h"


//----------------------------------------------------------------------------

using namespace heather;


Scope*
heather::type::newRootScope()
{
  Ptr<Scope> root = new Scope(kScopeL_CompileUnit);
  SrcPos sp;

  TypeVector generics;

  //-------- lang|Any
  root->registerType(sp, Type::kAnyTypeName, Type::newAny(true));

  //-------- lang|Object
  root->registerType(sp, Type::kObjectTypeName,
                     Type::newType(Type::kObjectTypeName, generics, Type()));

  //-------- lang|Bool
  root->registerType(sp, Type::kBoolTypeName,
                     Type::newType(Type::kBoolTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));


  //------------------------------
  // Number types

  //-------- Number, Int, Real, Rational, Complex
  root->registerType(sp, Type::kNumberTypeName,
                     Type::newType(Type::kNumberTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));

  root->registerType(sp, Type::kIntTypeName,
                     Type::newType(Type::kIntTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(sp, Type::kRealTypeName,
                     Type::newType(Type::kRealTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(sp, Type::kRationalTypeName,
                     Type::newType(Type::kRationalTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));

  root->registerType(sp, Type::kComplexTypeName,
                     Type::newType(Type::kComplexTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kNumberTypeName, true)));


  //------------------------------
  // String and symbols
  root->registerType(sp, Type::kStringTypeName,
                     Type::newType(Type::kStringTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));

  root->registerType(sp, Type::kKeywordTypeName,
                     Type::newType(Type::kKeywordTypeName,
                                   generics,
                                   Type::newTypeRef(Type::kObjectTypeName, true)));


  //------------------------------
  // Other basic types and constants

  Type eofType = Type::newType(Type::kEofTypeName,
                               generics,
                               Type::newTypeRef(Type::kObjectTypeName, true));
  root->registerType(sp, Type::kEofTypeName, eofType);

  Type nilType = Type::newType(Type::kNilTypeName,
                               generics,
                               Type::newTypeRef(Type::kObjectTypeName, true));
  root->registerType(sp, Type::kNilTypeName, nilType);

  Type unspecifiedType = Type::newType(Type::kUnspecifiedTypeName,
                                       generics,
                                       Type::newTypeRef(Type::kObjectTypeName, true));
  root->registerType(sp, Type::kUnspecifiedTypeName, unspecifiedType);


  Ptr<AptNode> eof = new VardefNode(sp, String("lang|eof"), kNormalVar,
                                    false, eofType,
                                    new ApplyNode(sp,
                                                  new SymbolNode(sp,
                                                                 Type::kEofTypeName)));
  root->registerVar(sp, String("lang|eof"), eof);


  Ptr<AptNode> nil = new VardefNode(sp, String("lang|nil"), kNormalVar,
                                    false, nilType,
                                    new ApplyNode(sp,
                                                  new SymbolNode(sp,
                                                                 Type::kNilTypeName)));
  root->registerVar(sp, String("lang|nil"), nil);


  Ptr<AptNode> unspecified = new VardefNode(sp, String("lang|unspecified"), kNormalVar,
                                            false, unspecifiedType,
                                            new ApplyNode(sp,
                                                          new SymbolNode(sp,
                                                                         Type::kUnspecifiedTypeName)));
  root->registerVar(sp, String("lang|unspecified"), unspecified);


  return root.release();
}

