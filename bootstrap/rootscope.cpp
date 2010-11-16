/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"
#include "predefined.h"
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
  SrcPos sp;

  TypeVector generics;

  //-------- lang|Any
  root->registerType(sp, Names::kAnyTypeName, Type::newAny(true));

  //-------- lang|Object
  root->registerType(sp, Names::kObjectTypeName,
                     Type::newType(Names::kObjectTypeName, generics, Type()));

  //-------- lang|Bool
  root->registerType(sp, Names::kBoolTypeName,
                     Type::newType(Names::kBoolTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  //-------- lang|Char
  root->registerType(sp, Names::kCharTypeName,
                     Type::newType(Names::kCharTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kObjectTypeName, true)));


  //------------------------------
  // Number types

  //-------- Number, Int, Real, Rational, Complex
  root->registerType(sp, Names::kNumberTypeName,
                     Type::newType(Names::kNumberTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  root->registerType(sp, Names::kIntTypeName,
                     Type::newType(Names::kIntTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kNumberTypeName, true)));

  root->registerType(sp, Names::kRealTypeName,
                     Type::newType(Names::kRealTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kNumberTypeName, true)));

  root->registerType(sp, Names::kRationalTypeName,
                     Type::newType(Names::kRationalTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kNumberTypeName, true)));

  root->registerType(sp, Names::kComplexTypeName,
                     Type::newType(Names::kComplexTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kNumberTypeName, true)));

  root->registerType(sp, Names::kInt32TypeName,
                     Type::newType(Names::kInt32TypeName,
                                   generics,
                                   Type::newTypeRef(Names::kNumberTypeName, true)));

  //------------------------------
  // String and symbols
  root->registerType(sp, Names::kStringTypeName,
                     Type::newType(Names::kStringTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  root->registerType(sp, Names::kKeywordTypeName,
                     Type::newType(Names::kKeywordTypeName,
                                   generics,
                                   Type::newTypeRef(Names::kObjectTypeName, true)));


  //------------------------------
  // Other basic types and constants

  Type eofType = Type::newType(Names::kEofTypeName,
                               generics,
                               Type::newTypeRef(Names::kObjectTypeName, true));
  root->registerType(sp, Names::kEofTypeName, eofType);

  Type nilType = Type::newType(Names::kNilTypeName,
                               generics,
                               Type::newTypeRef(Names::kObjectTypeName, true));
  root->registerType(sp, Names::kNilTypeName, nilType);

  Type unspecifiedType = Type::newType(Names::kUnspecifiedTypeName,
                                       generics,
                                       Type::newTypeRef(Names::kObjectTypeName, true));
  root->registerType(sp, Names::kUnspecifiedTypeName, unspecifiedType);


  Ptr<AptNode> eof =
    new VardefNode(sp, String("lang|eof"), kNormalVar,
                   false, eofType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kEofTypeName)));
  root->registerVar(sp, String("lang|eof"), eof);


  Ptr<AptNode> nil =
    new VardefNode(sp, String("lang|nil"), kNormalVar,
                   false, nilType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kNilTypeName)));
  root->registerVar(sp, String("lang|nil"), nil);


  Ptr<AptNode> unspecified =
    new VardefNode(sp, String("lang|unspecified"), kNormalVar,
                   false, unspecifiedType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kUnspecifiedTypeName)));
  root->registerVar(sp, String("lang|unspecified"), unspecified);


  //------------------------------ builtin functions
  NodeList params;
  params.push_back(new ParamNode(sp, String(), String("r"),
                                 kPosArg,
                                 Type::newTypeRef(String("T"), true, true),
                                 NULL));
  root->registerFunction(sp, Names::kLangReturn,
                         new FuncDefNode(sp,
                                         Names::kLangReturn,
                                         kFuncIsAbstract, // flags
                                         params,
                                         Type::newTypeRef(String("T"), true, true),
                                         NULL));

  //------------------------------ slicing

  {
    // def type Sliceable<K, E>
    TypeVector generics;
    generics.push_back(Type::newTypeRef(String("K"), true, true));
    generics.push_back(Type::newTypeRef(String("E"), true, true));

    root->registerType(sp, Names::kSliceableTypeName,
                       Type::newType(Names::kSliceableTypeName,
                                     generics, Type()));
  }
  Type sliceableRef;
  {
    TypeVector refGenerics;
    refGenerics.push_back(Type::newTypeRef(String("K"), true, true));
    refGenerics.push_back(Type::newTypeRef(String("E"), true, true));

    TypeConstVector refConstraints;
    sliceableRef = Type::newTypeRef(Names::kSliceableTypeName,
                                    refGenerics, refConstraints, true);
  }

  {
    // def type Sliceable!<K, E> : Sliceable<K, E>
    TypeVector generics;
    generics.push_back(Type::newTypeRef(String("K"), true, true));
    generics.push_back(Type::newTypeRef(String("E"), true, true));

    root->registerType(sp, Names::kSliceableXTypeName,
                       Type::newType(Names::kSliceableXTypeName,
                                     generics, sliceableRef));
  }
  Type sliceableXRef;
  Type refSliceableXRef;
  {
    TypeVector refGenerics;
    refGenerics.push_back(Type::newTypeRef(String("K"), true, true));
    refGenerics.push_back(Type::newTypeRef(String("E"), true, true));

    TypeConstVector refConstraints;
    sliceableXRef = Type::newTypeRef(Names::kSliceableXTypeName,
                                    refGenerics, refConstraints, true);
    refSliceableXRef = Type::newTypeRef(Names::kSliceableXTypeName,
                                        refGenerics, refConstraints, false);
  }

  {
    // def generic slice(obj @ 'S, key @ K) : 'E
    //   where S isa Sliceable<K, E> ...

    NodeList params;
    params.push_back(new ParamNode(sp, String(), String("obj"),
                                   kPosArg, sliceableRef, NULL));
    params.push_back(new ParamNode(sp, String(), String("key"),
                                   kPosArg,
                                   Type::newTypeRef(String("K"), true, true),
                                   NULL));
    root->registerFunction(sp, Names::kLangSlice,
                           new FuncDefNode(sp,
                                           Names::kLangSlice,
                                           kFuncIsGeneric | kFuncIsAbstract,
                                           params,
                                           Type::newTypeRef(String("E"), true, true),
                                           NULL));
  }

  {
    // def generic slice!(obj @ ^'S, key @ K, value @ E) : ^'S
    //   where S isa Sliceable!<K, E> ...

    NodeList params;
    params.push_back(new ParamNode(sp, String(), String("obj"),
                                   kPosArg, refSliceableXRef, NULL));
    params.push_back(new ParamNode(sp, String(), String("key"),
                                   kPosArg,
                                   Type::newTypeRef(String("K"), true, true),
                                   NULL));
    params.push_back(new ParamNode(sp, String(), String("value"),
                                   kPosArg,
                                   Type::newTypeRef(String("E"), true, true),
                                   NULL));
    root->registerFunction(sp, Names::kLangSliceX,
                           new FuncDefNode(sp,
                                           Names::kLangSliceX,
                                           kFuncIsGeneric | kFuncIsAbstract,
                                           params,
                                           refSliceableXRef,
                                           NULL));
  }

  return root.release();
}

