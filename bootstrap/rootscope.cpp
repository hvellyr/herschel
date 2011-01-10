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
heather::type::newRootScope(bool forUnitTests)
{
  Ptr<Scope> root = new Scope(kScopeL_CompileUnit);
  SrcPos sp;

  //-------- lang|Any
  root->registerType(sp, Names::kAnyTypeName, Type::newAny(true));

  //-------- lang|Object
  root->registerType(sp, Names::kObjectTypeName,
                     Type::newType(Names::kObjectTypeName, TypeVector(), Type()));


  //-------- lang|Bool
  root->registerType(sp, Names::kBoolTypeName,
                     Type::newType(Names::kBoolTypeName,
                                   TypeVector(),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  //-------- lang|Char
  root->registerType(sp, Names::kCharTypeName,
                     Type::newType(Names::kCharTypeName,
                                   TypeVector(),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));


  if (forUnitTests) {
    //------------------------------
    // Number types

    //-------- Number, Int, Real, Rational, Complex
    root->registerType(sp, Names::kNumberTypeName,
                       Type::newType(Names::kNumberTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kObjectTypeName, true)));

    root->registerType(sp, Names::kIntTypeName,
                       Type::newType(Names::kIntTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kNumberTypeName, true)));
  }


  //------------------------------
  // String and symbols
  root->registerType(sp, Names::kStringTypeName,
                     Type::newType(Names::kStringTypeName,
                                   TypeVector(),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  root->registerType(sp, Names::kKeywordTypeName,
                     Type::newType(Names::kKeywordTypeName,
                                   TypeVector(),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));


  //------------------------------
  // Other basic types and constants

  Type eofType = Type::newType(Names::kEofTypeName,
                               TypeVector(),
                               Type::newTypeRef(Names::kObjectTypeName, true));
  root->registerType(sp, Names::kEofTypeName, eofType);

  Type nilType = Type::newType(Names::kNilTypeName,
                               TypeVector(),
                               Type::newTypeRef(Names::kObjectTypeName, true));
  root->registerType(sp, Names::kNilTypeName, nilType);

  Type unspecifiedType = Type::newType(Names::kUnspecifiedTypeName,
                                       TypeVector(),
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


  //------------------------------ range
  // def class Range<T>
  root->registerType(sp, Names::kRangeTypeName,
                     Type::newType(Names::kRangeTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"),
                                                                  true, true)),
                                   Type()));


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

  //------------------------------ collections

  // Collection<T>
  // OrderedCollection<T> : Collection<T>
  // OrderedSliceable<Ordinal, T> : Sliceable<K, E>
  // Sequence<T> : (Collection<T>,
  //                OrderedCollection<T>, 
  //                OrderedSliceable<Ordinal, T>)

  root->registerType(sp, Names::kCollectionTypeName,
                     Type::newType(Names::kCollectionTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  root->registerType(sp, Names::kOrderedCollectionTypeName,
                     Type::newType(Names::kOrderedCollectionTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                   Type::newTypeRef(Names::kCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                                    newTypeConstVector(),
                                                    true)));

  root->registerType(sp, Names::kSequenceTypeName,
                     Type::newType(Names::kSequenceTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                   Type::newTypeRef(Names::kOrderedCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                                    newTypeConstVector(),
                                                    true)));

  root->registerType(sp, Names::kVectorTypeName,
                     Type::newType(Names::kVectorTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                   Type::newTypeRef(Names::kSequenceTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"), true, true)),
                                                    newTypeConstVector(),
                                                    true)));


  // Assoc<K, V>
  // AssocCollection<K, V> : Collection<Assoc<K, V>>
  // Map<K, V> : AssocCollection<K, V>

  root->registerType(sp, Names::kAssocTypeName,
                     Type::newType(Names::kAssocTypeName,
                                   newTypeVector(Type::newTypeRef(String("K"), true, true),
                                                 Type::newTypeRef(String("V"), true, true)),
                                   Type::newTypeRef(Names::kObjectTypeName, true)));

  root->registerType(
    sp, Names::kAssocCollectionTypeName,
    Type::newType(Names::kAssocCollectionTypeName,
                  newTypeVector(Type::newTypeRef(String("K"), true, true),
                                Type::newTypeRef(String("V"), true, true)),
                  Type::newTypeRef(
                    Names::kCollectionTypeName,
                    newTypeVector(Type::newTypeRef(
                                    Names::kAssocTypeName,
                                    newTypeVector(Type::newTypeRef(String("K"), true, true),
                                                  Type::newTypeRef(String("V"), true, true)),
                                    newTypeConstVector(),
                                    true)),
                    newTypeConstVector(),
                    true)));

  root->registerType(sp, Names::kMapTypeName,
                     Type::newType(Names::kMapTypeName,
                                   newTypeVector(Type::newTypeRef(String("K"), true, true),
                                                 Type::newTypeRef(String("V"), true, true)),
                                   Type::newTypeRef(Names::kAssocCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("K"), true, true),
                                                                  Type::newTypeRef(String("V"), true, true)),
                                                    newTypeConstVector(),
                                                    true)));

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

