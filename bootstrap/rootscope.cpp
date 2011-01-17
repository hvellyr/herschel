/* -*-c++-*-

   This file is part of the herschel package

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

using namespace herschel;


Scope*
herschel::type::newRootScope(bool forUnitTests)
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
  // Sequence<T> : (Collection<T>,
  //                OrderedCollection<T>)

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


  return root.release();
}

