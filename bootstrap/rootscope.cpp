/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
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

  if (forUnitTests) {
    //-------- lang|Any
    root->registerType(sp, Names::kAnyTypeName, Type::newAny(K(isValue)));

    //-------- lang|Object
    root->registerType(sp, Names::kObjectTypeName,
                       Type::newType(Names::kObjectTypeName, TypeVector(), Type()));


    //-------- lang|Bool
    root->registerType(sp, Names::kBoolTypeName,
                       Type::newType(Names::kBoolTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kObjectTypeName,
                                                      K(isValue))));

    //-------- lang|Char
    root->registerType(sp, Names::kCharTypeName,
                       Type::newType(Names::kCharTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kObjectTypeName,
                                                      K(isValue))));

    //------------------------------
    // Number types

    //-------- Number, Int, Real, Rational, Complex
    root->registerType(sp, Names::kNumberTypeName,
                       Type::newType(Names::kNumberTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kObjectTypeName,
                                                      K(isValue))));

    root->registerType(sp, Names::kInt32TypeName,
                       Type::newType(Names::kInt32TypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kNumberTypeName,
                                                      K(isValue))));
    root->registerType(sp, Names::kUInt32TypeName,
                       Type::newType(Names::kUInt32TypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kNumberTypeName,
                                                      K(isValue))));
  }


  //------------------------------
  // String and symbols
  root->registerType(sp, Names::kStringTypeName,
                     Type::newType(Names::kStringTypeName,
                                   TypeVector(),
                                   Type::newTypeRef(Names::kObjectTypeName,
                                                    K(isValue))));

  if (forUnitTests) {
    root->registerType(sp, Names::kKeywordTypeName,
                       Type::newType(Names::kKeywordTypeName,
                                     TypeVector(),
                                     Type::newTypeRef(Names::kObjectTypeName,
                                                      K(isValue))));
  }

  //------------------------------
  // Other basic types and constants

  Type eofType = Type::newType(Names::kEofTypeName,
                               TypeVector(),
                               Type::newTypeRef(Names::kObjectTypeName,
                                                K(isValue)));
  root->registerType(sp, Names::kEofTypeName, eofType);

  Type nilType = Type::newType(Names::kNilTypeName,
                               TypeVector(),
                               Type::newTypeRef(Names::kObjectTypeName,
                                                K(isValue)));
  root->registerType(sp, Names::kNilTypeName, nilType);

  Type unspecifiedType = Type::newType(Names::kUnspecifiedTypeName,
                                       TypeVector(),
                                       Type::newTypeRef(Names::kObjectTypeName,
                                                        K(isValue)));
  root->registerType(sp, Names::kUnspecifiedTypeName, unspecifiedType);


  Ptr<AptNode> eof =
    new VardefNode(sp, String("lang|eof"), kNormalVar,
                   !K(isLocal), eofType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kEofTypeName)));
  root->registerVar(sp, String("lang|eof"), eof);


  Ptr<AptNode> nil =
    new VardefNode(sp, String("lang|nil"), kNormalVar,
                   !K(isLocal), nilType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kNilTypeName)));
  root->registerVar(sp, String("lang|nil"), nil);


  Ptr<AptNode> unspecified =
    new VardefNode(sp, String("lang|unspecified"), kNormalVar,
                   !K(isLocal), unspecifiedType,
                   new ApplyNode(sp,
                                 new SymbolNode(sp,
                                                Names::kUnspecifiedTypeName)));
  root->registerVar(sp, String("lang|unspecified"), unspecified);


  //------------------------------ builtin functions
  NodeList params;
  params.push_back(new ParamNode(sp, String(), String("r"),
                                 kPosArg,
                                 Type::newTypeRef(String("T"),
                                                  K(isOpen), K(isValue)),
                                 NULL));
  root->registerFunction(sp, Names::kLangReturn,
                         new FuncDefNode(sp,
                                         Names::kLangReturn,
                                         kFuncIsAbstract, // flags
                                         params,
                                         Type::newTypeRef(String("T"),
                                                          K(isOpen), K(isValue)),
                                         NULL));

  //------------------------------ collections

  // Collection<T>
  // OrderedCollection<T> : Collection<T>
  // Sequence<T> : (Collection<T>,
  //                OrderedCollection<T>)

  root->registerType(sp, Names::kCollectionTypeName,
                     Type::newType(Names::kCollectionTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kObjectTypeName, K(isValue))));

  root->registerType(sp, Names::kOrderedCollectionTypeName,
                     Type::newType(Names::kOrderedCollectionTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"),
                                                                                   K(isOpen), K(isValue))),
                                                    newTypeConstVector(),
                                                    K(isValue))));

  root->registerType(sp, Names::kSequenceTypeName,
                     Type::newType(Names::kSequenceTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kOrderedCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"),
                                                                                   K(isOpen), K(isValue))),
                                                    newTypeConstVector(),
                                                    K(isValue))));

  root->registerType(sp, Names::kVectorTypeName,
                     Type::newType(Names::kVectorTypeName,
                                   newTypeVector(Type::newTypeRef(String("T"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kSequenceTypeName,
                                                    newTypeVector(Type::newTypeRef(String("T"),
                                                                                   K(isOpen), K(isValue))),
                                                    newTypeConstVector(),
                                                    K(isValue))));


  // Assoc<K, V>
  // AssocCollection<K, V> : Collection<Assoc<K, V>>
  // Map<K, V> : AssocCollection<K, V>

  root->registerType(sp, Names::kAssocTypeName,
                     Type::newType(Names::kAssocTypeName,
                                   newTypeVector(Type::newTypeRef(String("K"),
                                                                  K(isOpen), K(isValue)),
                                                 Type::newTypeRef(String("V"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kObjectTypeName, K(isValue))));

  root->registerType(
    sp, Names::kAssocCollectionTypeName,
    Type::newType(Names::kAssocCollectionTypeName,
                  newTypeVector(Type::newTypeRef(String("K"), K(isOpen), K(isValue)),
                                Type::newTypeRef(String("V"), K(isOpen), K(isValue))),
                  Type::newTypeRef(
                    Names::kCollectionTypeName,
                    newTypeVector(Type::newTypeRef(
                                    Names::kAssocTypeName,
                                    newTypeVector(Type::newTypeRef(String("K"),
                                                                   K(isOpen), K(isValue)),
                                                  Type::newTypeRef(String("V"),
                                                                   K(isOpen), K(isValue))),
                                    newTypeConstVector(),
                                    K(isValue))),
                    newTypeConstVector(),
                    K(isValue))));

  root->registerType(sp, Names::kMapTypeName,
                     Type::newType(Names::kMapTypeName,
                                   newTypeVector(Type::newTypeRef(String("K"),
                                                                  K(isOpen), K(isValue)),
                                                 Type::newTypeRef(String("V"),
                                                                  K(isOpen), K(isValue))),
                                   Type::newTypeRef(Names::kAssocCollectionTypeName,
                                                    newTypeVector(Type::newTypeRef(String("K"),
                                                                                   K(isOpen), K(isValue)),
                                                                  Type::newTypeRef(String("V"),
                                                                                   K(isOpen), K(isValue))),
                                                    newTypeConstVector(),
                                                    K(isValue))));


  return root.release();
}

