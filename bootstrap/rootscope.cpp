/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "rootscope.hpp"

#include "ast.hpp"
#include "predefined.hpp"
#include "scope.hpp"
#include "srcpos.hpp"
#include "type.hpp"
#include "utils.hpp"


namespace herschel {


std::shared_ptr<Scope> type::newRootScope(bool forUnitTests)
{
  auto root = makeScope(kScopeL_CompileUnit);
  SrcPos sp;

  if (forUnitTests) {
    //-------- lang.Any
    root->registerType(sp, Names::kAnyTypeName, Type::makeAny(K(isValue)));

    //-------- lang.Object
    root->registerType(sp, Names::kObjectTypeName,
                       Type::makeType(Names::kObjectTypeName, TypeVector(), Type()));


    //-------- lang.Bool
    root->registerType(
        sp, Names::kBoolTypeName,
        Type::makeType(Names::kBoolTypeName, TypeVector(),
                       Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

    //-------- lang.Char
    root->registerType(
        sp, Names::kCharTypeName,
        Type::makeType(Names::kCharTypeName, TypeVector(),
                       Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

    //------------------------------
    // Number types

    //-------- Number, Int, Real, Rational, Complex
    root->registerType(
        sp, Names::kNumberTypeName,
        Type::makeType(Names::kNumberTypeName, TypeVector(),
                       Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

    root->registerType(
        sp, Names::kInt32TypeName,
        Type::makeType(Names::kInt32TypeName, TypeVector(),
                       Type::makeTypeRef(Names::kNumberTypeName, K(isValue))));
    root->registerType(
        sp, Names::kUInt32TypeName,
        Type::makeType(Names::kUInt32TypeName, TypeVector(),
                       Type::makeTypeRef(Names::kNumberTypeName, K(isValue))));
  }


  //------------------------------
  if (forUnitTests) {
    // String and symbols
    root->registerType(
        sp, Names::kStringTypeName,
        Type::makeType(Names::kStringTypeName, TypeVector(),
                       Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

    root->registerType(
        sp, Names::kKeywordTypeName,
        Type::makeType(Names::kKeywordTypeName, TypeVector(),
                       Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));
  }

  //------------------------------
  // Other basic types and constants

  auto eofType = Type::makeType(Names::kEofTypeName, TypeVector(),
                                Type::makeTypeRef(Names::kObjectTypeName, K(isValue)));
  root->registerType(sp, Names::kEofTypeName, eofType);

  auto nilType = Type::makeType(Names::kNilTypeName, TypeVector(),
                                Type::makeTypeRef(Names::kObjectTypeName, K(isValue)));
  root->registerType(sp, Names::kNilTypeName, nilType);

  auto unspecifiedType =
      Type::makeType(Names::kUnspecifiedTypeName, TypeVector(),
                     Type::makeTypeRef(Names::kObjectTypeName, K(isValue)));
  root->registerType(sp, Names::kUnspecifiedTypeName, unspecifiedType);


  auto eof = makeVardefNode(sp, String(".lang.eof"), kNormalVar, !K(isLocal), eofType,
                            makeApplyNode(sp, makeSymbolNode(sp, Names::kEofTypeName)));
  root->registerVar(sp, String(".lang.eof"), eof);


  auto nil = makeVardefNode(sp, String(".lang.nil"), kNormalVar, !K(isLocal), nilType,
                            makeApplyNode(sp, makeSymbolNode(sp, Names::kNilTypeName)));
  root->registerVar(sp, String(".lang.nil"), nil);


  auto unspecified = makeVardefNode(
      sp, String(".lang.unspecified"), kNormalVar, !K(isLocal), unspecifiedType,
      makeApplyNode(sp, makeSymbolNode(sp, Names::kUnspecifiedTypeName)));
  root->registerVar(sp, String(".lang.unspecified"), unspecified);


  //------------------------------ builtin functions
  auto params = makeVector<std::shared_ptr<AstNode>>(
      makeParamNode(sp, String(), String("r"), kPosArg,
                    Type::makeTypeRef(String("T"), K(isOpen), K(isValue)), nullptr));
  root->registerFunction(
      sp, Names::kLangReturn,
      makeFuncDefNode(sp, Names::kLangReturn,
                      kFuncIsAbstract,  // flags
                      params, Type::makeTypeRef(String("T"), K(isOpen), K(isValue)),
                      nullptr));

  //------------------------------ collections

  // Collection<T>
  // OrderedCollection<T> : Collection<T>
  // Sequence<T> : (Collection<T>,
  //                OrderedCollection<T>)

  root->registerType(
      sp, Names::kCollectionTypeName,
      Type::makeType(Names::kCollectionTypeName,
                     makeVector(Type::makeTypeRef(String("T"), K(isOpen), K(isValue))),
                     Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

  root->registerType(
      sp, Names::kOrderedCollectionTypeName,
      Type::makeType(Names::kOrderedCollectionTypeName,
                     makeVector(Type::makeTypeRef(String("T"), K(isOpen), K(isValue))),
                     Type::makeTypeRef(Names::kCollectionTypeName,
                                       makeVector(Type::makeTypeRef(
                                           String("T"), K(isOpen), K(isValue))),
                                       TypeConstVector(), K(isValue))));

  root->registerType(
      sp, Names::kSequenceTypeName,
      Type::makeType(Names::kSequenceTypeName,
                     makeVector(Type::makeTypeRef(String("T"), K(isOpen), K(isValue))),
                     Type::makeTypeRef(Names::kOrderedCollectionTypeName,
                                       makeVector(Type::makeTypeRef(
                                           String("T"), K(isOpen), K(isValue))),
                                       TypeConstVector(), K(isValue))));

  root->registerType(
      sp, Names::kVectorTypeName,
      Type::makeType(Names::kVectorTypeName,
                     makeVector(Type::makeTypeRef(String("T"), K(isOpen), K(isValue))),
                     Type::makeTypeRef(Names::kSequenceTypeName,
                                       makeVector(Type::makeTypeRef(
                                           String("T"), K(isOpen), K(isValue))),
                                       TypeConstVector(), K(isValue))));


  // Assoc<K, V>
  // AssocCollection<K, V> : Collection<Assoc<K, V>>
  // Map<K, V> : AssocCollection<K, V>

  root->registerType(
      sp, Names::kAssocTypeName,
      Type::makeType(Names::kAssocTypeName,
                     makeVector(Type::makeTypeRef(String("K"), K(isOpen), K(isValue)),
                                Type::makeTypeRef(String("V"), K(isOpen), K(isValue))),
                     Type::makeTypeRef(Names::kObjectTypeName, K(isValue))));

  root->registerType(
      sp, Names::kAssocCollectionTypeName,
      Type::makeType(
          Names::kAssocCollectionTypeName,
          makeVector(Type::makeTypeRef(String("K"), K(isOpen), K(isValue)),
                     Type::makeTypeRef(String("V"), K(isOpen), K(isValue))),
          Type::makeTypeRef(
              Names::kCollectionTypeName,
              makeVector(Type::makeTypeRef(
                  Names::kAssocTypeName,
                  makeVector(Type::makeTypeRef(String("K"), K(isOpen), K(isValue)),
                             Type::makeTypeRef(String("V"), K(isOpen), K(isValue))),
                  TypeConstVector(), K(isValue))),
              TypeConstVector(), K(isValue))));

  root->registerType(
      sp, Names::kMapTypeName,
      Type::makeType(
          Names::kMapTypeName,
          makeVector(Type::makeTypeRef(String("K"), K(isOpen), K(isValue)),
                     Type::makeTypeRef(String("V"), K(isOpen), K(isValue))),
          Type::makeTypeRef(
              Names::kAssocCollectionTypeName,
              makeVector(Type::makeTypeRef(String("K"), K(isOpen), K(isValue)),
                         Type::makeTypeRef(String("V"), K(isOpen), K(isValue))),
              TypeConstVector(), K(isValue))));

  return root;
}

}  // namespace herschel
