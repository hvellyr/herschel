/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../predefined.h"
#include "../ptr.h"
#include "../rootscope.h"
#include "../scope.h"
#include "../srcpos.h"
#include "../str.h"
#include "../type.h"
#include "../typectx.h"
#include "../utils.h"

#include <iostream>

using namespace herschel;

namespace herschel
{
  std::ostream& operator<<(std::ostream& os, const TypeConstraint& constraint)
  {
    os << constraint.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const Type& type)
  {
    os << type.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const FunctionParameter& prm)
  {
    os << prm.toString();
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const FunctionSignature& sign)
  {
    os << sign.toString();
    return os;
  }
};


namespace
{
  // Test class tree:
  //
  // Obj <- Base     <- Medium  <- Top
  //     ^           <- Special <- Ultra
  //     |               |
  //     |               v
  //     \- Abstract <- Xyz
  std::shared_ptr<Scope> testScopeSetup()
  {
    auto scope = herschel::type::newRootScope(K(forUnitTests));

    scope->registerType(SrcPos(), String("Obj"),
                        Type::newType(String("Obj"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Base"),
                        Type::newType(String("Base"),
                                      TypeVector(),
                                      Type::newTypeRef("Obj")));

    scope->registerType(SrcPos(), String("Medium"),
                        Type::newType(String("Medium"),
                                      TypeVector(),
                                      Type::newTypeRef("Base")));
    scope->registerType(SrcPos(), String("Top"),
                        Type::newType(String("Top"),
                                      TypeVector(),
                                      Type::newTypeRef("Medium")));

    scope->registerType(SrcPos(), String("Abstract"),
                        Type::newType(String("Abstract"),
                                      TypeVector(),
                                      Type::newTypeRef("Obj")));
    scope->registerType(SrcPos(), String("Xyz"),
                        Type::newType(String("Xyz"),
                                      TypeVector(),
                                      Type::newTypeRef("Abstract")));

    TypeVector isa = vector_of(Type::newTypeRef("Base"))
                              (Type::newTypeRef("Xyz"));
    scope->registerType(SrcPos(), String("Special"),
                        Type::newType(String("Special"),
                                      TypeVector(),
                                      Type::newSeq(isa, K(isValue))));
    scope->registerType(SrcPos(), String("Ultra"),
                        Type::newType(String("Ultra"),
                                      TypeVector(),
                                      Type::newTypeRef("Special")));

    return scope;
  }

  // Test class tree:
  //
  // Obj <- Mappable<'K, 'E> <- Full(:Mappable<Abc, Def>)
  //     ^  ^                <- Partial<'Z>(:Mappable<Abc, 'Z>) <- Ultra(:Partial<Mno>)
  //     |  |
  //     |  \- OrdMap<'K, 'E> <- Multi(:OrdMap<Abc, Def>)
  //     |                       |
  //     \- Abc                  |
  //     \- Def                  |
  //     \- Mno <----------------/
  std::shared_ptr<Scope> testScopeSetupGenerics()
  {
    auto scope = herschel::type::newRootScope(K(forUnitTests));

    scope->registerType(SrcPos(), String("Obj"),
                        Type::newType(String("Obj"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Abc"),
                        Type::newType(String("Abc"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Def"),
                        Type::newType(String("Def"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Mno"),
                        Type::newType(String("Mno"), TypeVector(), Type()));

    scope->registerType(SrcPos(), String("Mappable"),
                        Type::newType(String("Mappable"),
                                      vector_of(Type::newTypeRef(String("K"), K(isopen), !K(isvalue)))
                                               (Type::newTypeRef(String("E"), K(isopen), !K(isvalue))),
                                      Type::newTypeRef("Obj")));
    scope->registerType(SrcPos(), String("OrdMap"),
                        Type::newType(String("OrdMap"),
                                      vector_of(Type::newTypeRef(String("K"), K(isopen), !K(isvalue)))
                                               (Type::newTypeRef(String("E"), K(isopen), !K(isvalue))),
                                      Type::newTypeRef(String("Mappable"),
                                                       vector_of(Type::newTypeRef(String("K"), K(isopen),
                                                                                  !K(isvalue)))
                                                                (Type::newTypeRef(String("E"), K(isopen),
                                                                                  !K(isvalue))),
                                                       !K(isvalue))));

    scope->registerType(SrcPos(), String("Full"),
                        Type::newType(String("Full"),
                                      TypeVector(),
                                      Type::newTypeRef(String("Mappable"),
                                                       vector_of(Type::newTypeRef("Abc"))
                                                                (Type::newTypeRef("Def")),
                                                       !K(isvalue))));

    scope->registerType(SrcPos(), String("Partial"),
                        Type::newType(String("Partial"),
                                      vector_of(Type::newTypeRef(String("Z"), K(isopen), !K(isvalue))),
                                      Type::newTypeRef(String("Mappable"),
                                                       vector_of(Type::newTypeRef("Abc"))
                                                                (Type::newTypeRef(String("Z"), K(isopen),
                                                                                  !K(isvalue))),
                                                       !K(isvalue))));

    scope->registerType(SrcPos(), String("Ultra"),
                        Type::newType(String("Ultra"),
                                      TypeVector(),
                                      Type::newTypeRef(String("Partial"),
                                                       vector_of(Type::newTypeRef("Mno")),
                                                       !K(isvalue))));

    scope->registerType(SrcPos(), String("Multi"),
                        Type::newType(String("Multi"),
                                      TypeVector(),
                                      Type::newSeq(
                                        vector_of(Type::newTypeRef("Mno"))
                                                 (Type::newTypeRef(String("OrdMap"),
                                                                   vector_of(Type::newTypeRef("Abc"))
                                                                   (Type::newTypeRef("Def")),
                                                                   !K(isvalue))),
                                        !K(isvalue))));

    return scope;
  }
} // end anon namespace


TEST_CASE("Is same for basic types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(Type::newTypeRef("Base"),
                               Type::newTypeRef("Base"),
                               *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isSameType(Type::newTypeRef("Xyz"),
                               Type::newTypeRef("Xyz"),
                               *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isSameType(Type::newTypeRef("Base"),
                                Type::newTypeRef("Medium"),
                                *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::isSameType(Type::newTypeRef("Base"),
                                Type::newTypeRef("Hello"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Is same for array types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newArray(Type::newTypeRef("Base"), 17, !K(isValue)),
            *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newArray(Type::newTypeRef("Xyz"), 17, !K(isValue)),
            *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(herschel::isSameType(
            Type::newArray(Type::newAny(K(isValue)), 5, K(isValue)),
            Type::newArray(Type::newAny(K(isValue)), 17, !K(isValue)),
            *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(
            Type::newArray(Type::newTypeRef("Base"), 5, K(isValue)),
            Type::newTypeRef("Base"),
            *scope, SrcPos(), !K(reportErrors)));
}


TEST_CASE("Is same for any type", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(Type::newAny(K(isValue)),
                               Type::newAny(K(isValue)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newAny(K(isValue)),
                                Type::newTypeRef("Medium"),
                                *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newTypeRef("Xyz"),
                                Type::newAny(K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));
}


TEST_CASE("Is same for union types", "[type]")
{
  auto scope = testScopeSetup();

  TypeVector union0 = vector_of(Type::newTypeRef("Xyz"))
                               (Type::newTypeRef("Medium"));

  TypeVector union1 = vector_of(Type::newTypeRef("Medium"))
                               (Type::newTypeRef("Xyz"));

  REQUIRE(herschel::isSameType(Type::newUnion(union0, K(isValue)),
                               Type::newUnion(union0, K(isValue)),
                               *scope, SrcPos(), K(reportErrors)));
  REQUIRE(!herschel::isSameType(Type::newUnion(union0, K(isValue)),
                                Type::newUnion(union1, K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));

  TypeVector union2 = vector_of(Type::newTypeRef("Medium"))
                               (Type::newTypeRef("Ultra"))
                               (Type::newTypeRef("Abstract"));

  REQUIRE(!herschel::isSameType(Type::newUnion(union0, K(isValue)),
                                Type::newUnion(union2, K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));
}


TEST_CASE("Is same for seq types", "[type]")
{
  auto scope = testScopeSetup();

  TypeVector seq0 = vector_of(Type::newTypeRef("Xyz"))
                             (Type::newTypeRef("Medium"));

  TypeVector seq1 = vector_of(Type::newTypeRef("Medium"))
                             (Type::newTypeRef("Xyz"));

  REQUIRE(herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                               Type::newSeq(seq0, K(isValue)),
                               *scope, SrcPos(), K(reportErrors)));
  REQUIRE(!herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                                Type::newSeq(seq1, K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));

  TypeVector seq2 = vector_of(Type::newTypeRef("Medium"))
                             (Type::newTypeRef("Ultra"))
                             (Type::newTypeRef("Abstract"));

  REQUIRE(!herschel::isSameType(Type::newSeq(seq0, K(isValue)),
                                Type::newSeq(seq2, K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));
}


TEST_CASE("Is same for measure types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                Type::newTypeRef("Xyz"),
                                                String("mk")),
                               Type::newMeasure(String("Maiko"),
                                                Type::newTypeRef("Xyz"),
                                                String("mk")),
                               *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("mk")),
                                Type::newTypeRef(String("Xyz"), K(isValue)),
                                *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newMeasure(String("Maiko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("mk")),
                                Type::newMeasure(String("Tomoko"),
                                                 Type::newTypeRef("Xyz"),
                                                 String("to")),
                                *scope, SrcPos(), !K(reportErrors)));
}


TEST_CASE("Is same for function types", "[type]")
{
  auto scope = testScopeSetup();

  FunctionParamVector params0;
  REQUIRE(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params0)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("foo"),
                                                    Type::newTypeRef("Xyz"),
                                                    params0)),
                                Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("bar"),
                                                    Type::newTypeRef("Abstract"),
                                                    params0)),
                                *scope, SrcPos(), !K(reportErrors)));

  FunctionParamVector params1 = vector_of(
    FunctionParameter(FunctionParameter::kParamPos, !K(isSpec),
                      String(), Type::newTypeRef("Medium")));
  REQUIRE(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("foo"),
                                                    Type::newTypeRef("Xyz"),
                                                    params1)),
                                Type::newFunction(
                                  FunctionSignature(!K(isGeneric), String("bar"),
                                                    Type::newTypeRef("Xyz"),
                                                    params0)),
                                *scope, SrcPos(), !K(reportErrors)));

  params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                      String("na"),
                                      Type::newTypeRef("Xyz")));
  params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                      String("nu"),
                                      Type::newTypeRef("Abstract")));
  params1.push_back(FunctionParameter(FunctionParameter::kParamRest, !K(isSpec),
                                      String("rest"),
                                      Type::newAny(K(isValue))));

  REQUIRE(herschel::isSameType(Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("foo"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               Type::newFunction(
                                 FunctionSignature(!K(isGeneric), String("bar"),
                                                   Type::newTypeRef("Xyz"),
                                                   params1)),
                               *scope, SrcPos(), !K(reportErrors)));
}


TEST_CASE("Is same for generics", "[type]")
{
  // Test class tree:
  //
  // Obj <- Mappable<'K, 'E> <- Full(:Mappable<Abc, Def>)
  //     ^                   <- Partial<'Z>(:Mappable<Abc, 'Z>) <- Ultra(:Partial<Mno>)
  //     \- Abc
  //     \- Def
  //     \- Mno
  auto scope = testScopeSetupGenerics();

  REQUIRE(!herschel::isSameType(Type::newTypeRef(String("Mappable"),
                                                 vector_of(Type::newTypeRef("Abc"))
                                                          (Type::newTypeRef("Def")),
                                                 !K(isvalue)),
                                Type::newTypeRef("Full"),
                                *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(herschel::isSameType(Type::newTypeRef(String("Mappable"),
                                                vector_of(Type::newTypeRef("Abc"))
                                                         (Type::newTypeRef("Def")),
                                                !K(isvalue)),
                               Type::newTypeRef(String("Mappable"),
                                                vector_of(Type::newTypeRef("Abc"))
                                                         (Type::newTypeRef("Def")),
                                                !K(isvalue)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::newTypeRef(String("Mappable"),
                                                 vector_of(Type::newTypeRef("Abc"))
                                                          (Type::newTypeRef("Def")),
                                                 !K(isvalue)),
                                Type::newTypeRef(String("Mappable"),
                                                 vector_of(Type::newTypeRef("Abc"))
                                                          (Type::newTypeRef("Mno")),
                                                 !K(isvalue)),
                                *scope, SrcPos(), !K(reportErrors)));
}
// TODO check generic types
// TODO check combinations of tests (arrays of generics, arrays of unions,
// sequences of generics and function types, etc.)


//----------------------------------------------------------------------------

// Test class tree:
//
// Obj <- Base     <- Medium  <- Top
//     ^           <- Special <- Ultra
//     |               |
//     |               v
//     \- Abstract <- Xyz

TEST_CASE("Inheritance for basic types", "[type]")
{
  auto scope = testScopeSetup();

  // a type A does not inherit itself
  REQUIRE(!herschel::inheritsFrom(Type::newTypeRef("Base"),
                                  Type::newTypeRef("Base"),
                                  *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::newTypeRef("Ultra"),
                                 Type::newTypeRef("Obj"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::newTypeRef("Special"),
                                 Type::newTypeRef("Base"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::newTypeRef("Special"),
                                 Type::newTypeRef("Abstract"),
                                 *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::inheritsFrom(Type::newTypeRef("Top"),
                                  Type::newTypeRef("Abstract"),
                                  *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::inheritsFrom(Type::newTypeRef("Xyz"),
                                  Type::newTypeRef("Base"),
                                  *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Inheritance for measure types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  *scope, SrcPos(), !K(reportError)));

  REQUIRE(herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                  Type::newTypeRef("Ultra"),
                                                  String("mk")),
                                 Type::newTypeRef(String("Abstract"), K(isValue)),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Xyz"),
                                                   String("mk")),
                                  Type::newTypeRef(String("Base"), K(isValue)),
                                  *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::inheritsFrom(Type::newMeasure(String("Maiko"),
                                                   Type::newTypeRef("Ultra"),
                                                   String("mk")),
                                  Type::newMeasure(String("Tomoko"),
                                                   Type::newTypeRef("Abstract"),
                                                   String("to")),
                                  *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Inheritance for generics", "[type]")
{
  // Test class tree:
  //
  // Obj <- Mappable<'K, 'E> <- Full(:Mappable<Abc, Def>)
  //     ^                   <- Partial<'Z>(:Mappable<Abc, 'Z>) <- Ultra(:Partial<Mno>)
  //     \- Abc
  //     \- Def
  //     \- Mno
  auto scope = testScopeSetupGenerics();

  REQUIRE(herschel::inheritsFrom(Type::newTypeRef("Full"),
                                 Type::newTypeRef(String("Mappable"),
                                                  vector_of(Type::newTypeRef(String("K"), K(isopen),
                                                                             !K(isvalue)))
                                                           (Type::newTypeRef(String("E"), K(isopen),
                                                                             !K(isvalue))),
                                                  !K(isvalue)),
                                 *scope, SrcPos(), !K(reportErrors)));
}


// Test class tree:
//
// Obj <- Base     <- Medium  <- Top
//     ^           <- Special <- Ultra
//     |               |
//     |               v
//     \- Abstract <- Xyz

TEST_CASE("Covariance for basic types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isCovariant(Type::newTypeRef("Base"),
                                Type::newTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::newTypeRef("Xyz"),
                                Type::newTypeRef("Xyz"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::newTypeRef("Medium"),
                                Type::newTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(Type::newTypeRef("Base"),
                                 Type::newTypeRef("Medium"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isContravariant(Type::newTypeRef("Base"),
                                    Type::newTypeRef("Medium"),
                                    *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isContravariant(Type::newTypeRef("Medium"),
                                     Type::newTypeRef("Base"),
                                     *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::isCovariant(Type::newTypeRef("Top"),
                                 Type::newTypeRef("Xyz"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isContravariant(Type::newTypeRef("Top"),
                                     Type::newTypeRef("Xyz"),
                                     *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isInvariant(Type::newTypeRef("Top"),
                                Type::newTypeRef("Xyz"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Covariance for multiple inheritance for basic types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isCovariant(Type::newTypeRef("Ultra"),
                                Type::newTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::newTypeRef("Ultra"),
                                Type::newTypeRef("Abstract"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Covariance for sliceable arrays", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Ultra"), K(isValue)),
                                               0, K(isValue)),
                                Type::newType(Names::kSliceableTypeName,
                                              vector_of(Type::newUInt32())
                                                       (Type::newTypeRef("Ultra")),
                                              Type()),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Special"), K(isValue)),
                                                0, K(isValue)),
                                 Type::newType(Names::kSliceableTypeName,
                                               vector_of(Type::newUInt32())
                                                        (Type::newTypeRef("Ultra")),
                                               Type()),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(Type::newArray(Type::newTypeRef(String("Ultra"), K(isValue)),
                                                0, K(isValue)),
                                 Type::newType(Names::kSliceableTypeName,
                                               vector_of(Type::newUInt32())
                                                        (Type::newTypeRef("Special")),
                                               Type()),
                                 *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Covariance for generics", "[type]")
{
  // Test class tree:
  //
  // Obj <- Mappable<'K, 'E> <- Full(:Mappable<Abc, Def>)
  //     ^                   <- Partial<'Z>(:Mappable<Abc, 'Z>) <- Ultra(:Partial<Mno>)
  //     \- Abc
  //     \- Def
  //     \- Mno
  auto scope = testScopeSetupGenerics();

  SECTION("Case 1")
  {
    REQUIRE(herschel::isCovariant(Type::newTypeRef("Full"),
                                  Type::newTypeRef(String("Mappable"),
                                                   vector_of(Type::newTypeRef(String("K"),
                                                                              K(isopen),
                                                                              !K(isvalue)))
                                                            (Type::newTypeRef(String("E"),
                                                                              K(isopen),
                                                                              !K(isvalue))),
                                                   !K(isvalue)),
                                  *scope, SrcPos(), !K(reportErrors)));
    REQUIRE(herschel::isCovariant(Type::newTypeRef("Ultra"),
                                  Type::newTypeRef(String("Mappable"),
                                                   vector_of(Type::newTypeRef(String("K"),
                                                                              K(isopen),
                                                                              !K(isvalue)))
                                                            (Type::newTypeRef(String("E"),
                                                                              K(isopen),
                                                                              !K(isvalue))),
                                                   !K(isvalue)),
                                  *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 2")
  {
    REQUIRE(herschel::isCovariant(Type::newTypeRef("Full"),
                                  Type::newTypeRef(String("Mappable"),
                                                   vector_of(Type::newTypeRef("Abc"))
                                                            (Type::newTypeRef("Def")),
                                                   !K(isvalue)),
                                  *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 3")
  {
    REQUIRE(herschel::isCovariant(Type::newTypeRef(String("Mappable"),
                                                   vector_of(Type::newTypeRef(String("K"),
                                                                              K(isopen),
                                                                              !K(isvalue)))
                                                            (Type::newTypeRef(String("E"),
                                                                              K(isopen),
                                                                              !K(isvalue))),
                                                   !K(isvalue)),
                                  Type::newTypeRef(String("Mappable"),
                                                   vector_of(Type::newTypeRef("Abc"))
                                                            (Type::newTypeRef("Def")),
                                                   !K(isvalue)),
                                  *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 4")
  {
    TypeCtx localCtx;

    REQUIRE(herschel::isCovariant(Type::newTypeRef("Multi"),
                                  Type::newTypeRef(String("OrdMap"),
                                                   vector_of(Type::newTypeRef(String("K"),
                                                                              K(isopen),
                                                                              !K(isvalue)))
                                                            (Type::newTypeRef(String("E"),
                                                                              K(isopen),
                                                                              !K(isvalue))),
                                                   !K(isvalue)),
                                  *scope, SrcPos(), !K(reportErrors)));
  }
}


//----------------------------------------------------------------------------

TEST_CASE("Type constraints construction", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                               Token(sp, kInt, 42));
  REQUIRE(kConstOp_equal == t0.constOp());
}


TEST_CASE("Type constraints equal constraint", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::newValue(kConstOp_equal,
                                               Token(sp, kInt, 42));
  TypeConstraint t1 = TypeConstraint::newValue(kConstOp_equal,
                                               Token(sp, kInt, 42));
  REQUIRE(kConstOp_equal == t0.constOp());
  REQUIRE(t0 == t1);
}


TEST_CASE("Type constraints and constraint", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::newValue(kConstOp_notEqual,
                                               Token(sp, kInt, 10));
  TypeConstraint t1 = TypeConstraint::newValue(kConstOp_notEqual,
                                               Token(sp, kInt, 21));
  TypeConstraint t2 = TypeConstraint::newAnd(t0, t1);
  TypeConstraint t3 = TypeConstraint::newOr(t0, t2);

  REQUIRE(kConstOp_and == t2.constOp());
  REQUIRE(t0 == t2.leftConstraint());
  REQUIRE(t1 == t2.rightConstraint());

  REQUIRE(kConstOp_or == t3.constOp());
  REQUIRE(t0 == t3.leftConstraint());
  REQUIRE(t2 == t3.rightConstraint());
}


TEST_CASE("Type constraints isa constraint", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::newType(kConstOp_isa,
                                              Type::newInt32());
  REQUIRE(kConstOp_isa == t0.constOp());
  REQUIRE(Type::newInt32() == t0.typeConstraint());
}


//----------------------------------------------------------------------------

TEST_CASE("Function parameter posParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::newPosParam(Type::newInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamPos == p0.kind());
}


TEST_CASE("Function parameter specParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::newSpecParam(Type::newInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamPos == p0.kind());
}


TEST_CASE("Function parameter namedParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::newNamedParam(String("abc"),
                                                          Type::newInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(String("abc") == p0.key());
  REQUIRE(FunctionParameter::kParamNamed == p0.kind());
}

TEST_CASE("Function parameter restParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::newRestParam(Type::newInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamRest == p0.kind());
}


//----------------------------------------------------------------------------

TEST_CASE("Function signature", "[type]")
{
  FunctionSignature fs0 = FunctionSignature(!K(isGeneric),
                                            String("abc"), Type::newInt32());

  FunctionParamVector params1 = vector_of
    (FunctionParameter::newSpecParam(Type::newString()))
    (FunctionParameter::newPosParam(Type::newInt32()))
    (FunctionParameter::newNamedParam(String("xyz"), Type::newFloat32()))
    (FunctionParameter::newRestParam(Type::newAny()));

  FunctionSignature fs1 = FunctionSignature(K(isGeneric),
                                            String("man"), Type::newInt32(),
                                            params1);

  REQUIRE(!fs0.isGeneric());
  REQUIRE(String("abc") == fs0.methodName());
  REQUIRE(Type::newInt32() == fs0.returnType());
  REQUIRE(fs0.parameters().empty());

  REQUIRE(fs1.isGeneric());
  REQUIRE(String("man") == fs1.methodName());
  REQUIRE(Type::newInt32() == fs1.returnType());
  REQUIRE((size_t)4 == fs1.parameters().size());
  REQUIRE(fs1.parameters()[0].type().isString());
  REQUIRE(FunctionParameter::kParamPos == fs1.parameters()[0].kind());
  REQUIRE(fs1.parameters()[0].isSpecialized());

  REQUIRE(fs1.parameters()[1].type().isInt32());
  REQUIRE(FunctionParameter::kParamPos == fs1.parameters()[1].kind());

  REQUIRE(fs1.parameters()[2].type().isFloat32());
  REQUIRE(FunctionParameter::kParamNamed == fs1.parameters()[2].kind());

  REQUIRE(fs1.parameters()[3].type().isAny());
  REQUIRE(FunctionParameter::kParamRest == fs1.parameters()[3].kind());
}


TEST_CASE("Function signature is open", "[type]")
{
  FunctionSignature fs0 = FunctionSignature(!K(isGeneric),
                                            String("abc"), Type::newInt32());
  REQUIRE(!fs0.isOpen());

  FunctionParamVector params1 = vector_of
    (FunctionParameter::newSpecParam(Type::newString()))
    (FunctionParameter::newPosParam(Type::newTypeRef(String("x"),
                                                     K(isOpen),
                                                     !K(isValue))));

  FunctionSignature fs1 = FunctionSignature(K(isGeneric), String("man"),
                                            Type::newTypeRef(String("y"),
                                                             K(isOpen),
                                                             !K(isValue)),
                                            params1);
  REQUIRE(fs1.isOpen());
}
// Test: isOpenSelf


//----------------------------------------------------------------------------

// Test class tree:
//
// Obj <- Mappable<'K, 'E> <- Full(:Mappable<Abc, Def>)
//     ^                   <- Partial<'Z>(:Mappable<Abc, 'Z>) <- Ultra(:Partial<Mno>)
//     |                   <- Multi(:Mappable<Abc, Def>)
//     \- Abc                  |
//     \- Def                  |
//     \- Mno <----------------/
TEST_CASE("Match generics for simple generics", "[type]")
{
  TypeCtx localCtx;
  auto scope = testScopeSetupGenerics();

  Type mapGen = Type::newTypeRef(String("Mappable"),
                                 vector_of(Type::newTypeRef(String("K"), K(isopen),
                                                            !K(isvalue)))
                                          (Type::newTypeRef(String("E"), K(isopen),
                                                            !K(isvalue))),
                                 !K(isvalue));
  Type mapCon = Type::newTypeRef(String("Mappable"),
                                 vector_of(Type::newTypeRef("Abc"))
                                          (Type::newTypeRef("Def")),
                                 !K(isvalue));
  REQUIRE(mapGen.matchGenerics(localCtx, mapCon, *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::newTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::newTypeRef("Def") == localCtx.lookupType(String("E")));
}


TEST_CASE("Match generic for inherited generics", "[type]")
{
  TypeCtx localCtx;
  auto scope = testScopeSetupGenerics();

  Type mappable = Type::newTypeRef(String("Mappable"),
                                   vector_of(Type::newTypeRef(String("K"), K(isopen),
                                                              !K(isvalue)))
                                            (Type::newTypeRef(String("E"), K(isopen),
                                                              !K(isvalue))),
                                   !K(isvalue));

  REQUIRE(mappable.matchGenerics(localCtx, Type::newTypeRef("Full"), *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::newTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::newTypeRef("Def") == localCtx.lookupType(String("E")));
}


TEST_CASE("Match generics for inherited generics with indirect multi inheritance", "[type]")
{
  TypeCtx localCtx;
  auto scope = testScopeSetupGenerics();

  Type mappable = Type::newTypeRef(String("Mappable"),
                                   vector_of(Type::newTypeRef(String("K"), K(isopen),
                                                              !K(isvalue)))
                                            (Type::newTypeRef(String("E"), K(isopen),
                                                              !K(isvalue))),
                                   !K(isvalue));

  REQUIRE(mappable.matchGenerics(localCtx, Type::newTypeRef("Multi"),
                                 *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::newTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::newTypeRef("Def") == localCtx.lookupType(String("E")));
}
