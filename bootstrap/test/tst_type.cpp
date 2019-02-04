/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2015 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "catch/catch.hpp"

#include "../predefined.hpp"
#include "../rootscope.hpp"
#include "../scope.hpp"
#include "../srcpos.hpp"
#include "../str.hpp"
#include "../type.hpp"
#include "../typectx.hpp"
#include "../utils.hpp"

#include <iostream>


namespace herschel {
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


namespace {
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
                        Type::makeType(String("Obj"), TypeVector(), Type()));
    scope->registerType(
        SrcPos(), String("Base"),
        Type::makeType(String("Base"), TypeVector(), Type::makeTypeRef("Obj")));

    scope->registerType(
        SrcPos(), String("Medium"),
        Type::makeType(String("Medium"), TypeVector(), Type::makeTypeRef("Base")));
    scope->registerType(
        SrcPos(), String("Top"),
        Type::makeType(String("Top"), TypeVector(), Type::makeTypeRef("Medium")));

    scope->registerType(
        SrcPos(), String("Abstract"),
        Type::makeType(String("Abstract"), TypeVector(), Type::makeTypeRef("Obj")));
    scope->registerType(
        SrcPos(), String("Xyz"),
        Type::makeType(String("Xyz"), TypeVector(), Type::makeTypeRef("Abstract")));

    TypeVector isa = makeVector(Type::makeTypeRef("Base"), Type::makeTypeRef("Xyz"));
    scope->registerType(SrcPos(), String("Special"),
                        Type::makeType(String("Special"), TypeVector(),
                                       Type::makeIntersection(isa, K(isValue))));
    scope->registerType(
        SrcPos(), String("Ultra"),
        Type::makeType(String("Ultra"), TypeVector(), Type::makeTypeRef("Special")));

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
                        Type::makeType(String("Obj"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Abc"),
                        Type::makeType(String("Abc"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Def"),
                        Type::makeType(String("Def"), TypeVector(), Type()));
    scope->registerType(SrcPos(), String("Mno"),
                        Type::makeType(String("Mno"), TypeVector(), Type()));

    scope->registerType(
        SrcPos(), String("Mappable"),
        Type::makeType(String("Mappable"),
                       makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                                  Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
                       Type::makeTypeRef("Obj")));
    scope->registerType(
        SrcPos(), String("OrdMap"),
        Type::makeType(
            String("OrdMap"),
            makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                       Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
            Type::makeTypeRef(
                String("Mappable"),
                makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                           Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
                !K(isvalue))));

    scope->registerType(
        SrcPos(), String("Full"),
        Type::makeType(String("Full"), TypeVector(),
                       Type::makeTypeRef(
                           String("Mappable"),
                           makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                           !K(isvalue))));

    scope->registerType(
        SrcPos(), String("Partial"),
        Type::makeType(
            String("Partial"),
            makeVector(Type::makeTypeRef(String("Z"), K(isopen), !K(isvalue))),
            Type::makeTypeRef(
                String("Mappable"),
                makeVector(Type::makeTypeRef("Abc"),
                           Type::makeTypeRef(String("Z"), K(isopen), !K(isvalue))),
                !K(isvalue))));

    scope->registerType(
        SrcPos(), String("Ultra"),
        Type::makeType(String("Ultra"), TypeVector(),
                       Type::makeTypeRef(String("Partial"),
                                         makeVector(Type::makeTypeRef("Mno")),
                                         !K(isvalue))));

    scope->registerType(
        SrcPos(), String("Multi"),
        Type::makeType(
            String("Multi"), TypeVector(),
            Type::makeIntersection(
                makeVector(Type::makeTypeRef("Mno"),
                           Type::makeTypeRef(String("OrdMap"),
                                             makeVector(Type::makeTypeRef("Abc"),
                                                        Type::makeTypeRef("Def")),
                                             !K(isvalue))),
                !K(isvalue))));

    return scope;
  }
}  // namespace


TEST_CASE("Is same for basic types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(Type::makeTypeRef("Base"), Type::makeTypeRef("Base"),
                               *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isSameType(Type::makeTypeRef("Xyz"), Type::makeTypeRef("Xyz"), *scope,
                               SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isSameType(Type::makeTypeRef("Base"), Type::makeTypeRef("Medium"),
                                *scope, SrcPos(), !K(reportError)));

  REQUIRE(!herschel::isSameType(Type::makeTypeRef("Base"), Type::makeTypeRef("Hello"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Is same for array types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(
      herschel::isSameType(Type::makeArray(Type::makeTypeRef("Base"), 5, K(isValue)),
                           Type::makeArray(Type::makeTypeRef("Base"), 17, !K(isValue)),
                           *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(
      !herschel::isSameType(Type::makeArray(Type::makeTypeRef("Base"), 5, K(isValue)),
                            Type::makeArray(Type::makeTypeRef("Xyz"), 17, !K(isValue)),
                            *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(
      herschel::isSameType(Type::makeArray(Type::makeAny(K(isValue)), 5, K(isValue)),
                           Type::makeArray(Type::makeAny(K(isValue)), 17, !K(isValue)),
                           *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::makeArray(Type::makeTypeRef("Base"), 5, K(isValue)),
                                Type::makeTypeRef("Base"), *scope, SrcPos(),
                                !K(reportErrors)));
}


TEST_CASE("Is same for any type", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isSameType(Type::makeAny(K(isValue)), Type::makeAny(K(isValue)),
                               *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::makeAny(K(isValue)), Type::makeTypeRef("Medium"),
                                *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(Type::makeTypeRef("Xyz"), Type::makeAny(K(isValue)),
                                *scope, SrcPos(), K(reportErrors)));
}


TEST_CASE("Is same for union types", "[type]")
{
  auto scope = testScopeSetup();

  TypeVector union0 = makeVector(Type::makeTypeRef("Xyz"), Type::makeTypeRef("Medium"));

  TypeVector union1 = makeVector(Type::makeTypeRef("Medium"), Type::makeTypeRef("Xyz"));

  REQUIRE(herschel::isSameType(Type::makeUnion(union0, K(isValue)),
                               Type::makeUnion(union0, K(isValue)), *scope, SrcPos(),
                               K(reportErrors)));
  REQUIRE(!herschel::isSameType(Type::makeUnion(union0, K(isValue)),
                                Type::makeUnion(union1, K(isValue)), *scope, SrcPos(),
                                K(reportErrors)));

  TypeVector union2 = makeVector(Type::makeTypeRef("Medium"), Type::makeTypeRef("Ultra"),
                                 Type::makeTypeRef("Abstract"));

  REQUIRE(!herschel::isSameType(Type::makeUnion(union0, K(isValue)),
                                Type::makeUnion(union2, K(isValue)), *scope, SrcPos(),
                                K(reportErrors)));
}


TEST_CASE("Is same for seq types", "[type]")
{
  auto scope = testScopeSetup();

  TypeVector seq0 = makeVector(Type::makeTypeRef("Xyz"), Type::makeTypeRef("Medium"));

  TypeVector seq1 = makeVector(Type::makeTypeRef("Medium"), Type::makeTypeRef("Xyz"));

  REQUIRE(herschel::isSameType(Type::makeIntersection(seq0, K(isValue)),
                               Type::makeIntersection(seq0, K(isValue)), *scope, SrcPos(),
                               K(reportErrors)));
  REQUIRE(!herschel::isSameType(Type::makeIntersection(seq0, K(isValue)),
                                Type::makeIntersection(seq1, K(isValue)), *scope,
                                SrcPos(), K(reportErrors)));

  TypeVector seq2 = makeVector(Type::makeTypeRef("Medium"), Type::makeTypeRef("Ultra"),
                               Type::makeTypeRef("Abstract"));

  REQUIRE(!herschel::isSameType(Type::makeIntersection(seq0, K(isValue)),
                                Type::makeIntersection(seq2, K(isValue)), *scope,
                                SrcPos(), K(reportErrors)));
}


TEST_CASE("Is same for function types", "[type]")
{
  auto scope = testScopeSetup();

  FunctionParamVector params0;
  REQUIRE(herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params0)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params0)),
      *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params0)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("bar"),
                                           Type::makeTypeRef("Xyz"), params0)),
      *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params0)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("bar"),
                                           Type::makeTypeRef("Abstract"), params0)),
      *scope, SrcPos(), !K(reportErrors)));

  FunctionParamVector params1 = makeVector(FunctionParameter(
      FunctionParameter::kParamPos, !K(isSpec), String(), Type::makeTypeRef("Medium")));
  REQUIRE(herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params1)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("bar"),
                                           Type::makeTypeRef("Xyz"), params1)),
      *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params1)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("bar"),
                                           Type::makeTypeRef("Xyz"), params0)),
      *scope, SrcPos(), !K(reportErrors)));

  params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                      String("na"), Type::makeTypeRef("Xyz")));
  params1.push_back(FunctionParameter(FunctionParameter::kParamNamed, !K(isSpec),
                                      String("nu"), Type::makeTypeRef("Abstract")));
  params1.push_back(FunctionParameter(FunctionParameter::kParamRest, !K(isSpec),
                                      String("rest"), Type::makeAny(K(isValue))));

  REQUIRE(herschel::isSameType(
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("foo"),
                                           Type::makeTypeRef("Xyz"), params1)),
      Type::makeFunction(FunctionSignature(!K(isGeneric), String("bar"),
                                           Type::makeTypeRef("Xyz"), params1)),
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

  REQUIRE(!herschel::isSameType(
      Type::makeTypeRef(String("Mappable"),
                        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                        !K(isvalue)),
      Type::makeTypeRef("Full"), *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(herschel::isSameType(
      Type::makeTypeRef(String("Mappable"),
                        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                        !K(isvalue)),
      Type::makeTypeRef(String("Mappable"),
                        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                        !K(isvalue)),
      *scope, SrcPos(), !K(reportErrors)));

  REQUIRE(!herschel::isSameType(
      Type::makeTypeRef(String("Mappable"),
                        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                        !K(isvalue)),
      Type::makeTypeRef(String("Mappable"),
                        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Mno")),
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
  REQUIRE(!herschel::inheritsFrom(Type::makeTypeRef("Base"), Type::makeTypeRef("Base"),
                                  *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::makeTypeRef("Ultra"), Type::makeTypeRef("Obj"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::makeTypeRef("Special"), Type::makeTypeRef("Base"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::inheritsFrom(Type::makeTypeRef("Special"),
                                 Type::makeTypeRef("Abstract"), *scope, SrcPos(),
                                 !K(reportError)));

  REQUIRE(!herschel::inheritsFrom(Type::makeTypeRef("Top"), Type::makeTypeRef("Abstract"),
                                  *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::inheritsFrom(Type::makeTypeRef("Xyz"), Type::makeTypeRef("Base"),
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

  REQUIRE(herschel::inheritsFrom(
      Type::makeTypeRef("Full"),
      Type::makeTypeRef(
          String("Mappable"),
          makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                     Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
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

  REQUIRE(herschel::isCovariant(Type::makeTypeRef("Base"), Type::makeTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::makeTypeRef("Xyz"), Type::makeTypeRef("Xyz"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::makeTypeRef("Medium"), Type::makeTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(Type::makeTypeRef("Base"), Type::makeTypeRef("Medium"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isContravariant(Type::makeTypeRef("Base"),
                                    Type::makeTypeRef("Medium"), *scope, SrcPos(),
                                    !K(reportError)));
  REQUIRE(!herschel::isContravariant(Type::makeTypeRef("Medium"),
                                     Type::makeTypeRef("Base"), *scope, SrcPos(),
                                     !K(reportError)));

  REQUIRE(!herschel::isCovariant(Type::makeTypeRef("Top"), Type::makeTypeRef("Xyz"),
                                 *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isContravariant(Type::makeTypeRef("Top"), Type::makeTypeRef("Xyz"),
                                     *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isInvariant(Type::makeTypeRef("Top"), Type::makeTypeRef("Xyz"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Covariance for multiple inheritance for basic types", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isCovariant(Type::makeTypeRef("Ultra"), Type::makeTypeRef("Base"),
                                *scope, SrcPos(), !K(reportError)));
  REQUIRE(herschel::isCovariant(Type::makeTypeRef("Ultra"), Type::makeTypeRef("Abstract"),
                                *scope, SrcPos(), !K(reportError)));
}


TEST_CASE("Covariance for sliceable arrays", "[type]")
{
  auto scope = testScopeSetup();

  REQUIRE(herschel::isCovariant(
      Type::makeArray(Type::makeTypeRef(String("Ultra"), K(isValue)), 0, K(isValue)),
      Type::makeType(Names::kSliceableTypeName,
                     makeVector(Type::makeInt32(), Type::makeTypeRef("Ultra")), Type()),
      *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(
      Type::makeArray(Type::makeTypeRef(String("Special"), K(isValue)), 0, K(isValue)),
      Type::makeType(Names::kSliceableTypeName,
                     makeVector(Type::makeInt32(), Type::makeTypeRef("Ultra")), Type()),
      *scope, SrcPos(), !K(reportError)));
  REQUIRE(!herschel::isCovariant(
      Type::makeArray(Type::makeTypeRef(String("Ultra"), K(isValue)), 0, K(isValue)),
      Type::makeType(Names::kSliceableTypeName,
                     makeVector(Type::makeInt32(), Type::makeTypeRef("Special")), Type()),
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
    REQUIRE(herschel::isCovariant(
        Type::makeTypeRef("Full"),
        Type::makeTypeRef(
            String("Mappable"),
            makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                       Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
            !K(isvalue)),
        *scope, SrcPos(), !K(reportErrors)));
    REQUIRE(herschel::isCovariant(
        Type::makeTypeRef("Ultra"),
        Type::makeTypeRef(
            String("Mappable"),
            makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                       Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
            !K(isvalue)),
        *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 2")
  {
    REQUIRE(herschel::isCovariant(
        Type::makeTypeRef("Full"),
        Type::makeTypeRef(String("Mappable"),
                          makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                          !K(isvalue)),
        *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 3")
  {
    REQUIRE(herschel::isCovariant(
        Type::makeTypeRef(
            String("Mappable"),
            makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                       Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
            !K(isvalue)),
        Type::makeTypeRef(String("Mappable"),
                          makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
                          !K(isvalue)),
        *scope, SrcPos(), !K(reportErrors)));
  }

  SECTION("Case 4")
  {
    TypeCtx localCtx;

    REQUIRE(herschel::isCovariant(
        Type::makeTypeRef("Multi"),
        Type::makeTypeRef(
            String("OrdMap"),
            makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                       Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
            !K(isvalue)),
        *scope, SrcPos(), !K(reportErrors)));
  }
}


//----------------------------------------------------------------------------

TEST_CASE("Type constraints construction", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::makeValue(kConstOp_equal, Token(sp, kInt, 42));
  REQUIRE(kConstOp_equal == t0.constOp());
}


TEST_CASE("Type constraints equal constraint", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::makeValue(kConstOp_equal, Token(sp, kInt, 42));
  TypeConstraint t1 = TypeConstraint::makeValue(kConstOp_equal, Token(sp, kInt, 42));
  REQUIRE(kConstOp_equal == t0.constOp());
  REQUIRE(t0 == t1);
}


TEST_CASE("Type constraints and constraint", "[type]")
{
  SrcPos sp;
  TypeConstraint t0 = TypeConstraint::makeValue(kConstOp_notEqual, Token(sp, kInt, 10));
  TypeConstraint t1 = TypeConstraint::makeValue(kConstOp_notEqual, Token(sp, kInt, 21));
  TypeConstraint t2 = TypeConstraint::makeAnd(t0, t1);
  TypeConstraint t3 = TypeConstraint::makeOr(t0, t2);

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
  TypeConstraint t0 = TypeConstraint::makeType(kConstOp_isa, Type::makeInt32());
  REQUIRE(kConstOp_isa == t0.constOp());
  REQUIRE(Type::makeInt32() == t0.typeConstraint());
}


//----------------------------------------------------------------------------

TEST_CASE("Function parameter posParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::makePosParam(Type::makeInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamPos == p0.kind());
}


TEST_CASE("Function parameter specParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::makeSpecParam(Type::makeInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamPos == p0.kind());
}


TEST_CASE("Function parameter namedParamCtor", "[type]")
{
  FunctionParameter p0 =
      FunctionParameter::makeNamedParam(String("abc"), Type::makeInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(String("abc") == p0.key());
  REQUIRE(FunctionParameter::kParamNamed == p0.kind());
}

TEST_CASE("Function parameter restParamCtor", "[type]")
{
  FunctionParameter p0 = FunctionParameter::makeRestParam(Type::makeInt32());
  REQUIRE(p0.type().isInt32());
  REQUIRE(!p0.isSpecialized());
  REQUIRE(p0.key().isEmpty());
  REQUIRE(FunctionParameter::kParamRest == p0.kind());
}


//----------------------------------------------------------------------------

TEST_CASE("Function signature", "[type]")
{
  FunctionSignature fs0 =
      FunctionSignature(!K(isGeneric), String("abc"), Type::makeInt32());

  FunctionParamVector params1 =
      makeVector(FunctionParameter::makeSpecParam(Type::makeString()),
                 FunctionParameter::makePosParam(Type::makeInt32()),
                 FunctionParameter::makeNamedParam(String("xyz"), Type::makeFloat32()),
                 FunctionParameter::makeRestParam(Type::makeAny()));

  FunctionSignature fs1 =
      FunctionSignature(K(isGeneric), String("man"), Type::makeInt32(), params1);

  REQUIRE(!fs0.isGeneric());
  REQUIRE(String("abc") == fs0.methodName());
  REQUIRE(Type::makeInt32() == fs0.returnType());
  REQUIRE(fs0.parameters().empty());

  REQUIRE(fs1.isGeneric());
  REQUIRE(String("man") == fs1.methodName());
  REQUIRE(Type::makeInt32() == fs1.returnType());
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
  FunctionSignature fs0 =
      FunctionSignature(!K(isGeneric), String("abc"), Type::makeInt32());
  REQUIRE(!fs0.isOpen());

  FunctionParamVector params1 =
      makeVector(FunctionParameter::makeSpecParam(Type::makeString()),
                 FunctionParameter::makePosParam(
                     Type::makeTypeRef(String("x"), K(isOpen), !K(isValue))));

  FunctionSignature fs1 =
      FunctionSignature(K(isGeneric), String("man"),
                        Type::makeTypeRef(String("y"), K(isOpen), !K(isValue)), params1);
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

  Type mapGen = (Type::makeTypeRef(
      String("Mappable"),
      makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                 Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
      !K(isvalue)));
  Type mapCon = Type::makeTypeRef(
      String("Mappable"), makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")),
      !K(isvalue));
  REQUIRE(mapGen.matchGenerics(localCtx, mapCon, *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::makeTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::makeTypeRef("Def") == localCtx.lookupType(String("E")));
}


TEST_CASE("Match generics for complex generics", "[type]")
{
  auto scope = testScopeSetupGenerics();

  SECTION("Is same")
  {
    TypeCtx localCtx;

    Type mapGen = (Type::makeTypeRef(
        String("Mappable"),
        makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                   Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
        !K(isvalue)));
    Type mapCon = (Type::makeTypeRef(
        String("Mappable"),
        makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                   Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
        !K(isvalue)));
    REQUIRE(mapGen.matchGenerics(localCtx, mapCon, *scope, SrcPos()));

    REQUIRE(localCtx.hasType(String("K")));
    REQUIRE(localCtx.hasType(String("E")));
    REQUIRE(!localCtx.hasType(String("T")));
  }

  SECTION("Union is covariant")
  {
    Type mapGen = (Type::makeTypeRef(
        String("Mappable"),
        makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                   Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
        !K(isvalue)));
    Type undef = Type::makeTypeRef(String("Undef"), !K(isvalue));

    Type unionGen = Type::makeUnion(makeVector(mapGen, undef), K(isValue));

    Type mapCon = Type::makeTypeRef(
        String("Mappable"),
        makeVector(Type::makeTypeRef("Abc"), Type::makeTypeRef("Def")), !K(isvalue));

    REQUIRE(isCovariant(unionGen, mapGen, *scope, SrcPos(), !K(reportErrors)));
    REQUIRE(isCovariant(unionGen, mapCon, *scope, SrcPos(), !K(reportErrors)));
  }
}


TEST_CASE("Match generic for inherited generics", "[type]")
{
  TypeCtx localCtx;
  auto scope = testScopeSetupGenerics();

  Type mappable = (Type::makeTypeRef(
      String("Mappable"),
      makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                 Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
      !K(isvalue)));

  REQUIRE(mappable.matchGenerics(localCtx, Type::makeTypeRef("Full"), *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::makeTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::makeTypeRef("Def") == localCtx.lookupType(String("E")));
}


TEST_CASE("Match generics for inherited generics with indirect multi inheritance",
          "[type]")
{
  TypeCtx localCtx;
  auto scope = testScopeSetupGenerics();

  Type mappable = (Type::makeTypeRef(
      String("Mappable"),
      makeVector(Type::makeTypeRef(String("K"), K(isopen), !K(isvalue)),
                 Type::makeTypeRef(String("E"), K(isopen), !K(isvalue))),
      !K(isvalue)));

  REQUIRE(mappable.matchGenerics(localCtx, Type::makeTypeRef("Multi"), *scope, SrcPos()));

  REQUIRE(localCtx.hasType(String("K")));
  REQUIRE(localCtx.hasType(String("E")));
  REQUIRE(!localCtx.hasType(String("T")));
  REQUIRE(Type::makeTypeRef("Abc") == localCtx.lookupType(String("K")));
  REQUIRE(Type::makeTypeRef("Def") == localCtx.lookupType(String("E")));
}

}  // namespace herschel
