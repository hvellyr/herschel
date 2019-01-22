/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "optional.hpp"
#include "token.hpp"

#include <map>
#include <memory>
#include <vector>


namespace herschel {

class Type;
class TypeConstraint;
class FunctionSignature;
class FunctionParameter;
class TypeCtx;
class TypeEnumMaker;
class Scope;
class SrcPos;
class TypeSlot;
class TypeProperty;

using TypeVector = std::vector<Type>;
using TypeConstVector = std::vector<TypeConstraint>;
using FunctionParamVector = std::vector<FunctionParameter>;
using StringTokenMap = std::map<String, Token>;
using TypeSlotList = std::vector<TypeSlot>;


//! Technically there are Type Instances and Type References.  Type
//! Instances represent a fully defined type, class, alias, enum,
//! function type, or base type (like Int, String).  They are normally never
//! used explicit in the code (when annotating variables, etc.); there're
//! normaly Type References are used:
//!
//! <code>
//! def type XYZ<a, b> : (Abc\<a>, Man\<b>)
//!
//! def v : XYZ<Int, Char> = 5
//! </code>
//!
//! The type annotation in the second line is a type references XYZ<Int,
//! Char> to the type definition in the first line, specializing two type
//! parameters.  The resulting effective type in the second line is
//! therefore "XYZ<Int, Char> : (Abc<Int>, Man<Char>)".


//! specifying the type of TypeImpl instance.
enum TypeKind {
  kType_Undefined,  //!< just as default value for undefined kind

  kType_Ref,  //!< specifies a type reference

  kType_Array,  //!< specifies an array kind

  kType_Function,  //!< specifies a function kind
  kType_Class,     //!< specifies a class kind
  kType_Type,      //!< specifies a type kind
  kType_Alias,     //!< specifies an type alias kind

  kType_Union,         //!< specifies an union type kind
  kType_Intersection,  //!< specifies an intersection type kind
};


//! Base class for the implementation of types.  This defines the base
//! interface a type implementation must provide.

class TypeImpl {
public:
  //! Make a deep copy of the receiver.  Note that even the base type Type
  //! must be cloned.
  virtual std::shared_ptr<TypeImpl> clone() const = 0;

  //! Indicates whether the receiver is equal to \p other.
  virtual bool isEqual(const TypeImpl* other) const = 0;

  //! Indicates whether the receiver is an "open" type, i.e. whether it has
  //! or is a generic (parameterized) type, which is not specified
  //! (e.g. takes the form 'T).  This predicate is recursive, i.e. if the
  //! receiver is a complex type then any descendant being "open" makes the
  //! receiver "open", too.
  virtual bool isOpen() const = 0;

  //! Indicates whether the receiver itself is an "open" type, i.e. whether
  //! it has or is a generic (parameterized) type, which is not specified
  //! (e.g. takes the form 'T).
  virtual bool isOpenSelf() const = 0;

  //! Replace generics types (or subtypes) with their full form according to
  //! the type context \p typeMap.  Note that this function solely is to
  //! allow recursion into subtypes hold by the receiving TypeImpl.  The
  //! real replacement should be handled by Type::replaceGenerics.
  virtual void replaceGenerics(const TypeCtx& typeMap) = 0;

  //! Indicates whether the type \p right0 matches the receiver in regard to
  //! its generics in the type context \p localCtx.  The intended usage of
  //! this function is that \p right0 is a concrete type (with possible
  //! concrete type parameters) which is checked against the "open" receiver
  //! type.  The \p scope is required for looking up type references, the \p
  //! srcpos for possible error messages.
  //!
  //! Note that this function is needed for recursion into complex types;
  //! the public API for this is Type::matchGenerics().
  virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0, const Scope& scope,
                             const SrcPos& srcpos) const = 0;

  //! Return a string representation, mostly useful for debugging purposes.
  //! \p isValue indicates whether the type is actually a value type (this
  //! information is kept in the outer Type instance, and not in the
  //! TypeImpl).
  virtual String toString(bool isValue) const = 0;
};


//! Represents a type in the compiler.  All type kinds (base, array, union,
//! etc.) are all represented by this single type.  To distinguish between
//! the various kinds either use the predicate functions (isRecord(), \c
//! isArray(), \c isInt32(), etc.) or the \c kind() function.  The type
//! class is intended to be uses as const immutable value object.
class Type {
public:
  Type(const Type& other);
  Type();

  //@{ Factory functions

  //! Creates a new type ref.  Covers the following example: xyz<a, b> <> nil
  static Type makeTypeRef(const String& name, const TypeVector& genericArgs,
                          const TypeConstVector& constraints, bool isValue);
  static Type makeTypeRef(const String& name, const TypeVector& genericArgs,
                          bool isValue);
  static Type makeTypeRef(const String& name, bool isValue);
  static Type makeTypeRef(zstring name, bool isValue = true);
  static Type makeTypeRef(const String& name, bool isOpen,
                          const TypeConstVector& constraints, bool isValue);
  static Type makeTypeRef(const String& name, bool isOpen, bool isValue);

  //! Rewrite \p old to a new typeref taking the typename \p name.
  static Type makeTypeRef(const String& name, const Type& old);

  //! Creates a new lang.Type<'T> type instance with 'T being \p type.
  static Type makeClassTypeOf(const Type& type, bool isValue = true);

  //! Creates a new 'T[] array type with \p base being the array base type
  //! 'T.  The \p sizeIndicator is informational, since the size is not
  //! essential part of an array type in herschel.
  static Type makeArray(const Type& base, int sizeIndicator, bool isValue);

  //! Creates a new lang.Any type instance.
  static Type makeAny(bool isValue = true);

  //! Creates a new lang.Int32 type instance.
  static Type makeInt32(bool isValue = true);
  //! Creates a new lang.UInt32 type instance.
  static Type makeUInt32(bool isValue = true);

  //! Creates a new lang.IntX type instance with X being the bitwidth.
  static Type makeInt(int bitwidth, bool isValue = true);
  //! Creates a new lang.UIntX type instance with X being the bitwidth.
  static Type makeUInt(int bitwidth, bool isValue = true);

  //! Creates a new lang.Rational type instance.
  static Type makeRational(bool isValue = true);
  //! Creates a new lang.Float32 type instance.
  static Type makeFloat32(bool isValue = true);
  //! Creates a new lang.String type instance.
  static Type makeString(bool isValue = true);
  //! Creates a new lang.Bool type instance.
  static Type makeBool(bool isValue = true);
  //! Creates a new lang.Keyword type instance.
  static Type makeKeyword(bool isValue = true);
  //! Creates a new lang.Char type instance.
  static Type makeChar(bool isValue = true);

  //! Creates a new Type type instance named \p name.  This represents a
  //! specific type, not the type template/definition.  Therefore all
  //! (possible) type parameters must be specified and given in \p generics,
  //! i.e. \p generics is not allowed to contain "open" types.  QUESTION: Is
  //! this true?  As far as I can see in the code this can happen anyway.
  static Type makeType(const String& name, const TypeVector& generics,
                       const Type& inherit);

  //! Creates a new Class type instance named \p inheriting from \p inherit.
  //! \p applySign specifies the signatures of the ctor apply call, and \p
  //! slots specifies the list of slots.  If the class is parameterized the
  //! parameter types are to be specifies in \p generics.
  //!
  //! \pre \p applySign must a valid signature; \p name must not be empty
  static Type makeClass(const String& name, const TypeVector& generics,
                        const Type& inherit, const FunctionSignature& applySign,
                        const TypeSlotList& slots);

  //! Creates a new type alias named \p name for type \p isa.  If the alias
  //! itself is parameterized the type parameters have to be specified in \p
  //! generics.

  //! \pre \p name must not be empty; \p isa must be a valid type instance
  static Type makeAlias(const String& name, const TypeVector& generics, const Type& isa);

  //! Create a new function type instance using the function signature \p
  //! sign.
  static Type makeFunction(const FunctionSignature& sign);

  //! Creates a new union type instance for the types \p types.  Note that
  //! even if \p types is given as vector the order of \p types is not
  //! relevant.
  static Type makeUnion(const TypeVector& types, bool isValue);

  //! Creates a new intersection type instance for the types \p types.
  static Type makeIntersection(const TypeVector& types, bool isValue);

  //@}


  //! Make a deep copy of the receiver.  Since copying can be expensive this
  //! is not implemented via the normal copying ctor.
  Type clone() const;

  //! Assign operator
  Type& operator=(const Type& other);

  //! Compare operator.  Indicates true if *this and \p other are fully
  //! identical.  For logical comparison of types use the isSameType()
  //! function.
  bool operator==(const Type& other) const;
  //! Compare operator.  For logical comparison of types use the
  //! isSameType() function.
  bool operator!=(const Type& other) const;

  //! Indicates whether the receiver is an initialized instance (i.e. not
  //! constructed by the Type() ctor).
  bool isDef() const;

  //! Return the type's internal implementation kind.
  TypeKind kind() const;

  //! @{ Base and builtin types

  //! indicates whether the type is a base type
  bool isBaseType() const;

  //! indicates whether the type is a plain type.  A plain type is a type
  //! which can be represented as non-complex machine types.
  bool isPlainType() const;

  bool isAny() const;
  bool isClangAtom() const;

  //! indicates whether the type is a generic lang.Integer type.
  bool isInteger() const;

  bool isInt32() const;
  bool isString() const;
  bool isFloat32() const;
  bool isNumber() const;
  bool isComplex() const;
  bool isAnyFloat() const;
  bool isRational() const;
  bool isUInt32() const;
  bool isAnyInt() const;
  bool isAnyUInt() const;
  bool isAnySignedInt() const;
  bool isChar() const;
  bool isBool() const;
  bool isKeyword() const;

  //! Indicates whether the receiver is any kind of number (isInt32(),
  //! isFloat32(), etc.).
  bool isAnyNumber() const;

  //! Indicates whether the receiver is a signed number.
  bool isSigned() const;

  //! Indicates whether the receiver is a lang.Type<'T> type instance.
  bool isClassTypeOf() const;

  bool isImaginary() const;
  void setIsImaginary(bool value);

  std::unique_ptr<TypeEnumMaker> makeBaseTypeEnumMaker() const;

  //! Return the typeProperty specication for the receiver.  Check \c
  //! isValid() on the return value before using it.  If \p mustExist is
  //! true requesting the typeproperty for a non-predefined type will
  //! assert.
  const TypeProperty& typeProperty(bool mustExist = true) const;

  //@}

  //@{ Custom types

  //! Indicates whether the receiver is a Record type instance.
  bool isRecord() const;

  //! Return the defined slots.
  //!
  //! \pre Only allowed when \c isRecord() returns true.
  const TypeSlotList& slots() const;
  //! Lookup the type for slot \p slotName.  If no such slot is defined for
  //! the receiver (or any of its superclasses) this function returns an
  //! undefined type (check \c isDef()).  The \p scope is required for
  //! normalizing the superclasses which are normally kept as reference
  //! types only.
  //!
  //! \pre Only allowed if \c isRecord() returns true.
  Type slotType(const String& slotName, const Scope& scope) const;

  bool isType() const;
  const Type& typeInheritance() const;
  const FunctionSignature& applySignature() const;

  //@}


  //! Indicates whether the receiver is an "open" type, i.e. whether it has
  //! or is a generic (parameterized) type, which is not specified
  //! (e.g. takes the form 'T).  This predicate is recursive, i.e. if the
  //! receiver is a complex type then any descendant being "open" make the
  //! receiver "open", too.
  bool isOpen() const;

  //! Indicates whether the receiver itself is an "open" type, i.e. whether
  //! it has or is a generic (parameterized) type, which is not specified
  //! (e.g. takes the form 'T).
  bool isOpenSelf() const;

  //! Indicates whether the type \p right0 matches the receiver in regard to
  //! its generics in the type context \p localCtx.  The intended usage of
  //! this function is that \p right0 is a concrete type (with possible
  //! concrete type parameters) which is checked against the "open" receiver
  //! type.  The \p scope is required for looking up type references, the \p
  //! srcpos for possible error messages.
  bool matchGenerics(TypeCtx& localCtx, const Type& right0, const Scope& scope,
                     const SrcPos& srcpos) const;

  //@{ alias types
  bool isAlias() const;
  const Type& aliasReplaces() const;
  //@}

  //@{ function types
  //! Indicates whether the type is a function type
  bool isFunction() const;
  const FunctionSignature& functionSignature() const;
  //@}

  //@{ array types

  //! Indicates whether the type is an array type.  Use \c arrayBaseType()
  //! to check the base array type.
  bool isArray() const;

  //! Returns the receiver's base type.  Only valid if \c isArray() is true.
  const Type& arrayBaseType() const;

  //! Returns the receiver's size indication.  If no size indication was
  //! explicitely given returns \c 0.
  int arraySizeIndicator() const;

  //! Returns the receiver's root base type.  If the receiver is a single
  //! depth array the value is identical to \c arrayBaseType().
  Type arrayRootType() const;

  //! Replace the receiver's base type with \p newBaseType.  Only valid if
  //! \c isArray() is true.
  Type rebase(const Type& newBaseType) const;
  //@}

  //@{ Union types

  //! Indicates whether the receiver is a union type.
  bool isUnion() const;

  //! Returns the list of types in a union type.
  const TypeVector& unionTypes() const;
  //@}

  //@{ Intersection types

  //! Indicates whether the receiver is a sequence type.
  bool isIntersection() const;

  //! Returns the list of types in a sequence type.
  const TypeVector& intersectionTypes() const;

  //! Indicates whether the receiver, which must be an intersection or
  //! union type, contains \p type.
  bool containsType(const Type& type) const;
  //@}

  //@{ Has constraints?
  bool hasConstraints() const;
  const TypeConstVector& constraints() const;
  Type setConstraints(const TypeConstVector& constraints) const;
  //@}

  //@{ Has Generics?
  bool hasGenerics() const;
  const TypeVector& generics() const;
  Type replaceGenerics(const TypeCtx& typeMap) const;
  //@}

  bool isRef() const;
  String typeName() const;
  String typeId() const;

  String toString() const;

  //! Types in herschel are either 'reference' or 'value' types.  Even if all
  //! objects are possible passed by reference internally, value types
  //! behave on (first) modification as if being copied (copy-on-write).
  bool isValueType() const;
  Type& setIsValueType(bool value);

  bool isBuiltinType(const String& name) const;

private:
  Type(TypeKind kind, bool isValue, bool isImaginary, std::shared_ptr<TypeImpl> impl);

  TypeKind fKind;
  bool fIsValue;
  bool fIsImaginary;
  std::shared_ptr<TypeImpl> fImpl;
};


//--------------------------------------------------------------------------

//! Encodes the slots defined on classes

class TypeSlot {
public:
  TypeSlot(const String& name, const Type& type, unsigned int flags);
  TypeSlot(const TypeSlot& other);

  TypeSlot clone() const;

  //! Assign operator
  TypeSlot& operator=(const TypeSlot& other);

  //! Compare operator.
  bool operator==(const TypeSlot& other) const;
  //! Compare operator
  bool operator!=(const TypeSlot& other) const;

  void replaceGenerics(const TypeCtx& typeMap);
  String toString() const;

  //! Return the slot's name
  String name() const;

  //! Return the slot's type
  Type type() const;

  //! Return the bitfields of flags
  unsigned int flags() const;

private:
  String fName;
  Type fType;
  unsigned int fFlags;
};


//--------------------------------------------------------------------------

enum TypeConstOperator {
  kConstOp_and,
  kConstOp_or,

  kConstOp_equal,
  kConstOp_notEqual,
  kConstOp_less,
  kConstOp_lessEqual,
  kConstOp_greater,
  kConstOp_greaterEqual,
  kConstOp_in,

  kConstOp_isa
};


class BaseTypeConstraintImpl {
public:
  virtual std::shared_ptr<BaseTypeConstraintImpl> clone() const = 0;
  virtual bool isEqual(const BaseTypeConstraintImpl* other) const = 0;
  virtual TypeConstOperator constOp() const = 0;
  virtual void replaceGenerics(const TypeCtx& typeMap) = 0;
  virtual String toString() const = 0;
};


class TypeConstraint {
public:
  TypeConstraint(const TypeConstraint& other);

  static TypeConstraint makeAnd(const TypeConstraint& left, const TypeConstraint& right);
  static TypeConstraint makeOr(const TypeConstraint& left, const TypeConstraint& right);
  static TypeConstraint makeValue(TypeConstOperator op, const Token& value);
  static TypeConstraint makeType(TypeConstOperator op, const Type& type);

  TypeConstraint clone() const;


  //! Assign operator
  TypeConstraint& operator=(const TypeConstraint& other);

  //! Compare operator.
  bool operator==(const TypeConstraint& other) const;
  //! Compare operator
  bool operator!=(const TypeConstraint& other) const;

  //! returns the constraint operator
  TypeConstOperator constOp() const;

  //! indicates whether the constraint is a value constraint like, ==, >=,
  //! in, etc.
  bool isValueConstraint() const;
  Token constraintValue() const;

  //! indicates whether the constraint is a logical constraint; i.e. if the
  //! constraint is a logical operation of other constraints.
  bool isLogicalConstraint() const;
  const TypeConstraint& leftConstraint() const;
  const TypeConstraint& rightConstraint() const;

  //! Indicates whether the constraint is a type constraint,
  //! i.e. testing for 'isa' relations.
  bool isTypeConstraint() const;
  Type typeConstraint() const;

  TypeConstraint replaceGenerics(const TypeCtx& typeMap);
  bool isDef() const { return true; }

  String toString() const;

private:
  TypeConstraint(std::shared_ptr<BaseTypeConstraintImpl> impl);

  std::shared_ptr<BaseTypeConstraintImpl> fImpl;
};


//--------------------------------------------------------------------------

class FunctionParameter {
public:
  enum ParameterKind { kParamPos, kParamNamed, kParamRest };

  FunctionParameter(ParameterKind kind, bool isSpec, const String& key, const Type& type);
  FunctionParameter(const FunctionParameter& other);

  static FunctionParameter makePosParam(const Type& type);
  static FunctionParameter makeSpecParam(const Type& type);
  static FunctionParameter makeNamedParam(const String& key, const Type& type);
  static FunctionParameter makeRestParam(const Type& type);

  FunctionParameter clone() const;

  //! Assign operator
  FunctionParameter& operator=(const FunctionParameter& other);

  //! Compare operator.
  bool operator==(const FunctionParameter& other) const;
  //! Compare operator
  bool operator!=(const FunctionParameter& other) const;

  //! Returns the kind of the parameter.
  ParameterKind kind() const;

  //! Indicate whether the parameter is specialized.  Can be true only for
  //! positional parameters.
  bool isSpecialized() const;

  //! returns the parameter key (if isNamed() is true)
  const String& key() const;

  //! returns the parameter's type
  const Type& type() const;

  FunctionParameter replaceGenerics(const TypeCtx& typeMap);
  bool isDef() const { return true; }

  String toString() const;

private:
  ParameterKind fKind;
  bool fIsSpecialized;
  String fKey;
  Type fType;
};


//--------------------------------------------------------------------------

class FunctionSignature {
public:
  FunctionSignature();

  FunctionSignature(bool isGeneric, const String& name, const Type& retType);
  FunctionSignature(bool isGeneric, const String& name, const Type& retType,
                    const FunctionParamVector& parameters);
  FunctionSignature(const FunctionSignature& other);

  FunctionSignature clone() const;

  //! Assign operator
  FunctionSignature& operator=(const FunctionSignature& other);

  //! Compare operator.
  bool operator==(const FunctionSignature& other) const;
  //! Compare operator
  bool operator!=(const FunctionSignature& other) const;

  //! Indicates whether the signature refers to a generic function.
  bool isGeneric() const;

  bool isOpen() const;

  bool matchGenerics(TypeCtx& localCtx, const FunctionSignature& right0,
                     const Scope& scope, const SrcPos& srcpos) const;

  //! Returns the name of the method.  May be empty if the function is
  //! anonymous.
  const String& methodName() const;

  //! Returns the returntype of the signature.
  const Type& returnType() const;

  //! Returns the signature's parameter list.  Is empty if the function has
  //! no parameters.  If the signature refers to a generic function
  //! (i.e. isGeneric returns true) the list has a least one parameter and
  //! the first parameter is at least specialized.
  const FunctionParamVector& parameters() const;

  FunctionSignature replaceGenerics(const TypeCtx& typeMap);

  String toString() const;
  String typeId() const;

  bool hasPositionalParam() const;

private:
  bool fIsGeneric;
  String fName;
  Type fReturnType;
  FunctionParamVector fParameters;
};


//--------------------------------------------------------------------------

class TypeUnit {
public:
  TypeUnit();
  TypeUnit(const String& name, const String& derivedFrom, const Type& effectiveType);
  TypeUnit(const TypeUnit& other);

  bool isDef() const;

  const String& name() const;
  const String& derivedFromName() const;
  const Type& effType() const;

  TypeUnit& operator=(const TypeUnit& other);

private:
  String fName;
  String fDerivedFrom;
  Type fEffType;
};


//--------------------------------------------------------------------------

bool inheritsFrom(const Type& left, const Type& right, const Scope& scope,
                  const SrcPos& srcpos, bool reportErrors = true);

bool isSameType(const Type& left, const Type& right, const Scope& scope,
                const SrcPos& srcpos, bool reportErrors = true);
//! Indicates whether the type @var{right} is covariant to (i.e. "narrower"
//! than) the type @var{left}.
bool isCovariant(const Type& left, const Type& right, const Scope& scope,
                 const SrcPos& srcpos, bool reportErrors = true);
//! Indicates whether the type @var{right} is contravariant to (i.e. "wider"
//! than) the type @var{left}.
bool isContravariant(const Type& left, const Type& right, const Scope& scope,
                     const SrcPos& srcpos, bool reportErrors = true);
//! Indicates whether the type @var{right} is invariant to the type
//! @var{left}, i.e. there's no relation between @var{left} and @var{right}.
bool isInvariant(const Type& left, const Type& right, const Scope& scope,
                 const SrcPos& srcpos, bool reportErrors = true);
//! Returns the "variance distance" of the types @var{right} and
//! @var{left}.  If @var{right} is covariant (i.e. narrower) to
//! @var{left} the first member of the result is negative, if it is
//! contravariant (i.e. wider) the first member of the result is
//! positive.  If the types are equivalent the result is 0.  If the
//! types are invariant the second member is false.
estd::optional<int> varianceDistance(const Type& left, const Type& right,
                                     const Scope& scope, const SrcPos& srcpos,
                                     bool reportErrors);
bool containsAny(const Type& left, const SrcPos& srcpos, bool reportErrors = true);

Type makeRangeType(const Type& generic);


void tyerror(const Type& type, zstring msg);

int floatTypeBitsize(const Type& ty);
int intTypeBitsize(const Type& ty);

Type maxFloatType(const Type& leftty, const Type& rightty);
Type maxIntType(const Type& leftty, const Type& rightty);

Type degeneralizeType(const SrcPos& srcpos, const Type& type,
                      const TypeVector& srcGenerics);


Type resolveType(const Type& type, const Scope& scope);

String arrayTypeName(const String& baseName);
String arrayTypeName(zstring baseName);

}  // namespace herschel
