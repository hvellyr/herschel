/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_type_h
#define bootstrap_type_h

#include "common.h"

#include <vector>
#include <map>

#include "token.h"
#include "refcountable.h"
#include "ptr.h"


namespace herschel
{
  class Type;
  class TypeConstraint;
  class FunctionSignature;
  class FunctionParameter;
  class TypeCtx;
  class TypeEnumMaker;
  class Scope;
  class SrcPos;

  typedef std::vector<Type> TypeVector;
  typedef std::vector<TypeConstraint> TypeConstVector;
  typedef std::vector<FunctionParameter> FunctionParamVector;
  typedef std::map<String, Token> StringTokenMap;


  //--------------------------------------------------------------------------

  //! Technically there are Type Instances and Type References.  Type
  //! Instances represent a fully defined type, class, alias, enum, measure,
  //! function type, or base type (like Int, String).  They are normally never
  //! used explicit in the code (when annotating variables, etc.); there're
  //! normaly Type References are used:
  //!
  //! <code>
  //! def type XYZ<a, b> : (Abc<a>, Man<b>)
  //!
  //! def v : XYZ<Int, Char> = 5
  //! </code>
  //!
  //! The type annotation in the second line is a type references XYZ<Int,
  //! Char> to the type definition in the first line, specializing two type
  //! parameters.  The resulting effective type in the second line is
  //! therefore "XYZ<Int, Char> : (Abc<Int>, Man<Char>)".

  //--------------------------------------------------------------------------

  enum TypeKind
  {
    kType_Undefined,

    kType_Ref,

    kType_Array,

    kType_Function,
    kType_Measure,
    kType_Class,
    kType_Type,
    kType_Alias,

    kType_Union,
    kType_Sequence,
  };

  class TypeImpl : public RefCountable
  {
  public:
    virtual TypeImpl* clone() const = 0;
    virtual bool isEqual(const TypeImpl* other) const = 0;

    virtual bool isOpen() const = 0;
    virtual bool isOpenSelf() const = 0;

    virtual void replaceGenerics(const TypeCtx& typeMap) = 0;

    virtual String toString(bool isValue) const = 0;

    virtual bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                               Scope* scope, const SrcPos& srcpos) const = 0;
  };


  class Type
  {
  public:
    Type(const Type& other);
    Type();


    //! creates a new type ref.  Covers the following example: xyz<a, b> <> nil
    static Type newTypeRef(const String& name, const TypeVector& genericArgs,
                           const TypeConstVector& constraints,
                           bool isValue);
    static Type newTypeRef(const String& name, const TypeVector& genericArgs,
                           bool isValue);
    static Type newTypeRef(const String& name, bool isValue);
    static Type newTypeRef(const char* name, bool isValue = true);
    static Type newTypeRef(const String& name, bool isOpen,
                           const TypeConstVector& constraints, bool isValue);
    static Type newTypeRef(const String& name, bool isOpen, bool isValue);

    //! rewrite \p old to a new typeref with name.
    static Type newTypeRef(const String& name, const Type& old);

    //! creates a lang|Class<type>
    static Type newClassOf(const Type& type, bool isValue = true);

    static Type newArray(const Type& base, int sizeIndicator, bool isValue);

    static Type newAny(bool isValue = true);

    static Type newInt32(bool isValue = true);
    static Type newUInt32(bool isValue = true);
    static Type newRational(bool isValue = true);
    static Type newFloat32(bool isValue = true);
    static Type newString(bool isValue = true);
    static Type newBool(bool isValue = true);

    //! Creates a new Type type-instance.  This represents a specific type,
    //! not the type template/definition.  Therefore all (possible) type
    //! parameters must be fill in (in generics).
    static Type newType(const String& name, const TypeVector& generics,
                        const Type& inherit);

    static Type newClass(const String& name, const TypeVector& generics,
                         const Type& inherit, const FunctionSignature& applySign);

    static Type newAlias(const String& name, const TypeVector& generics,
                         const Type& isa);

    static Type newMeasure(const String& name, const Type& baseType,
                           const String& defUnit);

    static Type newFunction(const FunctionSignature& sign);
    static Type newUnion(const TypeVector& types, bool isValue);
    static Type newSeq(const TypeVector& types, bool isValue);

    Type clone() const;

    //! assign operator
    Type& operator=(const Type& other);

    //! Compare operator.  Indicates true if *this and other are fully
    //! identical.
    bool operator==(const Type& other) const;
    //! Compare operator
    bool operator!=(const Type& other) const;


    bool isDef() const;

    TypeKind kind() const;

    //!@ base and builtin types
    //! indicates whether the type is a base type
    bool isBaseType() const;

    //! indicates whether thi type is a plain type.  A plain type is a type
    //! which can be represented as non-complex machine types.
    bool isPlainType() const;

    bool isAny() const;

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

    bool isAnyNumber() const;

    bool isSigned() const;

    bool isClassOf() const;

    bool isImaginary() const;
    void setIsImaginary(bool value);

    TypeEnumMaker* newBaseTypeEnumMaker() const;

    //!@ custom types
    bool isClass() const;

    //!@ custom types
    bool isType() const;
    const Type& typeInheritance() const;
    const FunctionSignature& applySignature() const;


    //@ is anything on this type is generic (i.e. a variable type
    //expression).  For references the type itself must be marked as generic
    //(e.g. 'T); for complex type any subtype (generics, parameters, etc.)
    //must be a Generic type reference (e.g. 'T[], List<'T>, &('T, Bool = false))
    bool isOpen() const;
    bool isOpenSelf() const;

    bool matchGenerics(TypeCtx& localCtx, const Type& right0,
                       Scope* scope, const SrcPos& srcpos) const;


    //!@ alias types
    bool isAlias() const;
    const Type& aliasReplaces() const;


    //!@ function types
    //! Indicates whether the type is a function type
    bool isFunction() const;
    const FunctionSignature& functionSignature() const;


    //!@ array types
    bool isArray() const;
    const Type& arrayBaseType() const;
    int arraySizeIndicator() const;
    //! return the base type.  If *this is a single depth array identical to
    //! arrayBaseType().
    Type arrayRootType() const;
    Type rebase(const Type& newBaseType) const;


    //!@ union types
    bool isUnion() const;
    const TypeVector& unionTypes() const;


    //!@ sequence types
    bool isSequence() const;
    const TypeVector& seqTypes() const;
    bool containsType(const Type& type) const;

    //!@ measure types
    bool isMeasure() const;
    const Type& measureBaseType() const;
    String measureUnit() const;

    //!@ has constraints?
    bool hasConstraints() const;
    const TypeConstVector& constraints() const;
    Type setConstraints(const TypeConstVector& constraints) const;

    bool hasGenerics() const;
    const TypeVector& generics() const;
    Type replaceGenerics(const TypeCtx& typeMap) const;

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
    Type(TypeKind kind, bool isValue, bool isImaginary, TypeImpl* impl);

    TypeKind      fKind;
    bool          fIsValue;
    bool          fIsImaginary;
    Ptr<TypeImpl> fImpl;
  };


  //--------------------------------------------------------------------------

  enum TypeConstOperator
  {
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


  class BaseTypeConstraintImpl : public RefCountable
  {
  public:
    virtual BaseTypeConstraintImpl* clone() const = 0;
    virtual bool isEqual(const BaseTypeConstraintImpl* other) const = 0;
    virtual TypeConstOperator constOp() const = 0;
    virtual void replaceGenerics(const TypeCtx& typeMap) = 0;
    virtual String toString() const = 0;
  };


  class TypeConstraint
  {
  public:
    TypeConstraint(const TypeConstraint& other);

    static TypeConstraint newAnd(const TypeConstraint& left,
                                 const TypeConstraint& right);
    static TypeConstraint newOr(const TypeConstraint& left,
                                const TypeConstraint& right);
    static TypeConstraint newValue(TypeConstOperator op, const Token& value);
    static TypeConstraint newType(TypeConstOperator op, const Type& type);

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
    TypeConstraint(BaseTypeConstraintImpl* impl);

    Ptr<BaseTypeConstraintImpl> fImpl;
  };


  //--------------------------------------------------------------------------

  class FunctionParameter
  {
  public:
    enum ParameterKind
    {
      kParamPos,
      kParamNamed,
      kParamRest
    };

    FunctionParameter(ParameterKind kind, bool isSpec, const String& key,
                      const Type& type);
    FunctionParameter(const FunctionParameter& other);

    static FunctionParameter newPosParam(const Type& type);
    static FunctionParameter newSpecParam(const Type& type);
    static FunctionParameter newNamedParam(const String& key,
                                           const Type& type);
    static FunctionParameter newRestParam(const Type& type);

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
    bool          fIsSpecialized;
    String        fKey;
    Type          fType;
  };


  //--------------------------------------------------------------------------

  class FunctionSignature
  {
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
                       Scope* scope, const SrcPos& srcpos) const;

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
    bool                fIsGeneric;
    String              fName;
    Type                fReturnType;
    FunctionParamVector fParameters;
  };


  //--------------------------------------------------------------------------

  class TypeUnit
  {
  public:
    TypeUnit();
    TypeUnit(const String& name, const String& derivedFrom,
             const Type& effectiveType);
    TypeUnit(const TypeUnit& other);

    bool isDef() const;

    const String& name() const;
    const String& derivedFromName() const;
    const Type& effType() const;

    TypeUnit& operator=(const TypeUnit& other);

  private:
    String fName;
    String fDerivedFrom;
    Type   fEffType;
  };


  //--------------------------------------------------------------------------

  bool inheritsFrom(const Type& left, const Type& right, Scope* scope,
                    const SrcPos& srcpos, bool reportErrors = true);

  bool isSameType(const Type& left, const Type& right, Scope* scope,
                  const SrcPos& srcpos, bool reportErrors = true);
  bool isCovariant(const Type& left, const Type& right, Scope* scope,
                   const SrcPos& srcpos, bool reportErrors = true);
  bool isContravariant(const Type& left, const Type& right, Scope* scope,
                       const SrcPos& srcpos, bool reportErrors = true);
  bool isInvariant(const Type& left, const Type& right, Scope* scope,
                   const SrcPos& srcpos, bool reportErrors = true);
  bool containsAny(const Type& left, const SrcPos& srcpos,
                   bool reportErrors = true);

  TypeVector newTypeVector();
  TypeVector newTypeVector(const Type& ty1);
  TypeVector newTypeVector(const Type& ty1, const Type& ty2);
  TypeVector newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3);
  TypeVector newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3,
                           const Type& ty4);
  TypeVector newTypeVector(const Type& ty1, const Type& ty2, const Type& ty3,
                           const Type& ty4, const Type& ty5);

  TypeConstVector newTypeConstVector();

  Type newRangeType(const Type& generic);


  void tyerror(const Type& type, const char* msg);

  int floatTypeBitsize(const Type& ty);
  int intTypeBitsize(const Type& ty);

  Type maxFloatType(const Type& leftty, const Type& rightty);
  Type maxIntType(const Type& leftty, const Type& rightty);

  Type degeneralizeType(const SrcPos& srcpos, const Type& type,
                        const TypeVector& srcGenerics);

};                              // namespace



#endif                          // bootstrap_type_h
