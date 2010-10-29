/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_type_h
#define bootstrap_type_h

#include "common.h"

#include <vector>
#include <map>

#include "token.h"
#include "refcountable.h"
#include "ptr.h"


namespace heather
{
  class Type;
  class TypeConstraint;
  class FunctionSignature;
  class FunctionParameter;
  class TypeCtx;
  class TypeEnumMaker;

  typedef std::vector<Type> TypeVector;
  typedef std::vector<TypeConstraint> TypeConstVector;
  typedef std::vector<FunctionSignature> FunctionSignatureVector;
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
    virtual bool isCovariant(const TypeImpl* other) const = 0;
    virtual bool isContravariant(const TypeImpl* other) const;
    virtual bool isInvariant(const TypeImpl* other) const;

    virtual void replaceGenerics(const TypeCtx& typeMap) = 0;

    virtual String toString(bool isValue) const = 0;
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
    static Type newTypeRef(const String& name, bool isValue);
    static Type newTypeRef(const String& name, bool isGeneric,
                           const TypeConstVector& constraints,
                           bool isValue);

    static Type newArray(const Type& base, int siceIndicator, bool isValue);

    static Type newAny(bool isValue = true);

    static Type newInt(bool isValue = true);
    static Type newRational(bool isValue = true);
    static Type newReal(bool isValue = true);
    static Type newString(bool isValue = true);

    //! Creates a new Type type-instance.  This represents a specific type,
    //! not the type template/definition.  Therefore all (possible) type
    //! parameters must be fill in (in generics).
    static Type newType(const String& name, const TypeVector& generics,
                        const Type& inherit);
    static Type newType(const String& name, const TypeVector& generics,
                        const Type& inherit,
                        const FunctionSignatureVector& protocol);

    static Type newClass(const String& name, const TypeVector& generics,
                         const Type& inherit);
    static Type newClass(const String& name, const TypeVector& generics,
                         const Type& inherit,
                         const FunctionSignature& defApplySign,
                         const FunctionSignatureVector& protocol);

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

    //! Indicates whether two types are co-variant.  If this returns false \p
    //! other is either contravariant or invariant.  Use isInvariant to check.
    bool isCovariant(const Type& other) const;
    bool isContravariant(const Type& other) const;
    //! Indicates whether two types are invariant.
    bool isInvariant(const Type& other) const;


    bool isDef() const;



    //!@ base and builtin types
    //! indicates whether the type is a base type
    bool isBaseType() const;

    bool isAny() const;

    bool isInt() const;
    bool isString() const;
    bool isReal() const;

    TypeEnumMaker* newBaseTypeEnumMaker() const;

    //!@ custom types
    bool isClass() const;
    const Type& classInheritance() const;
    const FunctionSignature& defaultApplySignature() const;
    const FunctionSignatureVector& classProtocol() const;

    //!@ custom types
    bool isType() const;
    const Type& typeInheritance() const;
    const FunctionSignatureVector& typeProtocol() const;


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


    //!@ measure types
    bool isMeasure() const;
    const Type& measureBaseType() const;


    //!@ has constraints?
    bool hasConstraints() const;
    const TypeConstVector& constraints() const;

    bool hasGenerics() const;
    const TypeVector& generics() const;
    Type replaceGenerics(const TypeCtx& typeMap) const;

    bool isRef() const;
    String typeName() const;

    String toString() const;

    //! Types in heather are either 'reference' or 'value' types.  Even if all
    //! objects are possible passed by reference internally, value types
    //! behave on (first) modification as if being copied (copy-on-write).
    bool isValueType() const;
    Type& setIsValueType(bool value);

    static const String kAnyTypeName;
    static const String kBoolTypeName;
    static const String kCharTypeName;
    static const String kDoubleTypeName;
    static const String kEofTypeName;
    static const String kFloatTypeName;
    static const String kIntTypeName;
    static const String kKeywordTypeName;
    static const String kLongDoubleTypeName;
    static const String kLongTypeName;
    static const String kNilTypeName;
    static const String kOctetTypeName;
    static const String kRationalTypeName;
    static const String kRealTypeName;
    static const String kShortTypeName;
    static const String kStringTypeName;
    static const String kULongTypeName;
    static const String kUShortTypeName;
    static const String kUWordTypeName;
    static const String kUnspecifiedTypeName;
    static const String kWordTypeName;

  private:
    Type(TypeKind kind, bool isValue, TypeImpl* impl);

    bool isBuiltinType(const String& name) const;

    TypeKind      fKind;
    bool          fIsValue;
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

    bool isCovariant(const TypeConstraint& other) const;
    bool isContravariant(const TypeConstraint& other) const;
    bool isInvariant(const TypeConstraint& other) const;

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

    //! Indicates whether two function parameters are co-variant.  If this
    //! returns false \p other is either contravariant or invariant.  Use
    //! isInvariant to check.
    bool isCovariant(const FunctionParameter& other) const;
    //! Indicates whether two function parameters are invariant.
    bool isInvariant(const FunctionParameter& other) const;
    bool isContravariant(const FunctionParameter& other) const;


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

    //! Indicates whether two function signatures are co-variant.  If this
    //! returns false \p other is either contravariant or invariant.  Use
    //! isInvariant to check.
    bool isCovariant(const FunctionSignature& other) const;
    //! Indicates whether two function signatures are invariant.
    bool isInvariant(const FunctionSignature& other) const;
    bool isContravariant(const FunctionSignature& other) const;

    //! Indicates whether the signature refers to a generic function.
    bool isGeneric() const;

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


};                              // namespace

#endif                          // bootstrap_type_h
