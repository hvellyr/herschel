/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_apt_h
#define bootstrap_apt_h

#include <list>
#include <vector>
#include <map>

#include "refcountable.h"
#include "port.h"
#include "ptr.h"
#include "numbers.h"
#include "parsertypes.h"
#include "type.h"
#include "scope.h"

namespace llvm
{
  class Value;
}

namespace herschel
{
  class AptNode;
  class CodeGenerator;
  class XmlRenderer;
  class Annotator;
  class Transformator;
  class Traversator;
  class Typifier;

  //--------------------------------------------------------------------------

  typedef std::vector<Ptr<AptNode> > NodeList;

  NodeList newNodeList();
  NodeList newNodeList(AptNode* n1);
  NodeList newNodeList(AptNode* n1, AptNode* n2);
  NodeList newNodeList(AptNode* n1, AptNode* n2, AptNode* n3);
  NodeList newNodeList(AptNode* n1, AptNode* n2, AptNode* n3, AptNode* n4);

  NodeList& appendNodes(NodeList& dst, const NodeList& nl);
  AptNode* singletonNodeListOrNull(const NodeList& nl);

  void copyNodes(NodeList* dst, const NodeList* src);
  NodeList copyNodes(const NodeList& src);

  //--------------------------------------------------------------------------

  enum TypeConvKind
  {
    kNoConv,
    kTypeCheckConv,
    kAtom2PlainConv,
    kPlain2AtomConv,
  };

  //! AptNode is the base of all abstract part tree nodes.  Since it is a
  //! refcounted object, keep it always in Ptr<>.
  class AptNode : public RefCountable
  {
  public:
    AptNode(const SrcPos& srcpos);
    AptNode(const SrcPos& srcpos, const Type& type);

    //! Returns the source position where the code for this node was seen in
    //! the source file.
    const SrcPos& srcpos() const;

    //! Returns the captured scope for this node.
    Scope* scope() const;
    //! Set the captured scope.
    AptNode* setScope(Scope* scope);

    const Type& type() const;
    void setType(const Type& type);

    const Type& dstType() const;
    void setDstType(const Type& type);
    TypeConvKind typeConv() const;
    void setTypeConv(TypeConvKind typeConv);

    bool isInTailPos() const;
    void setIsInTailPos(bool value);
    bool isSingleTypeRequired() const;
    void setIsSingleTypeRequired(bool value);

    //! Returns a (deep) copy of this node.
    virtual AptNode* clone() const = 0;

    //! Render a (debug) representation of this node using \p renderer.
    virtual void render(XmlRenderer* renderer) const = 0;

    //! Generate (binary) code for this node using \p generator.  For details
    //! see \p CodeGenerator.
    virtual llvm::Value* codegen(CodeGenerator* generator) const;

    //! Annotate this node using \p annotator.  For details see \p Annotator.
    virtual void annotate(Annotator* annotator) = 0;
    virtual void traverse(Traversator* traversator) = 0;

    //! Transform this node using \p transformator.
    virtual AptNode* transform(Transformator* annotator) = 0;
    virtual void typify(Typifier* typifier) = 0;

  protected:
    SrcPos       fSrcPos;
    Ptr<Scope>   fScope;
    Type         fType;
    Type         fDstType;
    TypeConvKind fTypeConvKind;
    bool         fIsInTailPos;
    bool         fIsSingleTypeRequired;
  };


  //--------------------------------------------------------------------------

  //! Mixin class to add support to AptNodes as being part of a loop
  //! transformation.  All nodes having the same loopId belong to the same
  //! compiled loop construct.
  class LoopAnnotatable
  {
  public:
    int loopId() const;
    void setLoopId(int loopId);

  protected:
    LoopAnnotatable();

    int fLoopId;
  };


  //! Mixin class to add support for delayed type speciation.  This is used on
  //! VardefNodes and AssignNodes to flag the variable type to be inferred not
  //! from the (probably undefined init expression) but the first assign to
  //! come.
  class DelayTypeAnnotatable
  {
  public:
    bool isTypeSpecDelayed() const;
    void setTypeSpecDelayed(bool value);

  protected:
    DelayTypeAnnotatable();

    bool fDelayTypeSpec;
  };


  //! Mixin class to add support for linkage specification.  Special type
  //! predicate is implemented for 'C' linkage.
  class LinkableSymbol
  {
  public:
    const String& linkage() const;
    void setLinkage(const String& linkage);

    bool hasCLinkage() const;

  protected:
    String fLinkage;
  };


  //--------------------------------------------------------------------------

  class ListNode : public AptNode
  {
  public:
    ListNode(const SrcPos& srcpos);

    //! Returns a changeable list of the receiver's children.  Node that the
    //! notion 'children' depends on the specific subclass of this.
    NodeList& children();

    //! Returns a readonly list of the receiver's children.  Node that the
    //! notion 'children' depends on the specific subclass of this.
    const NodeList& children() const;

    virtual void appendNode(AptNode* node);
    virtual void appendNodes(const NodeList& nodes);

  protected:
    NodeList   fChildren;
  };


  //--------------------------------------------------------------------------

  //! Represents an undefined value.
  //!
  //!  This is only used for delayed variable initialization and marks that
  //! the (local) variable is not to be initialized at all.  Normally this is
  //! not desirable, but when the compiler can prove that the variable is not
  //! accessed before an following assignment, it does not need to generate an
  //! initialization.  Ultimatively this helps in delaying the type defering
  //! for the variable.
  class UndefNode : public AptNode
  {
  public:
    UndefNode();

    virtual UndefNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class NamedNode
  {
  public:
    virtual ~NamedNode() { };
    virtual const String& name() const = 0;
  };


  //--------------------------------------------------------------------------

  class StringNode : public AptNode
  {
  public:
    StringNode(const SrcPos& srcpos, const String& value);

    virtual StringNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const String& value() const;

  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const SrcPos& srcpos, const String& value);

    virtual KeywordNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const String& value() const;

  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  enum SymReferType
  {
    kFreeVar,
    kGlobalVar,
    kLocalVar,
    kParam,
    kSlot,
    kFunction,
    kGeneric,
    kType,
  };

  class SymbolNode : public AptNode, public LoopAnnotatable,
                     public LinkableSymbol
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);
    SymbolNode(const SrcPos& srcpos, const String& value,
               const TypeVector& generics);

    virtual SymbolNode* clone() const;

    void setName(const String& nm);
    const String& name() const;
    std::string string() const;

    const TypeVector& generics() const;

    SymReferType refersTo() const;
    void setRefersTo(SymReferType type, bool isShared);
    bool isShared() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* an);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  protected:
    String       fValue;
    TypeVector   fGenerics;
    SymReferType fRefersTo;
    bool         fIsShared;     // refers to a variable outside of owning
                                // frame (= closed variable)
  };


  class ArrayTypeNode : public AptNode
  {
  public:
    ArrayTypeNode(const SrcPos& srcpos, AptNode* typeNode);

    AptNode* typeNode() const;

    virtual ArrayTypeNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fTypeNode;
  };


  //--------------------------------------------------------------------------

  class TypeNode : public AptNode
  {
  public:
    TypeNode(const SrcPos& srcpos, const Type& type);

    virtual TypeNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class BaseNumberNode : public AptNode
  {
  protected:
    BaseNumberNode(const SrcPos& srcpos, bool isImaginary, const Type& type);

  public:
    bool isImaginary() const;

  protected:
    bool fIsImaginary;
  };


  //--------------------------------------------------------------------------

  template<typename T>
  class NumberNode : public BaseNumberNode
  {
  protected:
    NumberNode(const SrcPos& srcpos, T value,
               bool isImaginary, const Type& type)
      : BaseNumberNode(srcpos, isImaginary, type),
        fValue(value)
    { }

  public:
    T value() const
    {
      return fValue;
    }

  protected:
    T fValue;
  };


  //--------------------------------------------------------------------------

  class IntNode : public NumberNode<int64_t>
  {
  public:
    IntNode(const SrcPos& srcpos, int64_t value, bool isImaginary,
            const Type& type);
    virtual IntNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, double value,
             bool isImaginary, const Type& type);
    virtual RealNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos, const Rational& value, bool isImaginary,
                 const Type& type);
    virtual RationalNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Char value);
    virtual CharNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    Char value() const;

  private:
    Char fValue;
  };


  //--------------------------------------------------------------------------

  class BoolNode : public AptNode
  {
  public:
    BoolNode(const SrcPos& srcpos, bool value);
    virtual BoolNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    Char value() const;

  private:
    bool fValue;
  };


  //--------------------------------------------------------------------------

  class UnitConstNode : public AptNode
  {
  public:
    UnitConstNode(const SrcPos& srcpos, AptNode* value,
                  const TypeUnit& unit);
    virtual UnitConstNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    AptNode* value() const;
    void setValue(AptNode* node);
    TypeUnit unit() const;

  private:
    Ptr<AptNode> fValue;
    TypeUnit     fUnit;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public ListNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos);
    virtual CompileUnitNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, AptNode* defined);

    AptNode* defNode() const;
    void setDefNode(AptNode* val);

  protected:
    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode, public LoopAnnotatable
  {
  public:
    LetNode(AptNode* node);
    virtual LetNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(AptNode* node);
    virtual DefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  enum BindingAllocType
  {
    kAlloc_Local,
    kAlloc_Shared               // variable is taken by closure
  };

  class BindingNode : public AptNode, public NamedNode
  {
  public:
    BindingNode(const SrcPos& srcpos,
                const String& symbolName, const Type& type,
                AptNode* initExpr);

    AptNode* initExpr() const;
    void setInitExpr(AptNode* val);
    virtual const String& name() const;

    void setAllocType(BindingAllocType type);
    BindingAllocType allocType() const;

  protected:
    String       fSymbolName;
    Ptr<AptNode> fInitExpr;
    BindingAllocType fAllocType;
  };


  enum VardefFlags {
    kNormalVar,
    kConstVar,
    kConfigVar,
    kEnumVar
  };

  class VardefNode : public BindingNode,
                     public DelayTypeAnnotatable,
                     public LinkableSymbol
  {
  public:
    VardefNode(const SrcPos& srcpos,
               const String& symbolName, VardefFlags flags,
               bool isLocal,
               const Type& type, AptNode* initExpr);

    virtual VardefNode* clone() const;

    bool isEnum() const;
    bool isConst() const;
    bool isConfig() const;
    bool isLocal() const;

    VardefFlags flags() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    bool fIsLocal;
    VardefFlags fFlags;
  };


  enum ParamFlags {
    kPosArg   = 1 << 0,
    kSpecArg  = 1 << 1,
    kNamedArg = 1 << 2,
    kRestArg  = 1 << 3
  };


  class ParamNode : public BindingNode
  {
  public:
    ParamNode(const SrcPos& srcpos,
              const String& keyName,
              const String& symbolName, ParamFlags flags,
              const Type& type, AptNode* initExpr);

    virtual ParamNode* clone() const;

    ParamFlags flags() const;
    const String& key() const;

    //! indicates whether this is a position parameter.
    bool isPositional() const;

    //! indicates whether this is a rest parameter ("...").
    bool isRestArg() const;

    //! Indicates whether this is a specialized parameter ("@").
    bool isSpecArg() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    String fKey;
    ParamFlags fFlags;
  };


  class SlotdefNode : public BindingNode
  {
  public:
    SlotdefNode(const SrcPos& srcpos,
                const String& symbolName,
                unsigned int flags,
                const Type& type, AptNode* initExpr);

    virtual SlotdefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    unsigned int flags() const;
    bool isAuto() const;

  private:
    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ArrayNode : public ListNode
  {
  public:
    ArrayNode(const SrcPos& srcpos);

    virtual ArrayNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class VectorNode : public ListNode
  {
  public:
    VectorNode(const SrcPos& srcpos);

    virtual VectorNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class DictNode : public ListNode
  {
  public:
    DictNode(const SrcPos& srcpos);

    void addPair(AptNode* key, AptNode* value);

    virtual DictNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos,
               AptNode* left, OperatorType op, AptNode* right);

    OperatorType op() const;
    AptNode* left() const;
    AptNode* right() const;

    void setLeft(AptNode* node);
    void setRight(AptNode* node);

    bool isMapTo() const;

    virtual BinaryNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fLeft;
    Ptr<AptNode> fRight;
    OperatorType fOp;
  };


  //--------------------------------------------------------------------------

  enum UnaryOperatorType
  {
    kUnaryOpInvalid,

    kUnaryOpNegate,
    kUnaryOpNot
  };

  class UnaryNode : public AptNode
  {
  public:
    UnaryNode(const SrcPos& srcpos, UnaryOperatorType op, AptNode* base);

    AptNode* base() const;
    void setBase(AptNode* base);

    UnaryOperatorType op() const;

    virtual UnaryNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fBase;
    UnaryOperatorType fOp;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              AptNode* from, AptNode* to, AptNode* by);

    AptNode* from() const;
    void setFrom(AptNode* node);
    AptNode* to() const;
    void setTo(AptNode* node);
    AptNode* by() const;
    void setBy(AptNode* node);

    virtual RangeNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fFrom;
    Ptr<AptNode> fTo;
    Ptr<AptNode> fBy;
  };


  //--------------------------------------------------------------------------

  class AssignNode : public AptNode,
                     public LoopAnnotatable,
                     public DelayTypeAnnotatable
  {
  public:
    AssignNode(const SrcPos& srcpos,
               AptNode* lvalue, AptNode* rvalue);

    AptNode* lvalue() const;
    AptNode* rvalue() const;
    void setLvalue(AptNode*);
    void setRvalue(AptNode* );

    virtual AssignNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fLValue;
    Ptr<AptNode> fRValue;
  };


  //--------------------------------------------------------------------------

  class IfNode : public AptNode
  {
  public:
    IfNode(const SrcPos& srcpos,
           AptNode* test, AptNode* consequent, AptNode* alternate);

    virtual IfNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    AptNode* test() const;
    void setTest(AptNode* node);
    AptNode* consequent() const;
    void setConsequent(AptNode* node);
    AptNode* alternate() const;
    void setAlternate(AptNode* node);

  private:
    Ptr<AptNode> fTest;
    Ptr<AptNode> fConsequent;
    Ptr<AptNode> fAlternate;
  };


  //--------------------------------------------------------------------------

  class SelectNode : public AptNode
  {
  public:
    struct SelectMapping
    {
      SelectMapping(const NodeList& values, AptNode* consequent);
      SelectMapping(const SelectMapping& other);

      // if fTestValues is empty this is an else branch
      NodeList     fTestValues;
      Ptr<AptNode> fConsequent;
    };

    typedef std::vector<SelectMapping> SelectMappingVector;


    SelectNode(const SrcPos& srcpos,
               AptNode* test, AptNode* comparator);

    virtual SelectNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    void addMapping(const NodeList& mappings, AptNode* consequent);
    void addMapping(AptNode* mapping, AptNode* consequent);
    void addElseMapping(AptNode* alternate);

    AptNode* test() const;
    void setTest(AptNode* nd);
    AptNode* comparator() const;
    void setComparator(AptNode* nd);
    SelectMappingVector& mappings();
    size_t mappingCount() const;
    const SelectMapping& mappingAt(size_t i) const;

    void setConsequentAt(size_t i, AptNode* consq);
    void setTestValueAt(size_t i, size_t j, AptNode* value);

  private:
    Ptr<AptNode>        fTest;
    Ptr<AptNode>        fComparator;
    SelectMappingVector fMappings;
  };


  //--------------------------------------------------------------------------

  class MatchNode : public AptNode
  {
  public:
    struct MatchMapping
    {
      MatchMapping(const SrcPos& srcpos, const String& varName,
                   const Type& matchType,
                   AptNode* consequent);
      MatchMapping(const MatchMapping& other);

      SrcPos fSrcPos;
      String fVarName;
      Type   fMatchType;
      Ptr<AptNode> fConsequent;
    };

    typedef std::vector<MatchMapping> MatchMappingVector;


    MatchNode(const SrcPos& srcpos, AptNode* expr);

    virtual MatchNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    void addMapping(const SrcPos& srcpos, const String& varName,
                    const Type& matchType,
                    AptNode* consequent);

    AptNode* expr() const;
    void setExpr(AptNode* nd);
    MatchMappingVector& mappings();
    size_t mappingCount() const;
    const MatchMapping& mappingAt(size_t i) const;

    void setConsequentAt(size_t i, AptNode* consq);

  private:
    Ptr<AptNode>       fExpr;
    MatchMappingVector fMappings;
  };


  //--------------------------------------------------------------------------

  class OnNode : public ListNode
  {
  public:
    OnNode(const SrcPos& srcpos,
           const String& key, const NodeList& params, AptNode* body);

    virtual OnNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const String& key() const;
    AptNode* body() const;
    void setBody(AptNode* node);
    const NodeList& params() const;
    NodeList& params();

  private:
    String       fKey;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class BlockNode : public ListNode
  {
  public:
    BlockNode(const SrcPos& srcpos);
    virtual BlockNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);
  };


  //--------------------------------------------------------------------------

  class FunctionNode : public ListNode
  {
  public:
    FunctionNode(const SrcPos&   srcpos,
                 const NodeList& params,
                 const Type&     retType,
                 AptNode*        body);

    virtual FunctionNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const Type& retType() const;
    void setRetType(const Type& type);

    AptNode* body() const;
    void setBody(AptNode* node);
    const NodeList& params() const;
    NodeList& params();

    size_t specializedParamsCount() const;
    bool hasSpecializedParams() const;

  protected:
    Type         fRetType;
    Ptr<AptNode> fBody;
  };


  enum {
    kFuncIsGeneric  = 1 << 0,   // generic function
    kFuncIsAbstract = 1 << 1,   // abstract function definition
    kFuncIsMethod   = 1 << 2,   // generic function implementation
  };


  class FuncDefNode : public FunctionNode, public NamedNode,
                      public LinkableSymbol
  {
  public:
    FuncDefNode(const SrcPos&   srcpos,
                const String&   sym,
                unsigned int    flags,
                const NodeList& params,
                const Type&     retType,
                AptNode*        body);

    virtual FuncDefNode* clone() const;

    virtual const String& name() const;
    bool isGeneric() const;
    bool isMethod() const;
    bool isAbstract() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    bool isAppMain() const;

  private:
    String       fSym;
    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ApplyNode : public ListNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, AptNode* base);

    AptNode* base() const;
    void setBase(AptNode* node);

    //! indicates whether base is a SymbolNode
    bool isSimpleCall() const;
    String simpleCallName() const;

    virtual ApplyNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class KeyargNode : public AptNode
  {
  public:
    KeyargNode(const SrcPos& srcpos,
               const String& key, AptNode* value);

    virtual KeyargNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const String& key() const;
    AptNode* value() const;
    void setValue(AptNode* node);

  private:
    String       fKey;
    Ptr<AptNode> fValue;
  };


  //--------------------------------------------------------------------------

  class WhileNode : public AptNode
  {
  public:
    WhileNode(const SrcPos& srcpos, AptNode* test, AptNode* body);

    virtual WhileNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    AptNode* body() const;
    void setBody(AptNode* node);
    AptNode* test() const;
    void setTest(AptNode* node);

  private:
    Ptr<AptNode> fTest;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class TypeDefNode : public AptNode
  {
  public:
    TypeDefNode(const SrcPos&   srcpos,
                const String&   typeName,
                bool            isClass,
                const Type&     isa,
                const NodeList& params,
                const NodeList& slots,
                const NodeList& onExprs);

    virtual TypeDefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

    const String& name() const;
    const Type& defType() const;
    bool isClass() const;

    const NodeList& params() const;
    const NodeList& slots() const;
    const NodeList& onExprs() const;

    NodeList& params();
    NodeList& slots();
    NodeList& onExprs();

  private:
    String fTypeName;
    bool   fIsClass;
    NodeList fParams;
    NodeList fSlots;
    NodeList fOnExprs;
    Type     fIsa;
  };


  //--------------------------------------------------------------------------

  class CastNode : public AptNode
  {
  public:
    CastNode(const SrcPos& srcpos,
             AptNode* base,
             const Type& type);

    virtual CastNode* clone() const;

    AptNode* base() const;
    void setBase(AptNode* node);

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class SlotRefNode : public AptNode
  {
  public:
    SlotRefNode(const SrcPos& srcpos, AptNode* base, const String& slotName);

    virtual SlotRefNode* clone() const;

    AptNode* base() const;
    void setBase(AptNode* base);
    String slotName() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier* typifier);

  private:
    Ptr<AptNode> fBase;
    String       fSlotName;
  };
};


#endif  // bootstrap_apt_h
