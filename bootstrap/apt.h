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

  using NodeList = std::vector<Ptr<AptNode>>;

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

  //! Encodes the possible value conversion kinds.
  enum TypeConvKind
  {
    kNoConv,                    //!< Requires no conversion
    kTypeCheckConv,             //!< Prob. req. no conv., but runtime check
    kAtom2PlainConv,            //!< Convert atom to plain value repr.
    kPlain2AtomConv,            //!< Convert plain to atom value repr.
  };

  zstring convkind2str(TypeConvKind kind);

  //--------------------------------------------------------------------------

  //! AptNode is the base of all abstract parse tree nodes.
  class AptNode : public RefCountable
  {
  public:
    AptNode(const SrcPos& srcpos);
    AptNode(const SrcPos& srcpos, const Type& type);

    //! Returns the source position where the code for this node was seen in
    //! the source file.  For synthesiyed nodes this may point to the nearest
    //! likely position.
    const SrcPos& srcpos() const;

    //! Returns the captured scope for this node.  The scope is only available
    //! after the \c Annotator pass has been applied.
    std::shared_ptr<Scope> scope() const;
    //! Set the captured scope.
    AptNode* setScope(std::shared_ptr<Scope> scope);

    //! Returns the type of this node.  Only available after \c Typifier pass
    //! has been applied.
    const Type& type() const;
    //! Set the type of this node.
    void setType(const Type& type);

    const Type& dstType() const;
    void setDstType(const Type& type);

    //! Returns the type conversion.  Only available after \c Typifier pass
    //! has been applied.
    TypeConvKind typeConv() const;
    //! Set the type conversion.
    void setTypeConv(TypeConvKind typeConv);

    //! Indicates whether this node is in tail call position.  Only available
    //! after \c Annotater pass has run.
    bool isInTailPos() const;
    //! Set whether this node is in tail call position.
    void setIsInTailPos(bool value);
    //! Indicates whether this node is in a position where an unambiguous
    //! single type is required.  This is normally only required for types
    //! with various evaluation branches in tail positions, where the compiler
    //! can't decide which of the evaluation branches will be chosen at
    //! runtime.  In positions where the return value is ignored this may not
    //! be an issue.
    //!
    //! Only available after \c Annotater pass has run.
    bool isSingleTypeRequired() const;
    //! Set whether this node is in a position where an unambiguous single
    //! type is required.
    void setIsSingleTypeRequired(bool value);

    //! Returns a (deep) copy of this node.
    virtual AptNode* clone() const = 0;

    //! Render a (debug) representation of this node using \p renderer.  For
    //! details see \c XmlRenderer.
    virtual void render(XmlRenderer* renderer) const = 0;

    //! Generate (binary) code for this node using \p generator.  For details
    //! see \p CodeGenerator.
    virtual llvm::Value* codegen(CodeGenerator* generator) const;

    //! Annotate this node using \p annotator.  For details see \c Annotator.
    virtual void annotate(Annotator* annotator) = 0;
    //! Apply the \c Traversator pass to this node.  For details see \c
    //! Traversator.
    virtual void traverse(Traversator* traversator) = 0;

    //! Transform this node using \p transformator.
    virtual AptNode* transform(Transformator* annotator) = 0;

    //! Apply the \c Typifier pass to this node.
    virtual void typify(Typifier& typifier) = 0;

    //! Dump an xml output to stderr.
    virtual void dump() const;

  protected:
    SrcPos       fSrcPos;
    std::shared_ptr<Scope>   fScope;
    Type         fType;
    Type         fDstType;
    TypeConvKind fTypeConvKind;
    bool         fIsInTailPos;
    bool         fIsSingleTypeRequired;
  };


  //--------------------------------------------------------------------------

  //! Mixin class to add support to \c AptNode subclasses as being part of a
  //! loop transformation.  All nodes having the same loopId belong to the
  //! same compiled loop construct.
  //!
  //! This is a kind of hack to help the \c Typifier identifier to find
  //! temporary variables and assigns for decomposed loop expressions.

  class LoopAnnotatable
  {
  public:
    int loopId() const;
    void setLoopId(int loopId);

  protected:
    LoopAnnotatable();

    int fLoopId;
  };


  //! Mixin class to add support for delayed type specification.  This is used
  //! on \c VardefNode and \c AssignNode to flag the variable type to be
  //! inferred not from the (probably undefined init expression) but the first
  //! assign to come.
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

  //! Base APT class which adds children functionality to \c AptNode.
  class ListNode : public AptNode
  {
  public:
    ListNode(const SrcPos& srcpos);

    //! Returns a mutable list of the receiver's children.  Note that the
    //! notion 'children' depends on the specific subclass.
    NodeList& children();

    //! Returns a readonly list of the receiver's children.  Note that the
    //! notion 'children' depends on the specific subclass.
    const NodeList& children() const;

    //! Appends a node to the end of children.  \p node should not be in the
    //! list of children yet.
    virtual void appendNode(AptNode* node);
    //! Appends a list of nodes to the end of children.  Neither of \p nodes
    //! should be in the list of children yet.
    virtual void appendNodes(const NodeList& nodes);

  protected:
    NodeList   fChildren;
  };


  //--------------------------------------------------------------------------

  //! Represents an undefined value.
  //!
  //! This is only used for delayed variable initialization and marks that the
  //! (local) variable is not to be initialized at all.  Normally this is not
  //! desirable, but when the compiler can prove that the variable is not
  //! accessed before an following assignment, it does not need to generate
  //! any initialization code.  Ultimatively this helps in delaying the type
  //! defering for the variable.  See \c DelayTypeAnnotatable.
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
    virtual void typify(Typifier& typifier);
  };


  //--------------------------------------------------------------------------

  //! Interface which adds a naming protocol.
  class NamedNode
  {
  public:
    virtual ~NamedNode() { };
    virtual const String& name() const = 0;
  };


  //--------------------------------------------------------------------------

  //! Node representing a constant string.

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
    virtual void typify(Typifier& typifier);

    //! Returns the (unicode) string value.
    const String& value() const;

  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  //! Node representing a constant keyword.

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
    virtual void typify(Typifier& typifier);

    //! Returns the keyword value.
    const String& value() const;

  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  //! Encodes the various kinds of entities a symbol in the language can refer
  //! to.
  enum SymReferType
  {
    kFreeVar,                   //!< A free (=undefined) variable
    kGlobalVar,                 //!< A global variable
    kLocalVar,                  //!< A local variable
    kParam,                     //!< A (function) parameter
    kSlot,                      //!< A (class) slot
    kFunction,                  //!< A function
    kGeneric,                   //!< A generic (=type parameter)
    kType,                      //!< A type
  };

  //! Represents a symbol.
  //!
  //! Symbols can be variable names, function names, language keywords, and
  //! alike.  Check \c refersTo() to get the kind of symbol.
  class SymbolNode : public AptNode, public LoopAnnotatable,
                     public LinkableSymbol
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);
    SymbolNode(const SrcPos& srcpos, const String& value,
               const TypeVector& generics);

    virtual SymbolNode* clone() const;

    //! Set the symbol's name.
    void setName(const String& nm);
    //! Return the symbol's name.
    const String& name() const;
    //! Return the symbol's name as \c std::string.
    std::string string() const;

    //! If the receiver refer's to a type returns a list of its type
    //! parameters (generics).
    const TypeVector& generics() const;

    //! Indicate to which entity this symbol refers.  See \c SymReferType for
    //! details.  Only available after the \c Annotator pass has been applied.
    SymReferType refersTo() const;
    //! Set the refer type and whether the symbol is refers to a variable
    //! outside of the owning frame (=variable to be closed).
    void setRefersTo(SymReferType type, bool isShared);
    //! Indicates whether this symbol refers to a variable outside of the
    //! owning frame (i.e. is a variable which has to be closed in a closure).
    bool isShared() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* an);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

  private:
    Ptr<AptNode> fTypeNode;
  };


  //--------------------------------------------------------------------------

  //! Represents a reference to a type expression.
  //!
  //! This node is used when an expression refers to a Type as first-class
  //! entity, e.g. in allocate expressions.
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
    virtual void typify(Typifier& typifier);
  };


  //--------------------------------------------------------------------------

  //! Base class for constant numbers
  class BaseNumberNode : public AptNode
  {
  protected:
    BaseNumberNode(const SrcPos& srcpos, bool isImaginary, const Type& type);

  public:
    //! Indicates whether the number is an imaginary constant number.
    bool isImaginary() const;

  protected:
    bool fIsImaginary;
  };


  //--------------------------------------------------------------------------

  //! Templated base class for constant numbers.
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
    //! Return the value of the number constant.
    T value() const
    {
      return fValue;
    }

  protected:
    T fValue;
  };


  //--------------------------------------------------------------------------

  //! Represents a constant integer number.
  //!
  //! This node is used for all bit widths of integers (8, 16, 32, 64) and
  //! both for signed and unsigned values.  The effective type is indicated by
  //! \p type to the constructor.

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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

    bool value() const;

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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

    using SelectMappingVector = std::vector<SelectMapping>;


    SelectNode(const SrcPos& srcpos,
               AptNode* test, AptNode* comparator);

    virtual SelectNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier& typifier);

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

    using MatchMappingVector = std::vector<MatchMapping>;


    MatchNode(const SrcPos& srcpos, AptNode* expr);

    virtual MatchNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual void traverse(Traversator* traversator);
    virtual AptNode* transform(Transformator* annotator);
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);
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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

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
    virtual void typify(Typifier& typifier);

  private:
    Ptr<AptNode> fBase;
    String       fSlotName;
  };
};


#endif  // bootstrap_apt_h
