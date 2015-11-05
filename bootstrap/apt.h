/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "numbers.h"
#include "parsertypes.h"
#include "port.h"
#include "scope.h"
#include "type.h"

#include <initializer_list>
#include <list>
#include <map>
#include <vector>

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

  using NodeList = std::vector<std::shared_ptr<AptNode>>;

  NodeList makeNodeList();
  NodeList makeNodeList(std::shared_ptr<AptNode> n1);
  NodeList makeNodeList(std::initializer_list<std::shared_ptr<AptNode>> l);

  NodeList& appendNodes(NodeList& dst, const NodeList& nl);
  std::shared_ptr<AptNode> singletonNodeListOrNull(const NodeList& nl);

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
  class AptNode
  {
  public:
    AptNode(const SrcPos& srcpos);
    AptNode(const SrcPos& srcpos, const Type& type);

    virtual ~AptNode();

    //! Returns the source position where the code for this node was seen in
    //! the source file.  For synthesiyed nodes this may point to the nearest
    //! likely position.
    const SrcPos& srcpos() const;

    //! Returns the captured scope for this node.  The scope is only available
    //! after the \c Annotator pass has been applied.
    std::shared_ptr<Scope> scope() const;
    //! Set the captured scope.
    void setScope(std::shared_ptr<Scope> scope);

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
    virtual std::shared_ptr<AptNode> clone() const = 0;

    //! Render a (debug) representation of this node using \p renderer.  For
    //! details see \c XmlRenderer.
    virtual void render(XmlRenderer* renderer) const = 0;

    //! Generate (binary) code for this node using \p generator.  For details
    //! see \p CodeGenerator.
    virtual llvm::Value* codegen(CodeGenerator* generator) const;

    //! Annotate this node using \p annotator.  For details see \c Annotator.
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd) = 0;
    //! Apply the \c Traversator pass to this node.  For details see \c
    //! Traversator.
    virtual void traverse(Traversator* traversator) = 0;

    //! Transform this node using \p transformator.
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd) = 0;

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
    virtual void appendNode(std::shared_ptr<AptNode> node);
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

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<UndefNode>
  makeUndefNode()
  {
    return std::make_shared<UndefNode>();
  }


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

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    //! Returns the (unicode) string value.
    const String& value() const;

  private:
    String fValue;
  };

  inline std::shared_ptr<StringNode>
  makeStringNode(const SrcPos& srcpos, const String& value)
  {
    return std::make_shared<StringNode>(srcpos, value);
  }


  //--------------------------------------------------------------------------

  //! Node representing a constant keyword.

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const SrcPos& srcpos, const String& value);

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    //! Returns the keyword value.
    const String& value() const;

  private:
    String fValue;
  };

  inline std::shared_ptr<KeywordNode>
  makeKeywordNode(const SrcPos& srcpos, const String& value)
  {
    return std::make_shared<KeywordNode>(srcpos, value);
  }


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

    virtual std::shared_ptr<AptNode> clone() const;

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
    virtual void annotate(Annotator* an, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  protected:
    String       fValue;
    TypeVector   fGenerics;
    SymReferType fRefersTo;
    bool         fIsShared;     // refers to a variable outside of owning
                                // frame (= closed variable)
  };

  inline std::shared_ptr<SymbolNode>
  makeSymbolNode(const SrcPos& srcpos, const String& value,
                 const TypeVector& generics = {})
  {
    return std::make_shared<SymbolNode>(srcpos, value, generics);
  }


  class ArrayTypeNode : public AptNode
  {
  public:
    ArrayTypeNode(const SrcPos& srcpos, std::shared_ptr<AptNode> typeNode);

    std::shared_ptr<AptNode> typeNode() const;

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fTypeNode;
  };

  inline std::shared_ptr<ArrayTypeNode>
  makeArrayTypeNode(const SrcPos& srcpos, std::shared_ptr<AptNode> typeNode)
  {
    return std::make_shared<ArrayTypeNode>(srcpos, std::move(typeNode));
  }


  //--------------------------------------------------------------------------

  //! Represents a reference to a type expression.
  //!
  //! This node is used when an expression refers to a Type as first-class
  //! entity, e.g. in allocate expressions.
  class TypeNode : public AptNode
  {
  public:
    TypeNode(const SrcPos& srcpos, const Type& type);

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<TypeNode>
  makeTypeNode(const SrcPos& srcpos, const Type& type)
  {
    return std::make_shared<TypeNode>(srcpos, type);
  }


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
    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<IntNode>
  makeIntNode(const SrcPos& srcpos, int64_t value, bool isImaginary,
              const Type& type)
  {
    return std::make_shared<IntNode>(srcpos, value, isImaginary, type);
  }


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, double value,
             bool isImaginary, const Type& type);
    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<RealNode>
  makeRealNode(const SrcPos& srcpos, double value, bool isImaginary,
               const Type& type)
  {
    return std::make_shared<RealNode>(srcpos, value, isImaginary, type);
  }


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos, const Rational& value, bool isImaginary,
                 const Type& type);
    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<RationalNode>
  makeRationalNode(const SrcPos& srcpos, const Rational& value,
                   bool isImaginary, const Type& type)
  {
    return std::make_shared<RationalNode>(srcpos, value, isImaginary, type);
  }


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Char value);
    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    Char value() const;

  private:
    Char fValue;
  };

  inline std::shared_ptr<CharNode>
  makeCharNode(const SrcPos& srcpos, Char value)
  {
    return std::make_shared<CharNode>(srcpos, value);
  }


  //--------------------------------------------------------------------------

  class BoolNode : public AptNode
  {
  public:
    BoolNode(const SrcPos& srcpos, bool value);
    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    bool value() const;

  private:
    bool fValue;
  };

  inline std::shared_ptr<BoolNode>
  makeBoolNode(const SrcPos& srcpos, bool value)
  {
    return std::make_shared<BoolNode>(srcpos, value);
  }


  //--------------------------------------------------------------------------

  class UnitConstNode : public AptNode
  {
  public:
    UnitConstNode(const SrcPos& srcpos, std::shared_ptr<AptNode> value,
                  const TypeUnit& unit);
    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    std::shared_ptr<AptNode> value() const;
    void setValue(std::shared_ptr<AptNode> node);
    TypeUnit unit() const;

  private:
    std::shared_ptr<AptNode> fValue;
    TypeUnit     fUnit;
  };

  inline std::shared_ptr<UnitConstNode>
  makeUnitConstNode(const SrcPos& srcpos, std::shared_ptr<AptNode> value,
                    const TypeUnit& unit)
  {
    return std::make_shared<UnitConstNode>(srcpos, std::move(value), unit);
  }


  //--------------------------------------------------------------------------

  class CompileUnitNode : public ListNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos);

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<CompileUnitNode>
  makeCompileUnitNode(const SrcPos& srcpos)
  {
    return std::make_shared<CompileUnitNode>(srcpos);
  }


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, std::shared_ptr<AptNode> defined);

    std::shared_ptr<AptNode> defNode() const;
    void setDefNode(std::shared_ptr<AptNode> val);

  protected:
    std::shared_ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode, public LoopAnnotatable
  {
  public:
    LetNode(std::shared_ptr<AptNode> node);

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<LetNode>
  makeLetNode(std::shared_ptr<AptNode> node)
  {
    return std::make_shared<LetNode>(std::move(node));
  }


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(std::shared_ptr<AptNode> node);

    virtual std::shared_ptr<AptNode> clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<DefNode>
  makeDefNode(std::shared_ptr<AptNode> node)
  {
    return std::make_shared<DefNode>(std::move(node));
  }


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
                std::shared_ptr<AptNode> initExpr);

    std::shared_ptr<AptNode> initExpr() const;
    void setInitExpr(std::shared_ptr<AptNode> val);
    virtual const String& name() const;

    void setAllocType(BindingAllocType type);
    BindingAllocType allocType() const;

  protected:
    String       fSymbolName;
    std::shared_ptr<AptNode> fInitExpr;
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
               const Type& type, std::shared_ptr<AptNode> initExpr);

    virtual std::shared_ptr<AptNode> clone() const;

    bool isEnum() const;
    bool isConst() const;
    bool isConfig() const;
    bool isLocal() const;

    VardefFlags flags() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    bool fIsLocal;
    VardefFlags fFlags;
  };

  inline std::shared_ptr<VardefNode>
  makeVardefNode(const SrcPos& srcpos, const String& symbolName,
                 VardefFlags flags, bool isLocal, const Type& type,
                 std::shared_ptr<AptNode> initExpr)
  {
    return std::make_shared<VardefNode>(srcpos, symbolName, flags,
                                        isLocal, type, std::move(initExpr));
  }


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
              const Type& type, std::shared_ptr<AptNode> initExpr);

    virtual std::shared_ptr<AptNode> clone() const;

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
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    String fKey;
    ParamFlags fFlags;
  };

  inline std::shared_ptr<ParamNode>
  makeParamNode(const SrcPos& srcpos, const String& keyName,
                const String& symbolName, ParamFlags flags, const Type& type,
                std::shared_ptr<AptNode> initExpr)
  {
    return std::make_shared<ParamNode>(srcpos, keyName, symbolName, flags,
                                       type, std::move(initExpr));
  }


  class SlotdefNode : public BindingNode
  {
  public:
    SlotdefNode(const SrcPos& srcpos,
                const String& symbolName,
                unsigned int flags,
                const Type& type, std::shared_ptr<AptNode> initExpr);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    unsigned int flags() const;
    bool isAuto() const;

  private:
    unsigned int fFlags;
  };

  inline std::shared_ptr<SlotdefNode>
  makeSlotdefNode(const SrcPos& srcpos, const String& symbolName,
                  unsigned int flags, const Type& type,
                  std::shared_ptr<AptNode> initExpr)
  {
    return std::make_shared<SlotdefNode>(srcpos, symbolName, flags,
                                         type, std::move(initExpr));
  }


  //--------------------------------------------------------------------------

  class ArrayNode : public ListNode
  {
  public:
    ArrayNode(const SrcPos& srcpos);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<ArrayNode>
  makeArrayNode(const SrcPos& srcpos)
  {
    return std::make_shared<ArrayNode>(srcpos);
  }


  //--------------------------------------------------------------------------

  class VectorNode : public ListNode
  {
  public:
    VectorNode(const SrcPos& srcpos);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<VectorNode>
  makeVectorNode(const SrcPos& srcpos)
  {
    return std::make_shared<VectorNode>(srcpos);
  }


  //--------------------------------------------------------------------------

  class DictNode : public ListNode
  {
  public:
    DictNode(const SrcPos& srcpos);

    void addPair(std::shared_ptr<AptNode> key, std::shared_ptr<AptNode> value);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<DictNode>
  makeDictNode(const SrcPos& srcpos)
  {
    return std::make_shared<DictNode>(srcpos);
  }


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos,
               std::shared_ptr<AptNode> left, OperatorType op,
               std::shared_ptr<AptNode> right);

    OperatorType op() const;
    std::shared_ptr<AptNode> left() const;
    std::shared_ptr<AptNode> right() const;

    void setLeft(std::shared_ptr<AptNode> node);
    void setRight(std::shared_ptr<AptNode> node);

    bool isMapTo() const;

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fLeft;
    std::shared_ptr<AptNode> fRight;
    OperatorType fOp;
  };

  inline std::shared_ptr<BinaryNode>
  makeBinaryNode(const SrcPos& srcpos, std::shared_ptr<AptNode> left,
                 OperatorType op, std::shared_ptr<AptNode> right)
  {
    return std::make_shared<BinaryNode>(srcpos, std::move(left), op,
                                        std::move(right));
  }


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
    UnaryNode(const SrcPos& srcpos, UnaryOperatorType op,
              std::shared_ptr<AptNode> base);

    std::shared_ptr<AptNode> base() const;
    void setBase(std::shared_ptr<AptNode> base);

    UnaryOperatorType op() const;

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fBase;
    UnaryOperatorType fOp;
  };

  inline std::shared_ptr<UnaryNode>
  makeUnaryNode(const SrcPos& srcpos, UnaryOperatorType op,
                std::shared_ptr<AptNode> base)
  {
    return std::make_shared<UnaryNode>(srcpos, op, std::move(base));
  }


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              std::shared_ptr<AptNode> from, std::shared_ptr<AptNode> to,
              std::shared_ptr<AptNode> by);

    std::shared_ptr<AptNode> from() const;
    void setFrom(std::shared_ptr<AptNode> node);
    std::shared_ptr<AptNode> to() const;
    void setTo(std::shared_ptr<AptNode> node);
    std::shared_ptr<AptNode> by() const;
    void setBy(std::shared_ptr<AptNode> node);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fFrom;
    std::shared_ptr<AptNode> fTo;
    std::shared_ptr<AptNode> fBy;
  };

  inline std::shared_ptr<RangeNode>
  makeRangeNode(const SrcPos& srcpos, std::shared_ptr<AptNode> from,
                std::shared_ptr<AptNode> to, std::shared_ptr<AptNode> by)
  {
    return std::make_shared<RangeNode>(srcpos, std::move(from), std::move(to),
                                       std::move(by));
  }


  //--------------------------------------------------------------------------

  class AssignNode : public AptNode,
                     public LoopAnnotatable,
                     public DelayTypeAnnotatable
  {
  public:
    AssignNode(const SrcPos& srcpos,
               std::shared_ptr<AptNode> lvalue,
               std::shared_ptr<AptNode> rvalue);

    std::shared_ptr<AptNode> lvalue() const;
    std::shared_ptr<AptNode> rvalue() const;
    void setLvalue(std::shared_ptr<AptNode>);
    void setRvalue(std::shared_ptr<AptNode> );

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fLValue;
    std::shared_ptr<AptNode> fRValue;
  };

  inline std::shared_ptr<AssignNode>
  makeAssignNode(const SrcPos& srcpos, std::shared_ptr<AptNode> lvalue,
                 std::shared_ptr<AptNode> rvalue)
  {
    return std::make_shared<AssignNode>(srcpos, std::move(lvalue),
                                        std::move(rvalue));
  }


  //--------------------------------------------------------------------------

  class IfNode : public AptNode
  {
  public:
    IfNode(const SrcPos& srcpos,
           std::shared_ptr<AptNode> test,
           std::shared_ptr<AptNode> consequent,
           std::shared_ptr<AptNode> alternate);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    std::shared_ptr<AptNode> test() const;
    void setTest(std::shared_ptr<AptNode> node);
    std::shared_ptr<AptNode> consequent() const;
    void setConsequent(std::shared_ptr<AptNode> node);
    std::shared_ptr<AptNode> alternate() const;
    void setAlternate(std::shared_ptr<AptNode> node);

  private:
    std::shared_ptr<AptNode> fTest;
    std::shared_ptr<AptNode> fConsequent;
    std::shared_ptr<AptNode> fAlternate;
  };

  inline std::shared_ptr<IfNode>
  makeIfNode(const SrcPos& srcpos,
             std::shared_ptr<AptNode> test,
             std::shared_ptr<AptNode> consequent,
             std::shared_ptr<AptNode> alternate)
  {
    return std::make_shared<IfNode>(srcpos, std::move(test),
                                    std::move(consequent),
                                    std::move(alternate));
  }


  //--------------------------------------------------------------------------

  class SelectNode : public AptNode
  {
  public:
    struct SelectMapping
    {
      SelectMapping(const NodeList& values,
                    std::shared_ptr<AptNode> consequent);
      SelectMapping(const SelectMapping& other);

      // if fTestValues is empty this is an else branch
      NodeList     fTestValues;
      std::shared_ptr<AptNode> fConsequent;
    };

    using SelectMappingVector = std::vector<SelectMapping>;


    SelectNode(const SrcPos& srcpos,
               std::shared_ptr<AptNode> test,
               std::shared_ptr<AptNode> comparator);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    void addMapping(const NodeList& mappings,
                    std::shared_ptr<AptNode> consequent);
    void addMapping(std::shared_ptr<AptNode> mapping,
                    std::shared_ptr<AptNode> consequent);
    void addElseMapping(std::shared_ptr<AptNode> alternate);

    std::shared_ptr<AptNode> test() const;
    void setTest(std::shared_ptr<AptNode> nd);
    std::shared_ptr<AptNode> comparator() const;
    void setComparator(std::shared_ptr<AptNode> nd);
    SelectMappingVector& mappings();
    size_t mappingCount() const;
    const SelectMapping& mappingAt(size_t i) const;

    void setConsequentAt(size_t i, std::shared_ptr<AptNode> consq);
    void setTestValueAt(size_t i, size_t j, std::shared_ptr<AptNode> value);

  private:
    std::shared_ptr<AptNode>        fTest;
    std::shared_ptr<AptNode>        fComparator;
    SelectMappingVector fMappings;
  };

  inline std::shared_ptr<SelectNode>
  makeSelectNode(const SrcPos& srcpos,
                 std::shared_ptr<AptNode> test,
                 std::shared_ptr<AptNode> comparator)
  {
    return std::make_shared<SelectNode>(srcpos, std::move(test),
                                        std::move(comparator));
  }


  //--------------------------------------------------------------------------

  class MatchNode : public AptNode
  {
  public:
    struct MatchMapping
    {
      MatchMapping(const SrcPos& srcpos, const String& varName,
                   const Type& matchType,
                   std::shared_ptr<AptNode> consequent);
      MatchMapping(const MatchMapping& other);

      SrcPos fSrcPos;
      String fVarName;
      Type   fMatchType;
      std::shared_ptr<AptNode> fConsequent;
    };

    using MatchMappingVector = std::vector<MatchMapping>;


    MatchNode(const SrcPos& srcpos, std::shared_ptr<AptNode> expr);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    void addMapping(const SrcPos& srcpos, const String& varName,
                    const Type& matchType,
                    std::shared_ptr<AptNode> consequent);

    std::shared_ptr<AptNode> expr() const;
    void setExpr(std::shared_ptr<AptNode> nd);
    MatchMappingVector& mappings();
    size_t mappingCount() const;
    const MatchMapping& mappingAt(size_t i) const;

    void setConsequentAt(size_t i, std::shared_ptr<AptNode> consq);

  private:
    std::shared_ptr<AptNode>       fExpr;
    MatchMappingVector fMappings;
  };

  inline std::shared_ptr<MatchNode>
  makeMatchNode(const SrcPos& srcpos,
                std::shared_ptr<AptNode> expr)
  {
    return std::make_shared<MatchNode>(srcpos, std::move(expr));
  }


  //--------------------------------------------------------------------------

  class OnNode : public ListNode
  {
  public:
    OnNode(const SrcPos& srcpos,
           const String& key, const NodeList& params,
           std::shared_ptr<AptNode> body);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    const String& key() const;
    std::shared_ptr<AptNode> body() const;
    void setBody(std::shared_ptr<AptNode> node);
    const NodeList& params() const;
    NodeList& params();

  private:
    String       fKey;
    std::shared_ptr<AptNode> fBody;
  };

  inline std::shared_ptr<OnNode>
  makeOnNode(const SrcPos& srcpos, const String& key, const NodeList& params,
             std::shared_ptr<AptNode> body)
  {
    return std::make_shared<OnNode>(srcpos, key, params, std::move(body));
  }


  //--------------------------------------------------------------------------

  class BlockNode : public ListNode
  {
  public:
    BlockNode(const SrcPos& srcpos);
    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);
  };

  inline std::shared_ptr<BlockNode>
  makeBlockNode(const SrcPos& srcpos)
  {
    return std::make_shared<BlockNode>(srcpos);
  }


  //--------------------------------------------------------------------------

  class FunctionNode : public ListNode
  {
  public:
    FunctionNode(const SrcPos&   srcpos,
                 const NodeList& params,
                 const Type&     retType,
                 std::shared_ptr<AptNode>        body);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    const Type& retType() const;
    void setRetType(const Type& type);

    std::shared_ptr<AptNode> body() const;
    void setBody(std::shared_ptr<AptNode> node);
    const NodeList& params() const;
    NodeList& params();

    size_t specializedParamsCount() const;
    bool hasSpecializedParams() const;

  protected:
    Type         fRetType;
    std::shared_ptr<AptNode> fBody;
  };

  inline std::shared_ptr<FunctionNode>
  makeFunctionNode(const SrcPos& srcpos, const NodeList& params,
                   const Type& retType, std::shared_ptr<AptNode> body)
  {
    return std::make_shared<FunctionNode>(srcpos, params, retType,
                                          std::move(body));
  }


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
                std::shared_ptr<AptNode> body);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual const String& name() const;
    bool isGeneric() const;
    bool isMethod() const;
    bool isAbstract() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    bool isAppMain() const;

  private:
    String       fSym;
    unsigned int fFlags;
  };

  inline std::shared_ptr<FuncDefNode>
  makeFuncDefNode(const SrcPos& srcpos,
                  const String&   sym,
                  unsigned int    flags,
                  const NodeList& params,
                  const Type&     retType,
                  std::shared_ptr<AptNode> body)
  {
    return std::make_shared<FuncDefNode>(srcpos, sym, flags, params,
                                         retType, std::move(body));
  }


  //--------------------------------------------------------------------------

  class ApplyNode : public ListNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base);

    std::shared_ptr<AptNode> base() const;
    void setBase(std::shared_ptr<AptNode> node);

    //! indicates whether base is a SymbolNode
    bool isSimpleCall() const;
    String simpleCallName() const;

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fBase;
  };

  inline std::shared_ptr<ApplyNode>
  makeApplyNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base)
  {
    return std::make_shared<ApplyNode>(srcpos, std::move(base));
  }


  //--------------------------------------------------------------------------

  class KeyargNode : public AptNode
  {
  public:
    KeyargNode(const SrcPos& srcpos,
               const String& key, std::shared_ptr<AptNode> value);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    const String& key() const;
    std::shared_ptr<AptNode> value() const;
    void setValue(std::shared_ptr<AptNode> node);

  private:
    String       fKey;
    std::shared_ptr<AptNode> fValue;
  };

  inline std::shared_ptr<KeyargNode>
  makeKeyargNode(const SrcPos& srcpos, const String& key,
                 std::shared_ptr<AptNode> value)
  {
    return std::make_shared<KeyargNode>(srcpos, key, std::move(value));
  }


  //--------------------------------------------------------------------------

  class WhileNode : public AptNode
  {
  public:
    WhileNode(const SrcPos& srcpos, std::shared_ptr<AptNode> test,
              std::shared_ptr<AptNode> body);

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

    std::shared_ptr<AptNode> body() const;
    void setBody(std::shared_ptr<AptNode> node);
    std::shared_ptr<AptNode> test() const;
    void setTest(std::shared_ptr<AptNode> node);

  private:
    std::shared_ptr<AptNode> fTest;
    std::shared_ptr<AptNode> fBody;
  };

  inline std::shared_ptr<WhileNode>
  makeWhileNode(const SrcPos& srcpos, std::shared_ptr<AptNode> test,
                std::shared_ptr<AptNode> body)
  {
    return std::make_shared<WhileNode>(srcpos, std::move(test),
                                       std::move(body));
  }


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

    virtual std::shared_ptr<AptNode> clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
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

  inline std::shared_ptr<TypeDefNode>
  makeTypeDefNode(const SrcPos&   srcpos,
                  const String&   typeName,
                  bool            isClass,
                  const Type&     isa,
                  const NodeList& params,
                  const NodeList& slots,
                  const NodeList& onExprs)
  {
    return std::make_shared<TypeDefNode>(srcpos, typeName, isClass, isa,
                                         params, slots, onExprs);
  }


  //--------------------------------------------------------------------------

  class CastNode : public AptNode
  {
  public:
    CastNode(const SrcPos& srcpos,
             std::shared_ptr<AptNode> base,
             const Type& type);

    virtual std::shared_ptr<AptNode> clone() const;

    std::shared_ptr<AptNode> base() const;
    void setBase(std::shared_ptr<AptNode> node);

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fBase;
  };

  inline std::shared_ptr<CastNode>
  makeCastNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base,
               const Type& type)
  {
    return std::make_shared<CastNode>(srcpos, std::move(base), type);
  }


  //--------------------------------------------------------------------------

  class SlotRefNode : public AptNode
  {
  public:
    SlotRefNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base,
                const String& slotName);

    virtual std::shared_ptr<AptNode> clone() const;

    std::shared_ptr<AptNode> base() const;
    void setBase(std::shared_ptr<AptNode> base);
    String slotName() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator, std::shared_ptr<AptNode> nd);
    virtual void traverse(Traversator* traversator);
    virtual std::shared_ptr<AptNode> transform(Transformator* annotator,
                                               std::shared_ptr<AptNode> nd);
    virtual void typify(Typifier& typifier);

  private:
    std::shared_ptr<AptNode> fBase;
    String       fSlotName;
  };

  inline std::shared_ptr<SlotRefNode>
  makeSlotRefNode(const SrcPos& srcpos, std::shared_ptr<AptNode> base,
                  const String& slotName)
  {
    return std::make_shared<SlotRefNode>(srcpos, std::move(base), slotName);
  }

} // namespace herschel
