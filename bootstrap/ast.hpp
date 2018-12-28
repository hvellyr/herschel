/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "numbers.hpp"
#include "parsertypes.hpp"
#include "port.hpp"
#include "predefined.hpp"
#include "scope.hpp"
#include "type.hpp"

#include <initializer_list>
#include <list>
#include <map>
#include <vector>


namespace llvm {
class Value;
}

namespace herschel {
class AstNode;


using NodeList = std::vector<std::shared_ptr<AstNode>>;

NodeList makeNodeList();
NodeList makeNodeList(std::shared_ptr<AstNode> n1);
NodeList makeNodeList(std::initializer_list<std::shared_ptr<AstNode>> l);

NodeList& appendNodes(NodeList& dst, const NodeList& nl);
std::shared_ptr<AstNode> singletonNodeListOrNull(const NodeList& nl);


//! Encodes the possible value conversion kinds.
enum TypeConvKind {
  kNoConv,          //!< Requires no conversion
  kTypeCheckConv,   //!< Prob. req. no conv., but runtime check
  kAtom2PlainConv,  //!< Convert atom to plain value repr.
  kPlain2AtomConv,  //!< Convert plain to atom value repr.
};


namespace details {
  template <typename T>
  std::shared_ptr<T> cloneScope(const T* src, std::shared_ptr<T> dst)
  {
    dst->setScope(src->scope());
    dst->setType(src->type());
    dst->setDstType(src->dstType());
    dst->setTypeConv(src->typeConv());
    return dst;
  }

  template <typename T>
  std::shared_ptr<T> nodeClone(std::shared_ptr<T> node)
  {
    if (node)
      return node->clone();
    return nullptr;
  }


  inline void copyNodes(NodeList* dst, const NodeList* src)
  {
    for (auto& s : *src) {
      dst->push_back(nodeClone(s));
    }
  }


  inline NodeList copyNodes(const NodeList& src)
  {
    NodeList dst;
    copyNodes(&dst, &src);
    return dst;
  }
}  // namespace details


//! AstNode is the base of all abstract parse tree nodes.
class AstNode {
public:
  AstNode(const SrcPos& srcpos)
      : fSrcPos(srcpos)
      , fTypeConvKind(kNoConv)
      , fIsInTailPos(false)
      , fIsSingleTypeRequired(false)
  {
  }

  AstNode(const SrcPos& srcpos, const Type& type)
      : fSrcPos(srcpos)
      , fType(type)
      , fTypeConvKind(kNoConv)
      , fIsInTailPos(false)
      , fIsSingleTypeRequired(false)
  {
  }

  virtual ~AstNode() {}

  //! Returns the source position where the code for this node was seen in
  //! the source file.  For synthesiyed nodes this may point to the nearest
  //! likely position.
  const SrcPos& srcpos() const { return fSrcPos; }

  virtual NodeList child_nodes() { return {}; }

  //! Returns the captured scope for this node.  The scope is only available
  //! after the \c Annotator pass has been applied.
  std::shared_ptr<Scope> scope() const { return fScope; }

  //! Set the captured scope.
  void setScope(std::shared_ptr<Scope> scope) { fScope = scope; }

  //! Returns the type of this node.  Only available after \c Typifier pass
  //! has been applied.
  const Type& type() const { return fType; }

  //! Set the type of this node.
  void setType(const Type& type) { fType = type; }


  const Type& dstType() const { return fDstType; }

  void setDstType(const Type& type) { fDstType = type; }

  //! Returns the type conversion.  Only available after \c Typifier pass
  //! has been applied.
  TypeConvKind typeConv() const { return fTypeConvKind; }

  //! Set the type conversion.
  void setTypeConv(TypeConvKind typeConv) { fTypeConvKind = typeConv; }

  //! Indicates whether this node is in tail call position.  Only available
  //! after \c Annotater pass has run.
  bool isInTailPos() const { return fIsInTailPos; }

  //! Set whether this node is in tail call position.
  void setIsInTailPos(bool value) { fIsInTailPos = value; }

  //! Indicates whether this node is in a position where an unambiguous
  //! single type is required.  This is normally only required for types
  //! with various evaluation branches in tail positions, where the compiler
  //! can't decide which of the evaluation branches will be chosen at
  //! runtime.  In positions where the return value is ignored this may not
  //! be an issue.
  //!
  //! Only available after \c Annotater pass has run.
  bool isSingleTypeRequired() const { return fIsSingleTypeRequired; }

  //! Set whether this node is in a position where an unambiguous single
  //! type is required.
  void setIsSingleTypeRequired(bool value) { fIsSingleTypeRequired = value; }

  virtual bool isRemoveable() const { return fIsRemoveable; }
  void setIsRemoveable(bool value) { fIsRemoveable = value; }

  virtual bool isObsolete() const { return fIsObsolete; }
  void setIsObsolete(bool value) { fIsObsolete = value; }

  //! Returns a (deep) copy of this node.
  virtual std::shared_ptr<AstNode> clone() const = 0;

protected:
  SrcPos fSrcPos;
  std::shared_ptr<Scope> fScope;
  Type fType;
  Type fDstType;
  TypeConvKind fTypeConvKind;
  bool fIsInTailPos;
  bool fIsSingleTypeRequired;
  bool fIsRemoveable = false;
  bool fIsObsolete = false;
};


//! Mixin class to add support for delayed type specification.  This is used
//! on \c VardefNode and \c AssignNode to flag the variable type to be
//! inferred not from the (probably undefined init expression) but the first
//! assign to come.
class DelayTypeAnnotatable {
public:
  bool isTypeSpecDelayed() const { return fDelayTypeSpec; }

  void setTypeSpecDelayed(bool value) { fDelayTypeSpec = value; }

protected:
  DelayTypeAnnotatable()
      : fDelayTypeSpec(false)
  {
  }

  bool fDelayTypeSpec;
};


//! Mixin class to add support for linkage specification.  Special type
//! predicate is implemented for 'C' linkage.
class LinkableSymbol {
public:
  const String& linkage() const { return fLinkage; }

  void setLinkage(const String& linkage) { fLinkage = linkage; }

  bool hasCLinkage() const { return fLinkage == String("C"); }

protected:
  String fLinkage;
};


//! Base APT class which adds children functionality to \c AstNode.
class ListNode : public AstNode {
public:
  ListNode(const SrcPos& srcpos)
      : AstNode(srcpos)
  {
  }

  //! Returns a mutable list of the receiver's children.  Note that the
  //! notion 'children' depends on the specific subclass.
  NodeList& children() { return fChildren; }

  //! Returns a readonly list of the receiver's children.  Note that the
  //! notion 'children' depends on the specific subclass.
  const NodeList& children() const { return fChildren; }

  NodeList child_nodes() override { return children(); }


  //! Appends a node to the end of children.  \p node should not be in the
  //! list of children yet.
  void appendNode(std::shared_ptr<AstNode> node) { fChildren.push_back(node); }

  //! Appends a list of nodes to the end of children.  Neither of \p nodes
  //! should be in the list of children yet.
  void appendNodes(const NodeList& nodes)
  {
    if (!nodes.empty())
      fChildren.insert(fChildren.end(), nodes.begin(), nodes.end());
  }

protected:
  NodeList fChildren;
};


//! Represents an undefined value.
//!
//! This is only used for delayed variable initialization and marks that the
//! (local) variable is not to be initialized at all.  Normally this is not
//! desirable, but when the compiler can prove that the variable is not
//! accessed before an following assignment, it does not need to generate
//! any initialization code.  Ultimatively this helps in delaying the type
//! defering for the variable.  See \c DelayTypeAnnotatable.
class UndefNode : public AstNode {
public:
  UndefNode()
      : AstNode(SrcPos())
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return std::make_shared<UndefNode>();
  }
};

inline std::shared_ptr<UndefNode> makeUndefNode()
{
  return std::make_shared<UndefNode>();
}


//! Interface which adds a naming protocol.
class NamedNode {
public:
  virtual ~NamedNode(){};
  virtual const String& name() const = 0;
};


//! Node representing a constant string.
class StringNode : public AstNode {
public:
  StringNode(const SrcPos& srcpos, const String& value)
      : AstNode(srcpos)
      , fValue(value)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<StringNode>(fSrcPos, fValue));
  }

  //! Returns the (unicode) string value.
  const String& value() const { return fValue; }

private:
  String fValue;
};

inline std::shared_ptr<StringNode> makeStringNode(const SrcPos& srcpos,
                                                  const String& value)
{
  return std::make_shared<StringNode>(srcpos, value);
}


//! Node representing a constant keyword.
class KeywordNode : public AstNode {
public:
  KeywordNode(const SrcPos& srcpos, const String& value)
      : AstNode(srcpos)
      , fValue(value)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<KeywordNode>(fSrcPos, fValue));
  }

  //! Returns the keyword value.
  const String& value() const { return fValue; }

private:
  String fValue;
};

inline std::shared_ptr<KeywordNode> makeKeywordNode(const SrcPos& srcpos,
                                                    const String& value)
{
  return std::make_shared<KeywordNode>(srcpos, value);
}


//! Encodes the various kinds of entities a symbol in the language can refer
//! to.
enum SymReferType {
  kFreeVar,    //!< A free (=undefined) variable
  kGlobalVar,  //!< A global variable
  kLocalVar,   //!< A local variable
  kParam,      //!< A (function) parameter
  kSlot,       //!< A (class) slot
  kFunction,   //!< A function
  kGeneric,    //!< A generic (=type parameter)
  kType,       //!< A type
};

//! Represents a symbol.
//!
//! Symbols can be variable names, function names, language keywords, and
//! alike.  Check \c refersTo() to get the kind of symbol.
class SymbolNode : public AstNode, public LinkableSymbol {
public:
  SymbolNode(const SrcPos& srcpos, const String& value)
      : AstNode(srcpos)
      , fValue(value)
      , fRefersTo(kFreeVar)
      , fIsShared(false)
  {
  }

  SymbolNode(const SrcPos& srcpos, const String& value, const TypeVector& generics)
      : AstNode(srcpos)
      , fValue(value)
      , fGenerics(generics)
      , fRefersTo(kFreeVar)
      , fIsShared(false)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto newnd = std::make_shared<SymbolNode>(fSrcPos, fValue);
    newnd->setLinkage(linkage());
    return details::cloneScope(this, newnd);
  }


  //! Set the symbol's name.
  void setName(const String& nm) { fValue = nm; }

  //! Return the symbol's name.
  const String& name() const { return fValue; }

  //! Return the symbol's name as \c std::string.
  std::string string() const { return std::string(StrHelper(fValue)); }

  //! If the receiver refer's to a type returns a list of its type
  //! parameters (generics).
  const TypeVector& generics() const { return fGenerics; }

  //! Indicate to which entity this symbol refers.  See \c SymReferType for
  //! details.  Only available after the \c Annotator pass has been applied.
  SymReferType refersTo() const { return fRefersTo; }

  //! Set the refer type and whether the symbol is refers to a variable
  //! outside of the owning frame (=variable to be closed).
  void setRefersTo(SymReferType type, bool isShared)
  {
    fRefersTo = type;
    fIsShared = isShared;
  }

  //! Indicates whether this symbol refers to a variable outside of the
  //! owning frame (i.e. is a variable which has to be closed in a closure).
  bool isShared() const { return fIsShared; }

protected:
  String fValue;
  TypeVector fGenerics;
  SymReferType fRefersTo;
  bool fIsShared;  // refers to a variable outside of owning
                   // frame (= closed variable)
};

inline std::shared_ptr<SymbolNode>
makeSymbolNode(const SrcPos& srcpos, const String& value, const TypeVector& generics = {})
{
  return std::make_shared<SymbolNode>(srcpos, value, generics);
}


class ArrayTypeNode : public AstNode {
public:
  ArrayTypeNode(const SrcPos& srcpos, std::shared_ptr<AstNode> typeNode)
      : AstNode(srcpos)
      , fTypeNode(std::move(typeNode))
  {
  }

  NodeList child_nodes() override { return { fTypeNode }; }

  std::shared_ptr<AstNode> typeNode() const { return fTypeNode; }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<ArrayTypeNode>(fSrcPos, details::nodeClone(fTypeNode)));
  }

private:
  std::shared_ptr<AstNode> fTypeNode;
};

inline std::shared_ptr<ArrayTypeNode> makeArrayTypeNode(const SrcPos& srcpos,
                                                        std::shared_ptr<AstNode> typeNode)
{
  return std::make_shared<ArrayTypeNode>(srcpos, std::move(typeNode));
}


//! Represents a reference to a type expression.
//!
//! This node is used when an expression refers to a Type as first-class
//! entity, e.g. in allocate expressions.
class TypeNode : public AstNode {
public:
  TypeNode(const SrcPos& srcpos, const Type& type)
      : AstNode(srcpos, type)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<TypeNode>(fSrcPos, fType.clone()));
  }
};

inline std::shared_ptr<TypeNode> makeTypeNode(const SrcPos& srcpos, const Type& type)
{
  return std::make_shared<TypeNode>(srcpos, type);
}


//! Base class for constant numbers
class BaseNumberNode : public AstNode {
protected:
  BaseNumberNode(const SrcPos& srcpos, bool isImaginary, const Type& type)
      : AstNode(srcpos, type)
      , fIsImaginary(isImaginary)
  {
  }

public:
  //! Indicates whether the number is an imaginary constant number.
  bool isImaginary() const { return fIsImaginary; }

protected:
  bool fIsImaginary;
};


//! Templated base class for constant numbers.
template <typename T>
class NumberNode : public BaseNumberNode {
protected:
  NumberNode(const SrcPos& srcpos, T value, bool isImaginary, const Type& type)
      : BaseNumberNode(srcpos, isImaginary, type)
      , fValue(value)
  {
  }

public:
  //! Return the value of the number constant.
  T value() const { return fValue; }

protected:
  T fValue;
};


//! Represents a constant integer number.
//!
//! This node is used for all bit widths of integers (8, 16, 32, 64) and
//! both for signed and unsigned values.  The effective type is indicated by
//! \p type to the constructor.
class IntNode : public NumberNode<int64_t> {
public:
  IntNode(const SrcPos& srcpos, int64_t value, bool isImaginary, const Type& type)
      : NumberNode<int64_t>(srcpos, value, isImaginary, type)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<IntNode>(fSrcPos, fValue, fIsImaginary, fType.clone()));
  }
};

inline std::shared_ptr<IntNode> makeIntNode(const SrcPos& srcpos, int64_t value,
                                            bool isImaginary, const Type& type)
{
  return std::make_shared<IntNode>(srcpos, value, isImaginary, type);
}


class RealNode : public NumberNode<double> {
public:
  RealNode(const SrcPos& srcpos, double value, bool isImaginary, const Type& type)
      : NumberNode<double>(srcpos, value, isImaginary, type)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<RealNode>(fSrcPos, fValue, fIsImaginary, fType.clone()));
  }
};

inline std::shared_ptr<RealNode> makeRealNode(const SrcPos& srcpos, double value,
                                              bool isImaginary, const Type& type)
{
  return std::make_shared<RealNode>(srcpos, value, isImaginary, type);
}


class RationalNode : public NumberNode<Rational> {
public:
  RationalNode(const SrcPos& srcpos, const Rational& value, bool isImaginary,
               const Type& type)
      : NumberNode<Rational>(srcpos, value, isImaginary, type)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<RationalNode>(
                                         fSrcPos, fValue, fIsImaginary, fType.clone()));
  }
};

inline std::shared_ptr<RationalNode> makeRationalNode(const SrcPos& srcpos,
                                                      const Rational& value,
                                                      bool isImaginary, const Type& type)
{
  return std::make_shared<RationalNode>(srcpos, value, isImaginary, type);
}


class CharNode : public AstNode {
public:
  CharNode(const SrcPos& srcpos, Char value)
      : AstNode(srcpos)
      , fValue(value)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<CharNode>(fSrcPos, fValue));
  }

  Char value() const { return fValue; }

private:
  Char fValue;
};

inline std::shared_ptr<CharNode> makeCharNode(const SrcPos& srcpos, Char value)
{
  return std::make_shared<CharNode>(srcpos, value);
}


class BoolNode : public AstNode {
public:
  BoolNode(const SrcPos& srcpos, bool value)
      : AstNode(srcpos)
      , fValue(value)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<BoolNode>(fSrcPos, fValue));
  }

  bool value() const { return fValue; }

private:
  bool fValue;
};

inline std::shared_ptr<BoolNode> makeBoolNode(const SrcPos& srcpos, bool value)
{
  return std::make_shared<BoolNode>(srcpos, value);
}


class CompileUnitNode : public ListNode {
public:
  CompileUnitNode(const SrcPos& srcpos)
      : ListNode(srcpos)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto node = std::make_shared<CompileUnitNode>(fSrcPos);
    details::copyNodes(&node->fChildren, &fChildren);
    return details::cloneScope(this, std::move(node));
  }
};

inline std::shared_ptr<CompileUnitNode> makeCompileUnitNode(const SrcPos& srcpos)
{
  return std::make_shared<CompileUnitNode>(srcpos);
}


class BaseDefNode : public AstNode {
public:
  BaseDefNode(const SrcPos& srcpos, std::shared_ptr<AstNode> defined)
      : AstNode(srcpos)
      , fDefined(std::move(defined))
  {
  }

  NodeList child_nodes() override { return { fDefined }; }

  std::shared_ptr<AstNode> defNode() const { return fDefined; }

  void setDefNode(std::shared_ptr<AstNode> val) { fDefined = std::move(val); }


protected:
  std::shared_ptr<AstNode> fDefined;
};


class LetNode : public BaseDefNode {
public:
  LetNode(std::shared_ptr<AstNode> node)
      : BaseDefNode(node->srcpos(), std::move(node))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this,
                               std::make_shared<LetNode>(details::nodeClone(fDefined)));
  }
};

inline std::shared_ptr<LetNode> makeLetNode(std::shared_ptr<AstNode> node)
{
  return std::make_shared<LetNode>(std::move(node));
}


class DefNode : public BaseDefNode {
public:
  DefNode(std::shared_ptr<AstNode> node)
      : BaseDefNode(node->srcpos(), std::move(node))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this,
                               std::make_shared<DefNode>(details::nodeClone(fDefined)));
  }
};

inline std::shared_ptr<DefNode> makeDefNode(std::shared_ptr<AstNode> node)
{
  return std::make_shared<DefNode>(std::move(node));
}


enum BindingAllocType {
  kAlloc_Local,
  kAlloc_Shared  // variable is taken by closure
};

class BindingNode : public AstNode, public NamedNode {
public:
  BindingNode(const SrcPos& srcpos, const String& symbolName, const Type& type,
              std::shared_ptr<AstNode> initExpr)
      : AstNode(srcpos, type)
      , fSymbolName(symbolName)
      , fInitExpr(std::move(initExpr))
      , fAllocType(kAlloc_Local)
  {
  }

  std::shared_ptr<AstNode> initExpr() const { return fInitExpr; }

  const String& name() const override { return fSymbolName; }

  void setAllocType(BindingAllocType allocType) { fAllocType = allocType; }

  BindingAllocType allocType() const { return fAllocType; }

  NodeList child_nodes() override
  {
    return fInitExpr ? NodeList{ fInitExpr } : NodeList{};
  }

protected:
  String fSymbolName;
  std::shared_ptr<AstNode> fInitExpr;
  BindingAllocType fAllocType;
};


enum VardefFlags { kNormalVar, kConstVar, kConfigVar, kEnumVar };

class VardefNode : public BindingNode,
                   public DelayTypeAnnotatable,
                   public LinkableSymbol {
public:
  VardefNode(const SrcPos& srcpos, const String& symbolName, VardefFlags flags,
             bool isLocal, const Type& type, std::shared_ptr<AstNode> init)
      : BindingNode(srcpos, symbolName, type, std::move(init))
      , fIsLocal(isLocal)
      , fFlags(flags)
  {
    if (initExpr())
      initExpr()->setIsSingleTypeRequired(true);
  }


  std::shared_ptr<AstNode> clone() const override
  {
    auto n = std::make_shared<VardefNode>(fSrcPos, fSymbolName, fFlags, fIsLocal,
                                          fType.clone(), details::nodeClone(fInitExpr));
    n->setLinkage(fLinkage);
    return details::cloneScope(this, std::move(n));
  }

  bool isEnum() const { return fFlags == kEnumVar; }

  bool isConst() const { return fFlags == kConstVar; }

  bool isConfig() const { return fFlags == kConfigVar; }

  bool isLocal() const { return fIsLocal; }

  VardefFlags flags() const { return fFlags; }

private:
  bool fIsLocal;
  VardefFlags fFlags;
};

inline std::shared_ptr<VardefNode>
makeVardefNode(const SrcPos& srcpos, const String& symbolName, VardefFlags flags,
               bool isLocal, const Type& type, std::shared_ptr<AstNode> initExpr)
{
  return std::make_shared<VardefNode>(srcpos, symbolName, flags, isLocal, type,
                                      std::move(initExpr));
}


enum ParamFlags {
  kPosArg = 1 << 0,
  kSpecArg = 1 << 1,
  kNamedArg = 1 << 2,
  kRestArg = 1 << 3
};


class ParamNode : public BindingNode {
public:
  ParamNode(const SrcPos& srcpos, const String& keyName, const String& symbolName,
            ParamFlags flags, const Type& type, std::shared_ptr<AstNode> init)
      : BindingNode(srcpos, symbolName, type, std::move(init))
      , fKey(keyName)
      , fFlags(flags)
  {
    hr_assert(implies(fFlags == kNamedArg, !fKey.isEmpty()));

    if (initExpr())
      initExpr()->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<ParamNode>(fSrcPos, fKey, fSymbolName, fFlags,
                                          fType.clone(), details::nodeClone(fInitExpr)));
  }

  ParamFlags flags() const { return fFlags; }

  const String& key() const { return fKey; }

  //! indicates whether this is a position parameter.
  bool isPositional() const { return (fFlags & kPosArg) != 0; }

  //! indicates whether this is a named parameter.
  bool isNamed() const { return (fFlags & kNamedArg) != 0; }

  //! indicates whether this is a rest parameter ("...").
  bool isRestArg() const { return (fFlags & kRestArg) != 0; }

  //! Indicates whether this is a specialized parameter ("@").
  bool isSpecArg() const { return (fFlags & kSpecArg) != 0; }

private:
  String fKey;
  ParamFlags fFlags;
};

inline std::shared_ptr<ParamNode>
makeParamNode(const SrcPos& srcpos, const String& keyName, const String& symbolName,
              ParamFlags flags, const Type& type, std::shared_ptr<AstNode> initExpr)
{
  return std::make_shared<ParamNode>(srcpos, keyName, symbolName, flags, type,
                                     std::move(initExpr));
}


class SlotdefNode : public BindingNode {
public:
  SlotdefNode(const SrcPos& srcpos, const String& symbolName, unsigned int flags,
              const Type& type, std::shared_ptr<AstNode> initExpr)
      : BindingNode(srcpos, symbolName, type, std::move(initExpr))
      , fFlags(flags)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<SlotdefNode>(fSrcPos, fSymbolName, fFlags, fType.clone(),
                                            details::nodeClone(fInitExpr)));
  }

  unsigned int flags() const { return fFlags; }

private:
  unsigned int fFlags;
};

inline std::shared_ptr<SlotdefNode> makeSlotdefNode(const SrcPos& srcpos,
                                                    const String& symbolName,
                                                    unsigned int flags, const Type& type,
                                                    std::shared_ptr<AstNode> initExpr)
{
  return std::make_shared<SlotdefNode>(srcpos, symbolName, flags, type,
                                       std::move(initExpr));
}


class ArrayNode : public ListNode {
public:
  ArrayNode(const SrcPos& srcpos)
      : ListNode(srcpos)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto an = std::make_shared<ArrayNode>(fSrcPos);
    details::copyNodes(&an->fChildren, &fChildren);
    return details::cloneScope(this, std::move(an));
  }
};

inline std::shared_ptr<ArrayNode> makeArrayNode(const SrcPos& srcpos)
{
  return std::make_shared<ArrayNode>(srcpos);
}


class VectorNode : public ListNode {
public:
  VectorNode(const SrcPos& srcpos)
      : ListNode(srcpos)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto vect = std::make_shared<VectorNode>(fSrcPos);
    details::copyNodes(&vect->fChildren, &fChildren);
    return details::cloneScope(this, std::move(vect));
  }
};

inline std::shared_ptr<VectorNode> makeVectorNode(const SrcPos& srcpos)
{
  return std::make_shared<VectorNode>(srcpos);
}


class BinaryNode : public AstNode {
public:
  BinaryNode(const SrcPos& srcpos, std::shared_ptr<AstNode> left, OperatorType op,
             std::shared_ptr<AstNode> right)
      : AstNode(srcpos)
      , fLeft(std::move(left))
      , fRight(std::move(right))
      , fOp(op)
  {
    hr_assert(fOp != kOpInvalid);
  }

  OperatorType op() const { return fOp; }

  std::shared_ptr<AstNode> left() const { return fLeft; }

  std::shared_ptr<AstNode> right() const { return fRight; }

  void setLeft(std::shared_ptr<AstNode> node) { fLeft = std::move(node); }

  void setRight(std::shared_ptr<AstNode> node) { fRight = std::move(node); }

  bool isMapTo() const { return fOp == kOpMapTo; }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<BinaryNode>(fSrcPos, details::nodeClone(fLeft), fOp,
                                           details::nodeClone(fRight)));
  }

  NodeList child_nodes() override { return { fLeft, fRight }; }

private:
  std::shared_ptr<AstNode> fLeft;
  std::shared_ptr<AstNode> fRight;
  OperatorType fOp;
};

inline std::shared_ptr<BinaryNode> makeBinaryNode(const SrcPos& srcpos,
                                                  std::shared_ptr<AstNode> left,
                                                  OperatorType op,
                                                  std::shared_ptr<AstNode> right)
{
  return std::make_shared<BinaryNode>(srcpos, std::move(left), op, std::move(right));
}


enum UnaryOperatorType {
  kUnaryOpInvalid,

  kUnaryOpNegate,
  kUnaryOpNot
};

class UnaryNode : public AstNode {
public:
  UnaryNode(const SrcPos& srcpos, UnaryOperatorType op, std::shared_ptr<AstNode> base)
      : AstNode(srcpos)
      , fBase(std::move(base))
      , fOp(op)
  {
  }

  std::shared_ptr<AstNode> base() const { return fBase; }

  void setBase(std::shared_ptr<AstNode> base) { fBase = std::move(base); }

  UnaryOperatorType op() const { return fOp; }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<UnaryNode>(fSrcPos, fOp, details::nodeClone(fBase)));
  }

  NodeList child_nodes() override { return { fBase }; }

private:
  std::shared_ptr<AstNode> fBase;
  UnaryOperatorType fOp;
};

inline std::shared_ptr<UnaryNode>
makeUnaryNode(const SrcPos& srcpos, UnaryOperatorType op, std::shared_ptr<AstNode> base)
{
  return std::make_shared<UnaryNode>(srcpos, op, std::move(base));
}


class RangeNode : public AstNode {
public:
  RangeNode(const SrcPos& srcpos, std::shared_ptr<AstNode> from,
            std::shared_ptr<AstNode> to, std::shared_ptr<AstNode> by)
      : AstNode(srcpos)
      , fFrom(std::move(from))
      , fTo(std::move(to))
      , fBy(std::move(by))
  {
  }

  std::shared_ptr<AstNode> from() const { return fFrom; }

  void setFrom(std::shared_ptr<AstNode> node) { fFrom = std::move(node); }

  std::shared_ptr<AstNode> to() const { return fTo; }

  void setTo(std::shared_ptr<AstNode> node) { fTo = std::move(node); }

  std::shared_ptr<AstNode> by() const { return fBy; }

  void setBy(std::shared_ptr<AstNode> node) { fBy = std::move(node); }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this,
        std::make_shared<RangeNode>(fSrcPos, details::nodeClone(fFrom),
                                    details::nodeClone(fTo), details::nodeClone(fBy)));
  }

  NodeList child_nodes() override
  {
    return fBy ? NodeList{ fFrom, fTo, fBy } : NodeList{ fFrom, fTo };
  }

private:
  std::shared_ptr<AstNode> fFrom;
  std::shared_ptr<AstNode> fTo;
  std::shared_ptr<AstNode> fBy;
};

inline std::shared_ptr<RangeNode> makeRangeNode(const SrcPos& srcpos,
                                                std::shared_ptr<AstNode> from,
                                                std::shared_ptr<AstNode> to,
                                                std::shared_ptr<AstNode> by)
{
  return std::make_shared<RangeNode>(srcpos, std::move(from), std::move(to),
                                     std::move(by));
}


class AssignNode : public AstNode, public DelayTypeAnnotatable {
public:
  AssignNode(const SrcPos& srcpos, std::shared_ptr<AstNode> lvalue,
             std::shared_ptr<AstNode> rvalue)
      : AstNode(srcpos)
      , fLValue(std::move(lvalue))
      , fRValue(std::move(rvalue))
  {
    if (fRValue)
      fRValue->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> lvalue() const { return fLValue; }

  std::shared_ptr<AstNode> rvalue() const { return fRValue; }

  void setLvalue(std::shared_ptr<AstNode> val) { fLValue = std::move(val); }

  void setRvalue(std::shared_ptr<AstNode> val) { fRValue = std::move(val); }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<AssignNode>(fSrcPos, details::nodeClone(fLValue),
                                           details::nodeClone(fRValue)));
  }

  NodeList child_nodes() override { return { fLValue, fRValue }; }

private:
  std::shared_ptr<AstNode> fLValue;
  std::shared_ptr<AstNode> fRValue;
};

inline std::shared_ptr<AssignNode> makeAssignNode(const SrcPos& srcpos,
                                                  std::shared_ptr<AstNode> lvalue,
                                                  std::shared_ptr<AstNode> rvalue)
{
  return std::make_shared<AssignNode>(srcpos, std::move(lvalue), std::move(rvalue));
}


class IfNode : public AstNode {
public:
  IfNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
         std::shared_ptr<AstNode> consequent, std::shared_ptr<AstNode> alternate)
      : AstNode(srcpos)
      , fTest(std::move(test))
      , fConsequent(std::move(consequent))
      , fAlternate(std::move(alternate))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<IfNode>(fSrcPos, details::nodeClone(fTest),
                                       details::nodeClone(fConsequent),
                                       details::nodeClone(fAlternate)));
  }

  std::shared_ptr<AstNode> test() const { return fTest; }

  void setTest(std::shared_ptr<AstNode> node) { fTest = std::move(node); }

  std::shared_ptr<AstNode> consequent() const { return fConsequent; }

  void setConsequent(std::shared_ptr<AstNode> node) { fConsequent = std::move(node); }

  std::shared_ptr<AstNode> alternate() const { return fAlternate; }

  void setAlternate(std::shared_ptr<AstNode> node) { fAlternate = std::move(node); }

  NodeList child_nodes() override
  {
    return fAlternate ? NodeList{ fTest, fConsequent, fAlternate }
                      : NodeList{ fTest, fConsequent };
  }

private:
  std::shared_ptr<AstNode> fTest;
  std::shared_ptr<AstNode> fConsequent;
  std::shared_ptr<AstNode> fAlternate;
};

inline std::shared_ptr<IfNode> makeIfNode(const SrcPos& srcpos,
                                          std::shared_ptr<AstNode> test,
                                          std::shared_ptr<AstNode> consequent,
                                          std::shared_ptr<AstNode> alternate)
{
  return std::make_shared<IfNode>(srcpos, std::move(test), std::move(consequent),
                                  std::move(alternate));
}


class SelectNode : public AstNode {
public:
  struct SelectMapping {
    SelectMapping(const NodeList& values, std::shared_ptr<AstNode> consequent)
        : fTestValues(values)
        , fConsequent(std::move(consequent))
    {
    }

    SelectMapping(const SelectMapping& other)
        : fTestValues(other.fTestValues)
        , fConsequent(other.fConsequent)
    {
    }

    // if fTestValues is empty this is an else branch
    NodeList fTestValues;
    std::shared_ptr<AstNode> fConsequent;
  };

  using SelectMappingVector = std::vector<SelectMapping>;


  SelectNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
             std::shared_ptr<AstNode> comparator)
      : AstNode(srcpos)
      , fTest(std::move(test))
      , fComparator(std::move(comparator))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto newNode = std::make_shared<SelectNode>(fSrcPos, details::nodeClone(fTest),
                                                details::nodeClone(fComparator));
    for (auto& mapping : fMappings) {
      newNode->fMappings.push_back(
          SelectMapping(details::copyNodes(mapping.fTestValues),
                        details::nodeClone(mapping.fConsequent)));
    }

    return details::cloneScope(this, std::move(newNode));
  }

  void addMapping(const NodeList& mappings, std::shared_ptr<AstNode> consequent)
  {
    fMappings.push_back(SelectMapping(mappings, std::move(consequent)));
  }

  void addMapping(std::shared_ptr<AstNode> mapping, std::shared_ptr<AstNode> consequent)
  {
    fMappings.push_back(SelectMapping(std::vector<NodeList::value_type>{ mapping },
                                      std::move(consequent)));
  }

  void addElseMapping(std::shared_ptr<AstNode> alternate)
  {
    fMappings.push_back(SelectMapping(NodeList(), std::move(alternate)));
  }

  std::shared_ptr<AstNode> test() const { return fTest; }

  void setTest(std::shared_ptr<AstNode> nd) { fTest = std::move(nd); }

  std::shared_ptr<AstNode> comparator() const { return fComparator; }

  void setComparator(std::shared_ptr<AstNode> nd) { fComparator = std::move(nd); }

  SelectMappingVector& mappings() { return fMappings; }

  size_t mappingCount() const { return fMappings.size(); }

  const SelectMapping& mappingAt(size_t i) const { return fMappings[i]; }

  void setConsequentAt(size_t i, std::shared_ptr<AstNode> consq)
  {
    fMappings[i].fConsequent = std::move(consq);
  }

  void setTestValueAt(size_t i, size_t j, std::shared_ptr<AstNode> value)
  {
    fMappings[i].fTestValues[j] = std::move(value);
  }

  NodeList child_nodes() override
  {
    auto result = NodeList{};

    result.emplace_back(fTest);
    if (fComparator)
      result.emplace_back(fComparator);

    for (size_t i = 0; i < fMappings.size(); i++) {
      if (fMappings[i].fTestValues.empty()) {
        result.emplace_back(fMappings[i].fConsequent);
      }
      else {
        for (auto& testValue : fMappings[i].fTestValues)
          result.emplace_back(testValue);
      }
      result.emplace_back(fMappings[i].fConsequent);
    }

    return result;
  }

private:
  std::shared_ptr<AstNode> fTest;
  std::shared_ptr<AstNode> fComparator;
  SelectMappingVector fMappings;
};

inline std::shared_ptr<SelectNode> makeSelectNode(const SrcPos& srcpos,
                                                  std::shared_ptr<AstNode> test,
                                                  std::shared_ptr<AstNode> comparator)
{
  return std::make_shared<SelectNode>(srcpos, std::move(test), std::move(comparator));
}


class MatchNode : public AstNode {
public:
  struct MatchMapping {
    MatchMapping(const SrcPos& srcpos, const String& varName, const Type& matchType,
                 std::shared_ptr<AstNode> consequent)
        : fSrcPos(srcpos)
        , fVarName(varName)
        , fMatchType(matchType)
        , fConsequent(std::move(consequent))
    {
    }

    MatchMapping(const MatchMapping& other)
        : fSrcPos(other.fSrcPos)
        , fVarName(other.fVarName)
        , fMatchType(other.fMatchType)
        , fConsequent(other.fConsequent)
    {
    }

    SrcPos fSrcPos;
    String fVarName;
    Type fMatchType;
    std::shared_ptr<AstNode> fConsequent;
  };

  using MatchMappingVector = std::vector<MatchMapping>;


  MatchNode(const SrcPos& srcpos, std::shared_ptr<AstNode> expr)
      : AstNode(srcpos)
      , fExpr(std::move(expr))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto newNode = std::make_shared<MatchNode>(fSrcPos, details::nodeClone(fExpr));
    for (auto& mapping : fMappings) {
      newNode->fMappings.push_back(MatchMapping(mapping.fSrcPos, mapping.fVarName,
                                                mapping.fMatchType.clone(),
                                                details::nodeClone(mapping.fConsequent)));
    }

    return details::cloneScope(this, std::move(newNode));
  }

  void addMapping(const SrcPos& srcpos, const String& varName, const Type& matchType,
                  std::shared_ptr<AstNode> consequent)
  {
    fMappings.push_back(MatchMapping(srcpos, varName, matchType, std::move(consequent)));
  }

  std::shared_ptr<AstNode> expr() const { return fExpr; }

  void setExpr(std::shared_ptr<AstNode> nd) { fExpr = std::move(nd); }

  MatchMappingVector& mappings() { return fMappings; }

  size_t mappingCount() const { return fMappings.size(); }

  const MatchMapping& mappingAt(size_t i) const { return fMappings[i]; }

  void setConsequentAt(size_t i, std::shared_ptr<AstNode> consq)
  {
    fMappings[i].fConsequent = std::move(consq);
  }

  NodeList child_nodes() override
  {
    auto result = NodeList{};

    result.emplace_back(fExpr);

    for (size_t i = 0; i < fMappings.size(); i++) {
      result.emplace_back(fMappings[i].fConsequent);
    }

    return result;
  }

private:
  std::shared_ptr<AstNode> fExpr;
  MatchMappingVector fMappings;
};

inline std::shared_ptr<MatchNode> makeMatchNode(const SrcPos& srcpos,
                                                std::shared_ptr<AstNode> expr)
{
  return std::make_shared<MatchNode>(srcpos, std::move(expr));
}


class BlockNode : public ListNode {
public:
  BlockNode(const SrcPos& srcpos)
      : ListNode(srcpos)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto block = std::make_shared<BlockNode>(fSrcPos);
    details::copyNodes(&block->fChildren, &fChildren);
    return details::cloneScope(this, std::move(block));
  }
};

inline std::shared_ptr<BlockNode> makeBlockNode(const SrcPos& srcpos)
{
  return std::make_shared<BlockNode>(srcpos);
}


class FunctionNode : public ListNode {
public:
  FunctionNode(const SrcPos& srcpos, const NodeList& params, const Type& retType,
               std::shared_ptr<AstNode> body)
      : ListNode(srcpos)
      , fRetType(retType)
      , fBody(std::move(body))
  {
    fChildren.assign(params.begin(), params.end());
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto nd = std::make_shared<FunctionNode>(fSrcPos, details::copyNodes(fChildren),
                                             fRetType.clone(), details::nodeClone(fBody));
    return details::cloneScope(this, nd);
  }

  const Type& retType() const { return fRetType; }

  void setRetType(const Type& type) { fRetType = std::move(type); }

  std::shared_ptr<AstNode> body() const { return fBody; }

  void setBody(std::shared_ptr<AstNode> node) { fBody = std::move(node); }

  const NodeList& params() const { return fChildren; }

  NodeList& params() { return fChildren; }

  size_t specializedParamsCount() const
  {
    return std::count_if(params().begin(), params().end(),
                         [](const NodeList::value_type& nd) {
                           auto prm = dynamic_cast<ParamNode*>(nd.get());
                           return prm && prm->isSpecArg();
                         });
  }

  bool hasSpecializedParams() const { return specializedParamsCount() > 0; }

  NodeList child_nodes() override
  {
    auto result = children();
    if (fBody)
      result.emplace_back(fBody);
    return result;
  }

protected:
  Type fRetType;
  std::shared_ptr<AstNode> fBody;
};

inline std::shared_ptr<FunctionNode> makeFunctionNode(const SrcPos& srcpos,
                                                      const NodeList& params,
                                                      const Type& retType,
                                                      std::shared_ptr<AstNode> body)
{
  return std::make_shared<FunctionNode>(srcpos, params, retType, std::move(body));
}


enum {
  kFuncIsGeneric = 1 << 0,   // generic function
  kFuncIsAbstract = 1 << 1,  // abstract function definition
  kFuncIsMethod = 1 << 2,    // generic function implementation
};


class FuncDefNode : public FunctionNode, public NamedNode, public LinkableSymbol {
public:
  FuncDefNode(const SrcPos& srcpos, const String& sym, unsigned int flags,
              const NodeList& params, const Type& retType, std::shared_ptr<AstNode> body)
      : FunctionNode(srcpos, params, retType, std::move(body))
      , fSym(sym)
      , fFlags(flags)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto n = std::make_shared<FuncDefNode>(fSrcPos, fSym, fFlags,
                                           details::copyNodes(fChildren),
                                           fRetType.clone(), details::nodeClone(fBody));
    n->setLinkage(fLinkage);
    return details::cloneScope(this, std::move(n));
  }

  const String& name() const override { return fSym; }

  bool isGeneric() const { return (fFlags & kFuncIsGeneric) != 0; }

  bool isMethod() const { return (fFlags & kFuncIsMethod) != 0; }

  bool isAbstract() const { return (fFlags & kFuncIsAbstract) != 0; }

  bool isAppMain() const { return name() == Names::kAppMain; }

private:
  String fSym;
  unsigned int fFlags;
};

inline std::shared_ptr<FuncDefNode> makeFuncDefNode(const SrcPos& srcpos,
                                                    const String& sym, unsigned int flags,
                                                    const NodeList& params,
                                                    const Type& retType,
                                                    std::shared_ptr<AstNode> body)
{
  return std::make_shared<FuncDefNode>(srcpos, sym, flags, params, retType,
                                       std::move(body));
}


class ApplyNode : public ListNode {
public:
  ApplyNode(const SrcPos& srcpos, std::shared_ptr<AstNode> baseNd)
      : ListNode(srcpos)
      , fBase(std::move(baseNd))
  {
    if (base())
      base()->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> base() const { return fBase; }

  void setBase(std::shared_ptr<AstNode> node) { fBase = std::move(node); }

  //! indicates whether base is a SymbolNode
  bool isSimpleCall() const
  {
    auto sym = dynamic_cast<SymbolNode*>(base().get());
    return sym && sym->generics().empty();
  }

  String simpleCallName() const
  {
    auto sym = dynamic_cast<SymbolNode*>(base().get());
    return (sym ? sym->name() : String());
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto apply = std::make_shared<ApplyNode>(fSrcPos, details::nodeClone(fBase));
    details::copyNodes(&apply->fChildren, &fChildren);
    apply->fRefFunction = fRefFunction;
    return details::cloneScope(this, std::move(apply));
  }

  NodeList child_nodes() override
  {
    auto result = NodeList{};
    result.emplace_back(fBase);
    result.insert(end(result), begin(children()), end(children()));
    return result;
  }

  std::weak_ptr<FunctionNode> refFunction() { return fRefFunction; }
  void setRefFunction(std::shared_ptr<FunctionNode> refFunction)
  {
    fRefFunction = refFunction;
  }

private:
  std::shared_ptr<AstNode> fBase;
  std::weak_ptr<FunctionNode> fRefFunction;
};

inline std::shared_ptr<ApplyNode> makeApplyNode(const SrcPos& srcpos,
                                                std::shared_ptr<AstNode> base)
{
  return std::make_shared<ApplyNode>(srcpos, std::move(base));
}


class WeakNode : public AstNode {
public:
  WeakNode(std::shared_ptr<AstNode> refNode)
      : AstNode(refNode ? refNode->srcpos() : SrcPos())
      , fRefNode(std::move(refNode))
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this,
                               std::make_shared<WeakNode>(details::nodeClone(fRefNode)));
  }

  std::shared_ptr<AstNode> refNode() const { return fRefNode; }
  void reset() { fRefNode = nullptr; }
  NodeList child_nodes() override { return fRefNode ? NodeList{ fRefNode } : NodeList{}; }

  bool isObsolete() const override { return fRefNode ? fRefNode->isObsolete() : false; }
  bool isRemoveable() const override { return true; }

private:
  std::shared_ptr<AstNode> fRefNode;
};

inline std::shared_ptr<WeakNode> makeWeakNode(std::shared_ptr<AstNode> refNode)
{
  return std::make_shared<WeakNode>(std::move(refNode));
}


class KeyargNode : public AstNode {
public:
  KeyargNode(const SrcPos& srcpos, const String& key, std::shared_ptr<AstNode> value)
      : AstNode(srcpos)
      , fKey(key)
      , fValue(std::move(value))
  {
    hr_assert(fValue);
    if (fValue)
      fValue->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<KeyargNode>(fSrcPos, fKey, details::nodeClone(fValue)));
  }

  const String& key() const { return fKey; }

  std::shared_ptr<AstNode> value() const { return fValue; }

  void setValue(std::shared_ptr<AstNode> node) { fValue = std::move(node); }

  NodeList child_nodes() override { return { fValue }; }

private:
  String fKey;
  std::shared_ptr<AstNode> fValue;
};

inline std::shared_ptr<KeyargNode> makeKeyargNode(const SrcPos& srcpos, const String& key,
                                                  std::shared_ptr<AstNode> value)
{
  return std::make_shared<KeyargNode>(srcpos, key, std::move(value));
}


class WhileNode : public AstNode {
public:
  WhileNode(const SrcPos& srcpos, std::shared_ptr<AstNode> test,
            std::shared_ptr<AstNode> body)
      : AstNode(srcpos)
      , fTest(std::move(test))
      , fBody(std::move(body))
  {
    if (fTest)
      fTest->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<WhileNode>(fSrcPos, details::nodeClone(fTest),
                                          details::nodeClone(fBody)));
  }

  std::shared_ptr<AstNode> body() const { return fBody; }

  void setBody(std::shared_ptr<AstNode> node) { fBody = std::move(node); }

  std::shared_ptr<AstNode> test() const { return fTest; }

  void setTest(std::shared_ptr<AstNode> node) { fTest = std::move(node); }

  NodeList child_nodes() override { return { fTest, fBody }; }

private:
  std::shared_ptr<AstNode> fTest;
  std::shared_ptr<AstNode> fBody;
};

inline std::shared_ptr<WhileNode> makeWhileNode(const SrcPos& srcpos,
                                                std::shared_ptr<AstNode> test,
                                                std::shared_ptr<AstNode> body)
{
  return std::make_shared<WhileNode>(srcpos, std::move(test), std::move(body));
}


class TypeDefNode : public AstNode {
public:
  TypeDefNode(const SrcPos& srcpos, const String& typeName, bool isRecord,
              const Type& isa, const NodeList& slots)
      : AstNode(srcpos)
      , fTypeName(typeName)
      , fIsRecord(isRecord)
      , fSlots(slots)
      , fIsa(isa)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this, std::make_shared<TypeDefNode>(fSrcPos, fTypeName, fIsRecord, fIsa.clone(),
                                            details::copyNodes(fSlots)));
  }

  const String& name() const { return fTypeName; }

  const Type& defType() const { return fIsa; }

  bool isRecord() const { return fIsRecord; }

  const NodeList& slots() const { return fSlots; }

  NodeList& slots() { return fSlots; }

  NodeList child_nodes() override { return fSlots; }

private:
  String fTypeName;
  bool fIsRecord;
  NodeList fSlots;
  Type fIsa;
};

inline std::shared_ptr<TypeDefNode> makeTypeDefNode(const SrcPos& srcpos,
                                                    const String& typeName, bool isRecord,
                                                    const Type& isa,
                                                    const NodeList& slots)
{
  return std::make_shared<TypeDefNode>(srcpos, typeName, isRecord, isa, slots);
}


class CastNode : public AstNode {
public:
  CastNode(const SrcPos& srcpos, std::shared_ptr<AstNode> base, const Type& type)
      : AstNode(srcpos, type)
      , fBase(std::move(base))
  {
    if (fBase)
      fBase->setIsSingleTypeRequired(true);
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(
        this,
        std::make_shared<CastNode>(fSrcPos, details::nodeClone(fBase), fType.clone()));
  }

  std::shared_ptr<AstNode> base() const { return fBase; }

  void setBase(std::shared_ptr<AstNode> node) { fBase = std::move(node); }

  NodeList child_nodes() override { return { fBase }; }

private:
  std::shared_ptr<AstNode> fBase;
};

inline std::shared_ptr<CastNode>
makeCastNode(const SrcPos& srcpos, std::shared_ptr<AstNode> base, const Type& type)
{
  return std::make_shared<CastNode>(srcpos, std::move(base), type);
}


class SlotRefNode : public AstNode {
public:
  SlotRefNode(const SrcPos& srcpos, std::shared_ptr<AstNode> base, const String& slotName)
      : AstNode(srcpos)
      , fBase(std::move(base))
      , fSlotName(slotName)
  {
  }

  std::shared_ptr<AstNode> clone() const override
  {
    return details::cloneScope(this, std::make_shared<SlotRefNode>(
                                         fSrcPos, details::nodeClone(fBase), fSlotName));
  }

  std::shared_ptr<AstNode> base() const { return fBase; }

  void setBase(std::shared_ptr<AstNode> base) { fBase = std::move(base); }

  String slotName() const { return fSlotName; }

  NodeList child_nodes() override { return { fBase }; }

private:
  std::shared_ptr<AstNode> fBase;
  String fSlotName;
};

inline std::shared_ptr<SlotRefNode> makeSlotRefNode(const SrcPos& srcpos,
                                                    std::shared_ptr<AstNode> base,
                                                    const String& slotName)
{
  return std::make_shared<SlotRefNode>(srcpos, std::move(base), slotName);
}


class DictNode : public ListNode {
public:
  DictNode(const SrcPos& srcpos)
      : ListNode(srcpos)
  {
  }

  void addPair(std::shared_ptr<AstNode> key, std::shared_ptr<AstNode> value)
  {
    hr_assert(key);
    hr_assert(value);

    appendNode(makeBinaryNode(key->srcpos(), key, kOpMapTo, std::move(value)));
  }

  std::shared_ptr<AstNode> clone() const override
  {
    auto dict = std::make_shared<DictNode>(fSrcPos);
    details::copyNodes(&dict->fChildren, &fChildren);
    return details::cloneScope(this, std::move(dict));
  }
};

inline std::shared_ptr<DictNode> makeDictNode(const SrcPos& srcpos)
{
  return std::make_shared<DictNode>(srcpos);
}


//--------------------------------------------------------------------------------

inline NodeList makeNodeList()
{
  return NodeList();
}


inline NodeList makeNodeList(std::shared_ptr<AstNode> n1)
{
  NodeList nl;
  if (n1)
    nl.push_back(std::move(n1));
  return nl;
}


inline NodeList makeNodeList(std::initializer_list<std::shared_ptr<AstNode>> l)
{
  NodeList nl;
  nl.insert(nl.end(), l.begin(), l.end());
  return nl;
}


inline NodeList& appendNodes(NodeList& dst, const NodeList& nl)
{
  if (!nl.empty())
    dst.insert(dst.end(), nl.begin(), nl.end());
  return dst;
}


inline std::shared_ptr<AstNode> singletonNodeListOrNull(const NodeList& nl)
{
  hr_assert(nl.size() < 2);
  if (nl.size() == 1)
    return nl[0];
  return nullptr;
}

}  // namespace herschel
