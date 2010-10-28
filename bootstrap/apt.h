/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
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

namespace heather
{
  class AptNode;
  class CodeGenerator;
  class XmlRenderer;
  class Annotator;
  class Transformator;


  //--------------------------------------------------------------------------

  typedef std::vector<Ptr<AptNode> > NodeList;


  //--------------------------------------------------------------------------

  class AptNode : public RefCountable
  {
  public:
    AptNode(const SrcPos& srcpos);

    const SrcPos& srcpos() const;
    Scope* scope() const;
    AptNode* setScope(Scope* scope);
    NodeList& children();
    const NodeList& children() const;

    virtual AptNode* clone() const = 0;

    virtual void appendNode(AptNode* node);
    virtual void appendNodes(const NodeList& nodes);

    virtual void render(XmlRenderer* renderer) const = 0;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator) = 0;
    virtual AptNode* transform(Transformator* annotator) = 0;

  protected:
    SrcPos     fSrcPos;
    NodeList   fChildren;
    Ptr<Scope> fScope;
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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

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
    kType,
  };

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);
    SymbolNode(const SrcPos& srcpos, const String& value,
               const TypeVector& generics);

    virtual SymbolNode* clone() const;

    void setName(const String& nm);
    const String& name() const;
    std::string string() const;

    SymReferType refersTo() const;
    void setRefersTo(SymReferType type, bool isShared);
    bool isShared() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* an);
    virtual AptNode* transform(Transformator* annotator);

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String       fValue;
    TypeVector   fGenerics;
    SymReferType fRefersTo;
    bool         fIsShared;     // refers to a variable outside of owning
                                // frame (= closed variable)
  };


  class ArraySymbolNode : public SymbolNode
  {
  public:
    ArraySymbolNode(const SrcPos& srcpos, const String& value);

    virtual ArraySymbolNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  //--------------------------------------------------------------------------

  template<typename T>
  class NumberNode : public AptNode
  {
  public:
  protected:
    NumberNode(const SrcPos& srcpos, T value,
               bool isImaginary, const Type& type)
      : AptNode(srcpos),
        fValue(value),
        fIsImaginary(isImaginary),
        fType(type)
    { }

  public:
    friend class XmlRenderer;

    T fValue;
    bool fIsImaginary;
    Type fType;
  };


  //--------------------------------------------------------------------------

  class IntNode : public NumberNode<int>
  {
  public:
    IntNode(const SrcPos& srcpos, int value, bool isImaginary,
            const Type& type);
    virtual IntNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
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
    virtual AptNode* transform(Transformator* annotator);
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
    virtual AptNode* transform(Transformator* annotator);
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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

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
    virtual AptNode* transform(Transformator* annotator);

    AptNode* value() const { return fValue; }

  private:
    friend class XmlRenderer;
    friend class Transformator;

    Ptr<AptNode> fValue;
    TypeUnit     fUnit;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public AptNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos);
    virtual CompileUnitNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, AptNode* defined);

    AptNode* defNode() const;
    void setDefNode(AptNode* val) { fDefined = val; }

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode
  {
  public:
    LetNode(AptNode* node);
    virtual LetNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(AptNode* node);
    virtual DefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
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

    const String& symbolName() const;
    const Type& type() const;
    AptNode* initExpr() const;
    void setInitExpr(AptNode* val) { fInitExpr = val; }
    virtual const String& name() const { return symbolName(); }

    void setAllocType(BindingAllocType type);
    BindingAllocType allocType() const;

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;
    friend class Transformator;

    String       fSymbolName;
    Type         fType;
    Ptr<AptNode> fInitExpr;
    BindingAllocType fAllocType;
  };


  enum VardefFlags {
    kNormalVar,
    kFluidVar,
    kConstVar,
    kConfigVar,
    kEnumVar
  };

  class VardefNode : public BindingNode
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

    const String& linkage() const;
    void setLinkage(const String& linkage);

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

    bool fIsLocal;
    VardefFlags fFlags;
    String fLinkage;
  };


  enum ParamFlags {
    kPosArg,
    kSpecArg,
    kNamedArg,
    kRestArg
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

    bool isRestArg() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

    String fKey;
    ParamFlags fFlags;
  };


  enum SlotFlags {
    kSimpleSlot     = 0,
    kTransientSlot  = 1 << 0,
    kReadonlySlot   = 1 << 1,
    kObservableSlot = 1 << 2,
    kPublicSlot     = 1 << 3,
    kOuterSlot      = 1 << 4,
    kInnerSlot      = 1 << 5,
    kAutoSlot       = 1 << 6,
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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ArrayNode : public AptNode
  {
  public:
    ArrayNode(const SrcPos& srcpos);

    virtual ArrayNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  //--------------------------------------------------------------------------

  class VectorNode : public AptNode
  {
  public:
    VectorNode(const SrcPos& srcpos);

    virtual VectorNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  //--------------------------------------------------------------------------

  class DictNode : public AptNode
  {
  public:
    DictNode(const SrcPos& srcpos);

    void addPair(AptNode* key, AptNode* value);

    virtual DictNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fLeft;
    Ptr<AptNode> fRight;
    OperatorType fOp;
  };


  //--------------------------------------------------------------------------

  class NegateNode : public AptNode
  {
  public:
    NegateNode(const SrcPos& srcpos, AptNode* base);

    const AptNode* base() const;
    AptNode* base();

    virtual NegateNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;
    friend class Transformator;

    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              AptNode* from, AptNode* to, AptNode* by);

    AptNode* from() const;
    AptNode* to() const;
    AptNode* by() const;

    virtual RangeNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class Transformator;

    Ptr<AptNode> fFrom;
    Ptr<AptNode> fTo;
    Ptr<AptNode> fBy;
  };


  //--------------------------------------------------------------------------

  class ThenWhileNode : public AptNode
  {
  public:
    ThenWhileNode(const SrcPos& srcpos,
                  AptNode* first, AptNode* step, AptNode* test);
    virtual ThenWhileNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    AptNode* first() const { return fFirst; }
    AptNode* step() const { return fStep; }
    AptNode* test() const { return fTest; }

  private:
    friend class XmlRenderer;
    friend class Transformator;

    Ptr<AptNode> fFirst;
    Ptr<AptNode> fStep;
    Ptr<AptNode> fTest;
  };


  //--------------------------------------------------------------------------

  class AssignNode : public AptNode
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
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;

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
    virtual AptNode* transform(Transformator* annotator);

    AptNode* test() const;
    AptNode* consequent() const;
    AptNode* alternate() const;

    void setAlternate(AptNode* node);

  private:
    friend class XmlRenderer;
    friend class Transformator;

    Ptr<AptNode> fTest;
    Ptr<AptNode> fConsequent;
    Ptr<AptNode> fAlternate;
  };


  //--------------------------------------------------------------------------

  class SelectNode : public AptNode
  {
  public:
    SelectNode(const SrcPos& srcpos,
               AptNode* test, AptNode* comparator);

    virtual SelectNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    void addMapping(const NodeList& mappings, AptNode* consequent);
    void addMapping(AptNode* mapping, AptNode* consequent);
    void addElseMapping(AptNode* alternate);

  private:
    friend class XmlRenderer;
    friend class Annotator;
    friend class Transformator;

    struct SelectMapping
    {
      SelectMapping(const NodeList& values, AptNode* consequent);
      SelectMapping(const SelectMapping& other);

      // if fTestValues is empty this is an else branch
      NodeList     fTestValues;
      Ptr<AptNode> fConsequent;
    };

    typedef std::vector<SelectMapping> SelectMappingVector;

    Ptr<AptNode>        fTest;
    Ptr<AptNode>        fComparator;
    SelectMappingVector fMappings;
  };


  //--------------------------------------------------------------------------

  class MatchNode : public AptNode
  {
  public:
    MatchNode(const SrcPos& srcpos, AptNode* expr);

    virtual MatchNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    void addMapping(const SrcPos& srcpos, const String& varName,
                    const Type& matchType,
                    AptNode* consequent);

  private:
    friend class XmlRenderer;
    friend class Annotator;
    friend class Transformator;

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

    Ptr<AptNode>       fExpr;
    MatchMappingVector fMappings;
  };


  //--------------------------------------------------------------------------

  class OnNode : public AptNode
  {
  public:
    OnNode(const SrcPos& srcpos,
           const String& key, const NodeList& params, AptNode* body);

    virtual OnNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    const String& key() const;
    const AptNode* body() const;
    AptNode* body();
    const NodeList& params() const;
    NodeList& params();

  private:
    friend class XmlRenderer;
    friend class Transformator;

    String       fKey;
    NodeList     fParams;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class BlockNode : public AptNode
  {
  public:
    BlockNode(const SrcPos& srcpos);
    virtual BlockNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);
  };


  //--------------------------------------------------------------------------

  class FunctionNode : public AptNode
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
    virtual AptNode* transform(Transformator* annotator);

    const NodeList& params() const;
    const Type& retType() const;

    AptNode* body()
    {
      return fBody;
    }

    NodeList& params()
    {
      return fParams;
    }

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;
    friend class Transformator;

    NodeList     fParams;
    Type         fRetType;
    Ptr<AptNode> fBody;
  };


  enum {
    kFuncIsGeneric = 1 << 0,
    kFuncIsAbstract = 1 << 1,
  };


  class FuncDefNode : public FunctionNode, public NamedNode
  {
  public:
    FuncDefNode(const SrcPos&   srcpos,
                const String&   sym,
                unsigned int    flags,
                const NodeList& params,
                const Type&     retType,
                AptNode*        body);

    virtual FuncDefNode* clone() const;

    const String& funcName() const;
    bool isGeneric() const;
    bool isAbstract() const;

    const String& linkage() const;
    void setLinkage(const String& linkage);

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    virtual const String& name() const { return funcName(); }

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String       fSym;
    unsigned int fFlags;
    String       fLinkage;
  };


  //--------------------------------------------------------------------------

  class ApplyNode : public AptNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, AptNode* base);

    AptNode* base() const;

    virtual ApplyNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;
    friend class Transformator;

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
    virtual AptNode* transform(Transformator* annotator);

    const String& key() const { return fKey; }
    AptNode* value() const { return fValue; }

  private:
    friend class XmlRenderer;
    friend class Transformator;

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
    virtual AptNode* transform(Transformator* annotator);

    AptNode* body() const;
    AptNode* test() const;

  private:
    friend class XmlRenderer;
    friend class Transformator;

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
                const NodeList& reqProtocol,
                const NodeList& onExprs);

    virtual TypeDefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

    const String& name() { return fTypeName; }
    const Type& defType() { return fIsa; }

  private:
    friend class XmlRenderer;

    String fTypeName;
    bool   fIsClass;
    NodeList fParams;
    NodeList fSlots;
    NodeList fReqProtocol;
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
    const Type& type() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
    virtual AptNode* transform(Transformator* annotator);

  private:
    friend class Transformator;

    Ptr<AptNode> fBase;
    Type         fType;
  };
};


#endif  // bootstrap_apt_h
