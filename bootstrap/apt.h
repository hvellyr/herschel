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


  //--------------------------------------------------------------------------

  typedef std::vector<Ptr<AptNode> > NodeList;


  //--------------------------------------------------------------------------

  class AptNode : public RefCountable
  {
  public:
    AptNode(const SrcPos& srcpos, Scope* scope);

    const SrcPos& srcpos() const;
    Scope* scope();
    NodeList& children();
    const NodeList& children() const;

    virtual AptNode* clone() const = 0;

    virtual void appendNode(AptNode* node);
    virtual void appendNodes(const NodeList& nodes);

    virtual void render(XmlRenderer* renderer) const = 0;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator) = 0;

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
    StringNode(const SrcPos& srcpos, Scope* scope, const String& value);

    virtual StringNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    String fValue;
  };


  //--------------------------------------------------------------------------

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const SrcPos& srcpos, Scope* scope, const String& value);

    virtual KeywordNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    String fValue;
  };


  //--------------------------------------------------------------------------

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const SrcPos& srcpos, Scope* scope, const String& value);
    SymbolNode(const SrcPos& srcpos, Scope* scope, const String& value,
               const TypeVector& generics);

    virtual SymbolNode* clone() const;

    void setName(const String& nm);
    const String& name() const;
    std::string string() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* an);

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String     fValue;
    TypeVector fGenerics;
  };


  class ArraySymbolNode : public SymbolNode
  {
  public:
    ArraySymbolNode(const SrcPos& srcpos, Scope* scope, const String& value);

    virtual ArraySymbolNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  template<typename T>
  class NumberNode : public AptNode
  {
  public:
  protected:
    NumberNode(const SrcPos& srcpos, Scope* scope, T value,
               bool isImaginary, const Type& type)
      : AptNode(srcpos, scope),
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
    IntNode(const SrcPos& srcpos, Scope* scope, int value, bool isImaginary,
            const Type& type);
    virtual IntNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, Scope* scope, double value,
             bool isImaginary, const Type& type);
    virtual RealNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos, Scope* scope,
                 const Rational& value, bool isImaginary,
                 const Type& type);
    virtual RationalNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Scope* scope, Char value);
    virtual CharNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    Char fValue;
  };


  //--------------------------------------------------------------------------

  class BoolNode : public AptNode
  {
  public:
    BoolNode(const SrcPos& srcpos, Scope* scope, bool value);
    virtual BoolNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    bool fValue;
  };


  //--------------------------------------------------------------------------

  class UnitConstant : public AptNode
  {
  public:
    UnitConstant(const SrcPos& srcpos, Scope* scope, AptNode* value,
                 const TypeUnit& unit);
    virtual UnitConstant* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    AptNode* value() const { return fValue; }

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fValue;
    TypeUnit     fUnit;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public AptNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos, Scope* scope);
    virtual CompileUnitNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, Scope* scope, AptNode* defined);

    AptNode* defNode() const;

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode
  {
  public:
    LetNode(Scope* scope, AptNode* node);
    virtual LetNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(Scope* scope, AptNode* node);
    virtual DefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class BindingNode : public AptNode, public NamedNode
  {
  public:
    BindingNode(const SrcPos& srcpos, Scope* scope,
                const String& symbolName, const Type& type,
                AptNode* initExpr);

    const String& symbolName() const;
    const Type& type() const;
    AptNode* initExpr() const;

    virtual const String& name() const { return symbolName(); }

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String       fSymbolName;
    Type         fType;
    Ptr<AptNode> fInitExpr;
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
    VardefNode(const SrcPos& srcpos, Scope* scope,
               const String& symbolName, VardefFlags flags,
               const Type& type, AptNode* initExpr);

    virtual VardefNode* clone() const;

    bool isEnum() const;
    bool isConst() const;
    bool isConfig() const;

    VardefFlags flags() const
    {
      return fFlags;
    }

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    VardefFlags fFlags;
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
    ParamNode(const SrcPos& srcpos, Scope* scope,
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
    SlotdefNode(const SrcPos& srcpos, Scope* scope,
                const String& symbolName,
                unsigned int flags,
                const Type& type, AptNode* initExpr);

    virtual SlotdefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ArrayNode : public AptNode
  {
  public:
    ArrayNode(const SrcPos& srcpos, Scope* scope)
      : AptNode(srcpos, scope)
    { }

    virtual ArrayNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class VectorNode : public AptNode
  {
  public:
    VectorNode(const SrcPos& srcpos, Scope* scope)
      : AptNode(srcpos, scope)
    { }

    virtual VectorNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class DictNode : public AptNode
  {
  public:
    DictNode(const SrcPos& srcpos, Scope* scope)
      : AptNode(srcpos, scope)
    { }

    void addPair(AptNode* key, AptNode* value);

    virtual DictNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos, Scope* scope,
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
    NegateNode(const SrcPos& srcpos, Scope* scope, AptNode* base);

    const AptNode* base() const;
    AptNode* base();

    virtual NegateNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos, Scope* scope,
              AptNode* from, AptNode* to, AptNode* by);

    AptNode* from() const;
    AptNode* to() const;
    AptNode* by() const;

    virtual RangeNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    Ptr<AptNode> fFrom;
    Ptr<AptNode> fTo;
    Ptr<AptNode> fBy;
  };


  //--------------------------------------------------------------------------

  class ThenWhileNode : public AptNode
  {
  public:
    ThenWhileNode(const SrcPos& srcpos, Scope* scope,
                  AptNode* first, AptNode* step, AptNode* test);
    virtual ThenWhileNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    AptNode* first() const { return fFirst; }
    AptNode* step() const { return fStep; }
    AptNode* test() const { return fTest; }

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fFirst;
    Ptr<AptNode> fStep;
    Ptr<AptNode> fTest;
  };


  //--------------------------------------------------------------------------

  class AssignNode : public AptNode
  {
  public:
    AssignNode(const SrcPos& srcpos, Scope* scope,
               AptNode* lvalue, AptNode* rvalue);

    AptNode* lvalue() const;
    AptNode* rvalue() const;

    virtual AssignNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fLValue;
    Ptr<AptNode> fRValue;
  };


  //--------------------------------------------------------------------------

  class IfNode : public AptNode
  {
  public:
    IfNode(const SrcPos& srcpos, Scope* scope,
           AptNode* test, AptNode* consequent, AptNode* alternate);

    virtual IfNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    AptNode* test() const;
    AptNode* consequent() const;
    AptNode* alternate() const;

    void setAlternate(AptNode* node);

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fTest;
    Ptr<AptNode> fConsequent;
    Ptr<AptNode> fAlternate;
  };


  //--------------------------------------------------------------------------

  class SelectNode : public AptNode
  {
  public:
    SelectNode(const SrcPos& srcpos, Scope* scope,
               AptNode* test, AptNode* comparator);

    virtual SelectNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    void addMapping(const NodeList& mappings, AptNode* consequent);
    void addMapping(AptNode* mapping, AptNode* consequent);
    void addElseMapping(AptNode* alternate);

  private:
    friend class XmlRenderer;
    friend class Annotator;

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
    MatchNode(const SrcPos& srcpos, Scope* scope, AptNode* expr);

    virtual MatchNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    void addMapping(const SrcPos& srcpos, const String& varName,
                    const Type& matchType,
                    AptNode* consequent);

  private:
    friend class XmlRenderer;
    friend class Annotator;

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
    OnNode(const SrcPos& srcpos, Scope* scope,
           const String& key, const NodeList& params, AptNode* body);

    virtual OnNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    const String& key() { return fKey; }
    AptNode* body() { return fBody; }
    NodeList& params() { return fParams; }

  private:
    friend class XmlRenderer;

    String       fKey;
    NodeList     fParams;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class BlockNode : public AptNode
  {
  public:
    BlockNode(const SrcPos& srcpos, Scope* scope);
    virtual BlockNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);
  };


  //--------------------------------------------------------------------------

  class FunctionNode : public AptNode
  {
  public:
    FunctionNode(const SrcPos&   srcpos, Scope* scope,
                 const NodeList& params,
                 const Type&     retType,
                 AptNode*        body);

    virtual FunctionNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

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
                Scope*          scope,
                const String&   sym,
                unsigned int    flags,
                const NodeList& params,
                const Type&     retType,
                AptNode*        body);

    virtual FuncDefNode* clone() const;

    const String& funcName() const;
    bool isGeneric() const;
    bool isAbstract() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    virtual const String& name() const { return funcName(); }

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String       fSym;
    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ApplyNode : public AptNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, Scope* scope, AptNode* base);

    AptNode* base() const;

    virtual ApplyNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class KeyargNode : public AptNode
  {
  public:
    KeyargNode(const SrcPos& srcpos, Scope* scope,
               const String& key, AptNode* value);

    virtual KeyargNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    const String& key() const { return fKey; }
    AptNode* value() const { return fValue; }

  private:
    friend class XmlRenderer;

    String       fKey;
    Ptr<AptNode> fValue;
  };


  //--------------------------------------------------------------------------

  class WhileNode : public AptNode
  {
  public:
    WhileNode(const SrcPos& srcpos, Scope* scope,
              AptNode* test, AptNode* body);

    virtual WhileNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

    AptNode* body() const;
    AptNode* test() const;

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fTest;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class TypeDefNode : public AptNode
  {
  public:
    TypeDefNode(const SrcPos&   srcpos,
                Scope*          scope,
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
             Scope* scope,
             AptNode* base,
             const Type& type);

    virtual CastNode* clone() const;

    AptNode* base() const;
    const Type& type() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator) const;
    virtual void annotate(Annotator* annotator);

  private:
    Ptr<AptNode> fBase;
    Type         fType;
  };
};


#endif  // bootstrap_apt_h
