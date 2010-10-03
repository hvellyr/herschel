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

namespace llvm
{
  class Value;
}

namespace heather
{
  class AptNode;
  class CodeGenerator;
  class XmlRenderer;


  //--------------------------------------------------------------------------

  typedef std::vector<Ptr<AptNode> > NodeList;


  //--------------------------------------------------------------------------

  class AptNode : public RefCountable
  {
  public:
    AptNode(const SrcPos& srcpos)
      : fSrcPos(srcpos)
    { }

    const SrcPos& srcpos() const
    {
      return fSrcPos;
    }

    virtual AptNode* clone() const = 0;

    virtual void render(XmlRenderer* renderer) const = 0;

    virtual void appendNode(AptNode* node);
    virtual void appendNodes(const NodeList& nodes);

    virtual llvm::Value* codegen(CodeGenerator* generator);

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    SrcPos   fSrcPos;
    NodeList fChildren;
  };


  //--------------------------------------------------------------------------

  class StringNode : public AptNode
  {
  public:
    StringNode(const SrcPos& srcpos, const String& value);

    virtual StringNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

    String fValue;
  };


  //--------------------------------------------------------------------------

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);
    SymbolNode(const SrcPos& srcpos, const String& value,
               const TypeVector& generics);

    virtual SymbolNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    std::string string() const;

  protected:
    friend class XmlRenderer;
    friend class CodeGenerator;

    String     fValue;
    TypeVector fGenerics;
  };


  class ArraySymbolNode : public SymbolNode
  {
  public:
    ArraySymbolNode(const SrcPos& srcpos, const String& value);

    virtual ArraySymbolNode* clone() const;
    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  template<typename T>
  class NumberNode : public AptNode
  {
  public:
  protected:
    NumberNode(const SrcPos& srcpos, T value, bool isImaginary,
               const Type& type)
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
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, double value, bool isImaginary,
             const Type& type);
    virtual RealNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos,
                 const Rational& value, bool isImaginary,
                 const Type& type);
    virtual RationalNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Char value);
    virtual CharNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

    bool fValue;
  };


  //--------------------------------------------------------------------------

  class UnitConstant : public AptNode
  {
  public:
    UnitConstant(const SrcPos& srcpos, AptNode* value, const TypeUnit& unit);
    virtual UnitConstant* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, AptNode* defined);

    AptNode* defNode() const;

  protected:
    friend class XmlRenderer;

    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode
  {
  public:
    LetNode(AptNode* node);
    virtual LetNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(AptNode* node);
    virtual DefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class BindingNode : public AptNode
  {
  public:
    BindingNode(const SrcPos& srcpos,
                const String& symbolName, const Type& type,
                AptNode* initExpr);

    const String& symbolName() const;
    const Type& type() const;
    AptNode* initExpr() const;

  protected:
    friend class XmlRenderer;

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
    VardefNode(const SrcPos& srcpos,
               const String& symbolName, VardefFlags flags,
               const Type& type, AptNode* initExpr);

    virtual VardefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    bool isEnum() const;
    bool isConst() const;
    bool isConfig() const;

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
    ParamNode(const SrcPos& srcpos,
              const String& keyName,
              const String& symbolName, ParamFlags flags,
              const Type& type, AptNode* initExpr);

    virtual ParamNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    ParamFlags flags() const;
    const String& key() const;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ArrayNode : public AptNode
  {
  public:
    ArrayNode(const SrcPos& srcpos)
      : AptNode(srcpos)
    { }

    virtual ArrayNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class VectorNode : public AptNode
  {
  public:
    VectorNode(const SrcPos& srcpos)
      : AptNode(srcpos)
    { }

    virtual VectorNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
  };


  //--------------------------------------------------------------------------

  class DictNode : public AptNode
  {
  public:
    DictNode(const SrcPos& srcpos)
      : AptNode(srcpos)
    { }

    virtual DictNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    void addPair(AptNode* key, AptNode* value);
  };


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos,
               AptNode* left, OperatorType op, AptNode* right);

    virtual BinaryNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    OperatorType op() const;
    AptNode* left() const;
    AptNode* right() const;

    void setLeft(AptNode* node);
    void setRight(AptNode* node);


    bool isMapTo() const;

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fLeft;
    Ptr<AptNode> fRight;
    OperatorType fOp;
  };


  //--------------------------------------------------------------------------

  class NegateNode : public AptNode
  {
  public:
    NegateNode(const SrcPos& srcpos, AptNode* base);

    virtual NegateNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              AptNode* from, AptNode* to, AptNode* by);
    virtual RangeNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    AptNode* from() const;
    AptNode* to() const;
    AptNode* by() const;

  private:
    friend class XmlRenderer;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
    AssignNode(const SrcPos& srcpos, AptNode* lvalue, AptNode* rvalue);
    virtual AssignNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    AptNode* lvalue() const;
    AptNode* rvalue() const;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
    SelectNode(const SrcPos& srcpos, AptNode* test, AptNode* comparator);

    virtual SelectNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    void addMapping(const NodeList& mappings, AptNode* consequent);
    void addMapping(AptNode* mapping, AptNode* consequent);
    void addElseMapping(AptNode* alternate);

  private:
    friend class XmlRenderer;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

    void addMapping(const SrcPos& srcpos, const String& varName,
                    const Type& matchType,
                    AptNode* consequent);

  private:
    friend class XmlRenderer;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
    BlockNode(const SrcPos& srcpos);
    virtual BlockNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);
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
    virtual llvm::Value* codegen(CodeGenerator* generator);

    const NodeList& params() const;
    const Type& retType() const;

  protected:
    friend class XmlRenderer;

    NodeList     fParams;
    Type         fRetType;
    Ptr<AptNode> fBody;
  };


  enum {
    kFuncIsGeneric = 1 << 0,
    kFuncIsAbstract = 1 << 1,
  };


  class FuncDefNode : public FunctionNode
  {
  public:
    FuncDefNode(const SrcPos&   srcpos,
                const String&   sym,
                unsigned int    flags,
                const NodeList& params,
                const Type&     retType,
                AptNode*        body);

    virtual FuncDefNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

    const String& funcName() const;
    bool isGeneric() const;
    bool isAbstract() const;

  private:
    friend class XmlRenderer;

    String       fSym;
    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ApplyNode : public AptNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, AptNode* base);

    virtual ApplyNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;
    friend class CodeGenerator;

    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class KeyargNode : public AptNode
  {
  public:
    KeyargNode(const SrcPos& srcpos, const String& key, AptNode* value);

    virtual KeyargNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

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
    virtual llvm::Value* codegen(CodeGenerator* generator);

  private:
    friend class XmlRenderer;

    Ptr<AptNode> fTest;
    Ptr<AptNode> fBody;
  };


  //--------------------------------------------------------------------------

  class TypeNode : public AptNode
  {
  public:
    TypeNode(const SrcPos& srcpos, const String& typeName,
             bool isClass,
             const Type& isa,
             const NodeList& params,
             const NodeList& slots,
             const NodeList& reqProtocol,
             const NodeList& onExprs);

    virtual TypeNode* clone() const;

    virtual void render(XmlRenderer* renderer) const;
    virtual llvm::Value* codegen(CodeGenerator* generator);

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
};


#endif  // bootstrap_apt_h
