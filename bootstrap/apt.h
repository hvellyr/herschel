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

namespace heather
{
  class AptNode;

  //--------------------------------------------------------------------------

  typedef std::vector<Ptr<AptNode> > NodeList;
  typedef std::list<String> StringList;
  typedef std::map<String, String> StringStringMap;



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

    virtual void display(Port<Octet>* port) const = 0;

    virtual void appendNode(AptNode* node);
    virtual void appendNodes(const NodeList& nodes);

  protected:
    SrcPos   fSrcPos;
    NodeList fChildren;
  };


  //--------------------------------------------------------------------------

  class StringNode : public AptNode
  {
  public:
    StringNode(const SrcPos& srcpos, const String& value);

    virtual StringNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const SrcPos& srcpos, const String& value);

    virtual KeywordNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);

    virtual SymbolNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  template<typename T>
  class NumberNode : public AptNode
  {
  public:
  protected:
    NumberNode(const SrcPos& srcpos, T value, bool isImaginary)
      : AptNode(srcpos),
        fValue(value),
        fIsImaginary(isImaginary)
    { }

    T fValue;
    bool fIsImaginary;
  };


  //--------------------------------------------------------------------------

  class IntNode : public NumberNode<int>
  {
  public:
    IntNode(const SrcPos& srcpos, int value, bool isImaginary);
    virtual IntNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, double value, bool isImaginary);
    virtual RealNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos,
                 const Rational& value, bool isImaginary);
    virtual RationalNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Char value);
    virtual CharNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  private:
    Char fValue;
  };


  //--------------------------------------------------------------------------

  class BoolNode : public AptNode
  {
  public:
    BoolNode(const SrcPos& srcpos, bool value);
    virtual BoolNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  private:
    bool fValue;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public AptNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos);
    virtual CompileUnitNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class ModuleNode : public AptNode
  {
  public:
    ModuleNode(const SrcPos& srcpos,
               const String& modName, const String& publicId);
    virtual ModuleNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
    String fModName;
    String fPublicId;
  };


  //--------------------------------------------------------------------------

  class ExportNode : public AptNode
  {
  public:
    enum VizType {
      kPrivate,
      kInner,
      kOuter,
      kPublic,
    };

    ExportNode(const SrcPos& srcpos,
               VizType viz,
               bool isFinal,
               const StringList& symbols);
    virtual ExportNode* clone() const;
    virtual void display(Port<Octet>* port) const;

    const char* vizAttr(VizType viz) const;

  private:
    VizType fViz;
    bool    fIsFinal;
    StringList fSymbols;
  };


  //--------------------------------------------------------------------------

  class ImportNode : public AptNode
  {
  public:
    ImportNode(const SrcPos& srcpos,
               const String& codeFile,
               const StringStringMap& renames);
    virtual ImportNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
    String fCodeFile;
    StringStringMap fRenames;
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

  protected:
    String       fSymbolName;
    Type         fType;
    Ptr<AptNode> fInitExpr;
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, AptNode* defined);

    AptNode* defNode() const;

  protected:
    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode
  {
  public:
    LetNode(AptNode* node);
    virtual LetNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(AptNode* node);
    virtual DefNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  enum VardefFlags {
    kNormalVar,
    kFluidVar,
    kConstVar,
    kConfigVar
  };

  class VardefNode : public BindingNode
  {
  public:
    VardefNode(const SrcPos& srcpos,
               const String& symbolName, VardefFlags flags,
               const Type& type, AptNode* initExpr);

    virtual VardefNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
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
    virtual void display(Port<Octet>* port) const;
    
    ParamFlags flags() const;
    const String& key() const;

  private:
    String fKey;
    ParamFlags fFlags;
  };


  enum SlotFlags {
    kSimpleSlot     = 0,
    kTransientSlot  = 1 << 0,
    kReadonlySlot   = 1 << 1,
    kObservableSlot = 1 << 2
  };

  class SlotdefNode : public BindingNode
  {
  public:
    SlotdefNode(const SrcPos& srcpos,
                const String& symbolName,
                unsigned int flags,
                const Type& type, AptNode* initExpr);

    virtual SlotdefNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
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
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class VectorNode : public AptNode
  {
  public:
    VectorNode(const SrcPos& srcpos)
      : AptNode(srcpos)
    { }

    virtual VectorNode* clone() const;
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class DictNode : public AptNode
  {
  public:
    DictNode(const SrcPos& srcpos)
      : AptNode(srcpos)
    { }

    virtual DictNode* clone() const;
    virtual void display(Port<Octet>* port) const;

    void addPair(AptNode* key, AptNode* value);
  };


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos,
               AptNode* left, OperatorType op, AptNode* right);

    virtual BinaryNode* clone() const;
    virtual void display(Port<Octet>* port) const;

    OperatorType op() const;
    AptNode* left() const;
    AptNode* right() const;

    void setLeft(AptNode* node);
    void setRight(AptNode* node);


    bool isMapTo() const;

  private:
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
    virtual void display(Port<Octet>* port) const;

  private:
    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              AptNode* from, AptNode* to, AptNode* by);
    virtual RangeNode* clone() const;
    virtual void display(Port<Octet>* port) const;

    AptNode* from() const;
    AptNode* to() const;
    AptNode* by() const;

  private:
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
    virtual void display(Port<Octet>* port) const;

  private:
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
    virtual void display(Port<Octet>* port) const;

    AptNode* lvalue() const;
    AptNode* rvalue() const;

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
    virtual void display(Port<Octet>* port) const;

    AptNode* test() const;
    AptNode* consequent() const;
    AptNode* alternate() const;

  private:
    Ptr<AptNode> fTest;
    Ptr<AptNode> fConsequent;
    Ptr<AptNode> fAlternate;
  };


  //--------------------------------------------------------------------------

  class OnNode : public AptNode
  {
  public:
    OnNode(const SrcPos& srcpos,
           const String& key, const NodeList& params, AptNode* body);

    virtual OnNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
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
    virtual void display(Port<Octet>* port) const;
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
    virtual void display(Port<Octet>* port) const;

    const NodeList& params() const;
    const Type& retType() const;

  protected:
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
    virtual void display(Port<Octet>* port) const;

    const String& funcName() const;
    bool isGeneric() const;
    bool isAbstract() const;

  private:
    String       fSym;
    unsigned int fFlags;
  };


  //--------------------------------------------------------------------------

  class ApplyNode : public AptNode
  {
  public:
    ApplyNode(const SrcPos& srcpos, AptNode* base);

    virtual ApplyNode* clone() const;
    virtual void display(Port<Octet>* port) const;

  private:
    Ptr<AptNode> fBase;
  };


  //--------------------------------------------------------------------------

  class KeyargNode : public AptNode
  {
  public:
    KeyargNode(const SrcPos& srcpos, const String& key, AptNode* value);

    virtual KeyargNode* clone() const;
    virtual void display(Port<Octet>* port) const;

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
    virtual void display(Port<Octet>* port) const;

  private:
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
    virtual void display(Port<Octet>* port) const;

  private:
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
