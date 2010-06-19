/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_apt_h
#define bootstrap_apt_h

#include <list>
#include <map>

#include "refcountable.h"
#include "port.h"
#include "ptr.h"
#include "numbers.h"
#include "parsertypes.h"

namespace heather
{
  class AptNode;

  //--------------------------------------------------------------------------

  typedef std::list<Ptr<AptNode> > NodeList;
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


    virtual void display(Port<Octet>* port) const = 0;

    virtual void appendNode(AptNode* node);

  protected:
    SrcPos   fSrcPos;
    NodeList fChildren;
  };


  //--------------------------------------------------------------------------

  class StringNode : public AptNode
  {
  public:
    StringNode(const SrcPos& srcpos, const String& value);

    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const SrcPos& srcpos, const String& value);

    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const SrcPos& srcpos, const String& value);

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
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(const SrcPos& srcpos, double value, bool isImaginary);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const SrcPos& srcpos,
                 const Rational& value, bool isImaginary);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(const SrcPos& srcpos, Char value);
    virtual void display(Port<Octet>* port) const;
  private:
    Char fValue;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public AptNode
  {
  public:
    CompileUnitNode(const SrcPos& srcpos);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class ModuleNode : public AptNode
  {
  public:
    ModuleNode(const SrcPos& srcpos,
               const String& modName, const String& publicId);
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
                const String& symbolName, AptNode* type, AptNode* initExpr);

  protected:
    String fSymbolName;
    Ptr<AptNode> fType;
    Ptr<AptNode> fInitExpr;
  };


  //--------------------------------------------------------------------------

  class BaseDefNode : public AptNode
  {
  public:
    BaseDefNode(const SrcPos& srcpos, AptNode* defined);

  protected:
    Ptr<AptNode> fDefined;
  };


  class LetNode : public BaseDefNode
  {
  public:
    LetNode(AptNode* node);
    virtual void display(Port<Octet>* port) const;
  };


  class DefNode : public BaseDefNode
  {
  public:
    DefNode(AptNode* node);
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
               AptNode* type, AptNode* initExpr);

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
              AptNode* type, AptNode* initExpr);

    virtual void display(Port<Octet>* port) const;

  private:
    String fKey;
    ParamFlags fFlags;
  };


  //--------------------------------------------------------------------------

  class ArrayNode : public AptNode
  {
  public:
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class VectorNode : public AptNode
  {
  public:
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class DictNode : public AptNode
  {
  public:
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class BinaryNode : public AptNode
  {
  public:
    BinaryNode(const SrcPos& srcpos,
               AptNode* left, OperatorType op, AptNode* right);

    virtual void display(Port<Octet>* port) const;

    OperatorType op() const;
    AptNode* left() const;
    AptNode* right() const;

    bool isMapTo() const;

  private:
    Ptr<AptNode> fLeft;
    Ptr<AptNode> fRight;
    OperatorType fOp;
  };


  //--------------------------------------------------------------------------

  class RangeNode : public AptNode
  {
  public:
    RangeNode(const SrcPos& srcpos,
              AptNode* from, AptNode* to, AptNode* by);
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

  class AssignNode : public AptNode
  {
  public:
    AssignNode(const SrcPos& srcpos, AptNode* lvalue, AptNode* rvalue);
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
    virtual void display(Port<Octet>* port) const;
  };
};


#endif  // bootstrap_apt_h
