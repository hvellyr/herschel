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
    virtual void display(Port<Octet>* port) const = 0;

    virtual void appendNode(AptNode* node);

  protected:
    NodeList fChildren;
  };


  //--------------------------------------------------------------------------

  class StringNode : public AptNode
  {
  public:
    StringNode(const String& value);

    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class KeywordNode : public AptNode
  {
  public:
    KeywordNode(const String& value);

    virtual void display(Port<Octet>* port) const;
  private:
    String fValue;
  };


  //--------------------------------------------------------------------------

  class SymbolNode : public AptNode
  {
  public:
    SymbolNode(const String& value);

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
    NumberNode(T value, bool isImaginary)
      : fValue(value),
        fIsImaginary(isImaginary)
    { }

    T fValue;
    bool fIsImaginary;
  };


  //--------------------------------------------------------------------------

  class IntNode : public NumberNode<int>
  {
  public:
    IntNode(int value, bool isImaginary);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RealNode : public NumberNode<double>
  {
  public:
    RealNode(double value, bool isImaginary);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class RationalNode : public NumberNode<Rational>
  {
  public:
    RationalNode(const Rational& value, bool isImaginary);
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class CharNode : public AptNode
  {
  public:
    CharNode(Char value);
    virtual void display(Port<Octet>* port) const;
  private:
    Char fValue;
  };


  //--------------------------------------------------------------------------

  class CompileUnitNode : public AptNode
  {
  public:
    CompileUnitNode();
    virtual void display(Port<Octet>* port) const;
  };


  //--------------------------------------------------------------------------

  class ModuleNode : public AptNode
  {
  public:
    ModuleNode(const String& modName, const String& publicId,
               bool isModule);
    virtual void display(Port<Octet>* port) const;

  private:
    bool   fIsModule;
    String fModName;
    String fPublicId;
  };


  //--------------------------------------------------------------------------

  class ExportNode : public AptNode
  {
  public:
    ExportNode(const StringList& flags,
               const StringList& symbols);
    virtual void display(Port<Octet>* port) const;

  private:
    StringList fFlags;
    StringList fSymbols;
  };


  //--------------------------------------------------------------------------

  class ImportNode : public AptNode
  {
  public:
    ImportNode(const String& codeFile,
               const StringStringMap& renames);
    virtual void display(Port<Octet>* port) const;

  private:
    String fCodeFile;
    StringStringMap fRenames;
  };


  //--------------------------------------------------------------------------

  enum VardefFlags {
    kNoFlags,
    kIsFluid,
    kIsConst,
    kIsConfig
  };

  class VardefNode : public AptNode
  {
  public:
    VardefNode(const String& symbolName, VardefFlags flags,
               AptNode* type, AptNode* initExpr);
    virtual void display(Port<Octet>* port) const;

  private:
    String fSymbolName;
    VardefFlags fFlags;
    Ptr<AptNode> fType;
    Ptr<AptNode> fInitExpr;
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
    BinaryNode(AptNode* left, OperatorType op, AptNode* right);

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
};

#endif  // bootstrap_apt_h
