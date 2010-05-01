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

  class NumberNode : public AptNode
  {
  public:
  };


  //--------------------------------------------------------------------------

  class IntNode : public NumberNode
  {
  public:
    IntNode(int value);
    virtual void display(Port<Octet>* port) const;

  private:
    int fValue;
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
};

#endif  // bootstrap_apt_h
