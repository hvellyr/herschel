/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_apt_h
#define bootstrap_apt_h

#include <list>

#include "refcountable.h"
#include "port.h"
#include "ptr.h"


namespace heather
{
  class AptNode;

  //--------------------------------------------------------------------------

  typedef std::list<Ptr<AptNode> > NodeList;


  //--------------------------------------------------------------------------

  class AptNode : public RefCountable
  {
  public:
    virtual void display(Port<Octet>* port) const = 0;

    virtual void appendNode(AptNode* node);

  protected:
    void displayNodeList(Port<Octet>* port,
                         const char* tagName,
                         const NodeList& nodelist) const;

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
};

#endif  // bootstrap_apt_h
