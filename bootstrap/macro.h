/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_macro_h
#define bootstrap_macro_h

#include <map>
#include <vector>

//#include "pass1.h"
#include "ptr.h"
#include "refcountable.h"
#include "srcpos.h"
#include "str.h"
#include "token.h"
#include "registry.h"

namespace heather
{
  //----------------------------------------------------------------------------

  class MacroPattern
  {
  public:
    MacroPattern(const SrcPos& srcpos,
                 const TokenVector& pattern, const TokenVector& replc)
      : fSrcPos(srcpos),
        fPattern(pattern),
        fReplacement(replc)
    { }

    MacroPattern(const MacroPattern& other)
    {
      *this = other;
    }


    MacroPattern& operator=(const MacroPattern& other)
    {
      fSrcPos      = other.fSrcPos;
      fPattern     = other.fPattern;
      fReplacement = other.fReplacement;
      return *this;
    }

    SrcPos      fSrcPos;
    TokenVector fPattern;
    TokenVector fReplacement;
  };

  typedef std::vector<MacroPattern> MacroPatternVector;


  //----------------------------------------------------------------------------

  enum MacroType
  {
    kMacro_Invalid,
    kMacro_Any,
    kMacro_Def,
    kMacro_On,
    kMacro_Stmt,
    kMacro_Function
  };

  String toString(MacroType type);


  //----------------------------------------------------------------------------

  class SyntaxTreeNode : public RefCountable
  {
  public:
    typedef std::map<Token, Ptr<SyntaxTreeNode> > NodeMap;

    SyntaxTreeNode();

    SyntaxTreeNode* findNode(const Token& token) const;

    //! find the first macro parameter's node and return the macro parameter
    SyntaxTreeNode* findMacroParam(Token* macroParam) const;

    void setNode(const Token& token, SyntaxTreeNode* node);
    void setEndNode(const TokenVector& replacement);
    bool hasEndSet() const;

    String toString() const;

    const TokenVector& replacement() const;

  private:
    NodeMap     fNodes;
    bool        fHasEndPattern;
    TokenVector fEndReplacement;
  };


  //----------------------------------------------------------------------------

  enum MacroParamType
  {
    kMacro_unknown,
    kMacro_expr,
    kMacro_body,
    kMacro_name
  };

  MacroParamType macroParamType(const Token& token, String* paramName);

  //----------------------------------------------------------------------------

  class SyntaxTable : public RefCountable
  {
  public:
    typedef std::map<String, Ptr<SyntaxTreeNode> > PatternMap;

    static SyntaxTable* compile(const String& macroName,
                                const MacroPatternVector& patterns);

    SyntaxTreeNode* rootNode() const;
    SyntaxTreeNode* findPattern(const String& name) const;
    void setPattern(const String& name, SyntaxTreeNode* node);

    void mixinPatternPart(SyntaxTreeNode* patternTree,
                          const TokenVector& pattern,
                          const TokenVector& rplcmtn);
    void mixinPattern(const String& macroName,
                      const TokenVector& pattern,
                      const TokenVector& rplcmnt);

    String toString() const;

  private:
    PatternMap fItems;
  };


  //----------------------------------------------------------------------------

  class Macro : public RefCountable
  {
  public:
    Macro(SyntaxTable* table, MacroType type)
      : fSyntaxTable(table),
        fMacroType(type)
    { }

    SyntaxTable* syntaxTable() const
    {
      return fSyntaxTable;
    }

    MacroType type() const
    {
      return fMacroType;
    }

  private:
    //-------- data members

    Ptr<SyntaxTable> fSyntaxTable;
    MacroType        fMacroType;
  };


  class MacroRegistry : public Registry<Ptr<Macro> >
  {
  public:
    void registerMacro(const String& id, Macro* macro)
    {
      Ptr<Macro> keep = macro;
      registerValue(id, macro);
    }


    Macro* lookupMacro(const String& id) const
    {
      Ptr<Macro> macro;
      // deal with qualified ids and context
      if (lookup(id, &macro))
      {
        return macro;
      }

      return NULL;
    }
  };

};                              // namespace

#endif                          // macro