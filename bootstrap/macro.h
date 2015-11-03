/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <map>
#include <vector>

//#include "pass1.h"
#include "ptr.h"
#include "refcountable.h"
#include "srcpos.h"
#include "str.h"
#include "token.h"
#include "registry.h"

namespace herschel
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

  using MacroPatternVector = std::vector<MacroPattern>;


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
    using NodeMap = std::map<Token, Ptr<SyntaxTreeNode>>;

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
    kMacro_name,
    kMacro_param,
    kMacro_posParam,
    kMacro_namedParam,
    kMacro_restParam,
    kMacro_paramlist,
    kMacro_operator
  };

  MacroParamType macroParamType(const Token& token, String* paramName);

  //----------------------------------------------------------------------------

  class SyntaxTable : public RefCountable
  {
  public:
    using PatternMap = std::map<String, Ptr<SyntaxTreeNode>>;

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

} // namespace
