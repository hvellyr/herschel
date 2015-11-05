/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "srcpos.h"
#include "str.h"
#include "token.h"
#include "registry.h"

#include <map>
#include <memory>
#include <vector>


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

  class SyntaxTreeNode
  {
  public:
    using NodeMap = std::map<Token, std::shared_ptr<SyntaxTreeNode>>;

    SyntaxTreeNode();

    std::shared_ptr<SyntaxTreeNode> findNode(const Token& token) const;

    //! find the first macro parameter's node and return the macro parameter
    std::shared_ptr<SyntaxTreeNode> findMacroParam(Token* macroParam) const;

    void setNode(const Token& token, std::shared_ptr<SyntaxTreeNode> node);
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

  class SyntaxTable
  {
  public:
    using PatternMap = std::map<String, std::shared_ptr<SyntaxTreeNode>>;

    static std::shared_ptr<SyntaxTable> compile(const String& macroName,
                                                const MacroPatternVector& patterns);

    std::shared_ptr<SyntaxTreeNode> rootNode() const;
    std::shared_ptr<SyntaxTreeNode> findPattern(const String& name) const;
    void setPattern(const String& name, std::shared_ptr<SyntaxTreeNode> node);

    void mixinPatternPart(std::shared_ptr<SyntaxTreeNode> patternTree,
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

  class Macro
  {
  public:
    Macro(std::shared_ptr<SyntaxTable> table, MacroType type)
      : fSyntaxTable(std::move(table)),
        fMacroType(type)
    { }

    std::shared_ptr<SyntaxTable> syntaxTable() const
    {
      return fSyntaxTable;
    }

    MacroType type() const
    {
      return fMacroType;
    }

  private:
    //-------- data members

    std::shared_ptr<SyntaxTable> fSyntaxTable;
    MacroType        fMacroType;
  };

} // namespace
