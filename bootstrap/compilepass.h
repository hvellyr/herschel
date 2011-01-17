/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_compilepass_h
#define bootstrap_compilepass_h

#include "refcountable.h"


namespace heather
{
  class Token;
  class AptNode;

  template<typename TIn, typename TOut>
  class CompilePass : public RefCountable
  {
  public:
    virtual TOut apply(TIn src, bool doTrace) = 0;

  protected:
    virtual TOut doApply(TIn src) = 0;

    virtual int passLevel() const = 0;
  };


  class TokenCompilePass : public CompilePass<const Token&, Token>
  {
  public:
    TokenCompilePass(int level)
      : fLevel(level)
    {}

    ~TokenCompilePass() { }

    virtual Token apply(const Token& src, bool doTrace);

    virtual int passLevel() const { return fLevel; }

  private:
    int fLevel;
  };


  class Token2AptNodeCompilePass : public CompilePass<const Token&, AptNode*>
  {
  public:
    Token2AptNodeCompilePass(int level)
      : fLevel(level)
    {}

    ~Token2AptNodeCompilePass() { }

    virtual AptNode* apply(const Token& src, bool doTrace);

    virtual int passLevel() const { return fLevel; }

  private:
    int fLevel;
  };


  class AptNodeCompilePass : public CompilePass<AptNode*, AptNode*>
  {
  public:
    AptNodeCompilePass(int level, bool showNodeType = false)
      : fLevel(level),
        fShowNodeType(showNodeType)
    {}

    ~AptNodeCompilePass() { }

    virtual AptNode* apply(AptNode* src, bool doTrace);

    virtual int passLevel() const { return fLevel; }

  private:
    int fLevel;
    bool fShowNodeType;
  };
};                              // namespace

#endif                          // bootstrap_compilepass_h
