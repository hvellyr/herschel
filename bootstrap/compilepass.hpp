/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <memory>


namespace herschel {

class Token;
class AstNode;

//! Interface specifying the protocol of a compiler pass
//!
//! It is templated on the type of the input and output items, i.e. a pass
//! consuming tokens and producing nodes will be typed as
//! <tt>CompilePass\<const Token&, herschel::AstNode*><tt>.
template <typename TIn, typename TOut>
class CompilePass {
public:
  //! Apply the compile pass to \p src and return a transformed value.  If
  //! \p doTrace is \c true render a trace of the transformed return value.
  virtual TOut apply(TIn src, bool doTrace) = 0;

protected:
  //! Apply the compile pass to \p src and return a transformed value.  This
  //! function is called from \c apply() to do the proper transformation.
  virtual TOut doApply(TIn src) = 0;

  //! Return the current passlevel.
  virtual int passLevel() const = 0;
};


//! Abstract base class for token transformation compiler passes.
//!
//! Such passes consume and produce tokens.  Subclasses have to provide at
//! least the \c CompilePass::doApply().
class TokenCompilePass : public CompilePass<const Token&, Token> {
public:
  TokenCompilePass(int level)
      : fLevel(level)
  {
  }

  ~TokenCompilePass() {}

  Token apply(const Token& src, bool doTrace) override;

  int passLevel() const override { return fLevel; }

private:
  int fLevel;
};


//! Abstract base class for compiler passes generating a APT node tree from
//! a stream of Tokens.
//!
//! Subclasses have to provide at least the \c CompilePass::doApply().
class Token2AstNodeCompilePass
    : public CompilePass<const Token&, std::shared_ptr<AstNode>> {
public:
  Token2AstNodeCompilePass(int level)
      : fLevel(level)
  {
  }

  ~Token2AstNodeCompilePass() {}

  std::shared_ptr<AstNode> apply(const Token& src, bool doTrace) override;

  int passLevel() const override { return fLevel; }

private:
  int fLevel;
};


//! Abstract base class for APT node tree transformation compiler passes.
//!
//! Subclasses have to provide at least the \c CompilePass::doApply().
class AstNodeCompilePass
    : public CompilePass<std::shared_ptr<AstNode>, std::shared_ptr<AstNode>> {
public:
  AstNodeCompilePass(int level, bool showNodeType = false)
      : fLevel(level)
   // , fShowNodeType(showNodeType)
  {
  }

  ~AstNodeCompilePass() {}

  std::shared_ptr<AstNode> apply(std::shared_ptr<AstNode> src, bool doTrace) override;

  int passLevel() const override { return fLevel; }

private:
  int fLevel;
  //bool fShowNodeType;
};

}  // namespace herschel
