/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "compilepass.hpp"

#include <vector>
#include <memory>


namespace herschel
{
  class AstNode;
  class Compiler;
  class Scope;

  using NodeList = std::vector<std::shared_ptr<AstNode>>;

  //------------------------------------------------------------------------------

  //! Defines the "annotate" pass over the abstract parse tree.
  //!
  //! Currently the annotate pass detects shared variable access (which is
  //! required for closure detection).
  class Annotator
  {
  public:
    Annotator(std::shared_ptr<Scope> scope, Compiler& compiler);

    void annotateNode(std::shared_ptr<AstNode> node);
    void annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype);

    //-------- data members

    std::shared_ptr<Scope> fScope;
    Compiler&  fCompiler;       // backlink to owning compiler
  };


  //--------------------------------------------------------------------------

  //! \c TokenCompilePass wrapper for the \c Annotator pass to be used in the
  //! process pipeline as fourth pass.

  class AnnotatePass : public AstNodeCompilePass
  {
  public:
    AnnotatePass(int level, std::shared_ptr<Scope> scope, Compiler& compiler);
    std::shared_ptr<AstNode> doApply(std::shared_ptr<AstNode> src) override;

  private:
    std::shared_ptr<Scope> fScope;
    Compiler&  fCompiler;       // backlink to owning compiler
  };

} // namespace
