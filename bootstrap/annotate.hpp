/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "compilepass.hpp"

#include <memory>
#include <vector>


namespace herschel {
class AstNode;
class Compiler;

using NodeList = std::vector<std::shared_ptr<AstNode>>;

//------------------------------------------------------------------------------

//! Defines the "annotate" pass over the abstract parse tree.
//!
//! Currently the annotate pass detects shared variable access (which is
//! required for closure detection).
class Annotator {
public:
  Annotator(Compiler& compiler);

  std::shared_ptr<AstNode> annotateNode(std::shared_ptr<AstNode> node);
  void annotateNodeList(NodeList& nl, bool marktailpos, bool marksingletype);

  void replaceNode(std::shared_ptr<AstNode> newNode);

  //-------- data members

  Compiler& fCompiler;  // backlink to owning compiler
  std::shared_ptr<AstNode> fNewNode;
};


//--------------------------------------------------------------------------

//! \c TokenCompilePass wrapper for the \c Annotator pass to be used in the
//! process pipeline as fourth pass.

class AnnotatePass : public AstNodeCompilePass {
public:
  AnnotatePass(int level, Compiler& compiler);
  std::shared_ptr<AstNode> doApply(std::shared_ptr<AstNode> src) override;

private:
  Compiler& fCompiler;  // backlink to owning compiler
};

}  // namespace herschel
