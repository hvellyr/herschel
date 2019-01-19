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
class Scope;

using NodeList = std::vector<std::shared_ptr<AstNode>>;

//! Defines the 2nd annotate pass over the abstract parse tree.
class Annotator2 {
public:
  Annotator2(Compiler& compiler);

  void annotateNode(std::shared_ptr<AstNode> node);
  void annotateNodeList(const NodeList& nl);

  Compiler& fCompiler;  // backlink to owning compiler
};


//! \c TokenCompilePass wrapper for the \c Annotator pass to be used in the
//! process pipeline as fourth pass.
class Annotate2Pass : public AstNodeCompilePass {
public:
  Annotate2Pass(int level, Compiler& compiler);
  std::shared_ptr<AstNode> doApply(std::shared_ptr<AstNode> src) override;

private:
  Compiler& fCompiler;  // backlink to owning compiler
};

}  // namespace herschel
