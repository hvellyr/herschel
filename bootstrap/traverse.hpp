/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <memory>
#include <vector>


namespace herschel {
class AstNode;

using NodeList = std::vector<std::shared_ptr<AstNode>>;


// class TraverseDelegate {
// public:
//   virtual ~TraverseDelegate() {}

//   virtual bool preApply(std::shared_ptr<AstNode> node) = 0;
//   virtual void postApply(std::shared_ptr<AstNode> node) = 0;
// };


enum class TraversePhase { before, after };

template <typename Delegate>
class Traversator {
public:
  Traversator(Delegate& delegate)
      : fDelegate(delegate)
  {
  }

  void traverseNode(std::shared_ptr<AstNode> node);
  void traverseNodeList(NodeList& nl);

  bool apply(std::shared_ptr<AstNode> node, TraversePhase phase);

  Delegate& fDelegate;
};
};  // namespace herschel


#include "traverse.ipp"
