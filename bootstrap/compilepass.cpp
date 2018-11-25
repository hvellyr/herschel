/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "compilepass.hpp"

#include "ast.hpp"
#include "port.hpp"
#include "properties.hpp"
#include "token.hpp"
//#include "xmlrenderer.hpp"


namespace herschel {


Token TokenCompilePass::apply(const Token& src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  Token t = src;

  if (doPass) {
    t = doApply(src);

    // if (doTrace && Properties::isTracePass(passLevel())) {
    //   FilePort stream{ stdout };
    //   display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
    //   t.toPort(stream);
    //   displayln(stream, "");
    // }
  }

  return t;
}


std::shared_ptr<AstNode> Token2AstNodeCompilePass::apply(const Token& src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    auto n = doApply(src);
    // if (doTrace && Properties::isTracePass(passLevel()) && n) {
    //   XmlRenderer out{ std::make_shared<FilePort>(stdout) };
    //   out.render(*n);
    // }

    return n;
  }

  return nullptr;
}


std::shared_ptr<AstNode> AstNodeCompilePass::apply(std::shared_ptr<AstNode> src,
                                                   bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    auto n = doApply(src);

    // if (doTrace && Properties::isTracePass(passLevel()) && n) {
    //   XmlRenderer out{ std::make_shared<FilePort>(stdout), fShowNodeType };
    //   out.render(*n);
    // }

    return n;
  }
  else {
    return src;
  }
}

}  // namespace herschel
