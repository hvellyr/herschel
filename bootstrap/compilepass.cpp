/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "apt.h"
#include "compilepass.h"
#include "port.h"
#include "properties.h"
#include "ptr.h"
#include "token.h"
#include "xmlout.h"

//----------------------------------------------------------------------------

using namespace herschel;


Token
TokenCompilePass::apply(const Token& src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  Token t = src;

  if (doPass) {
    t = doApply(src);

    if (doTrace && Properties::isTracePass(passLevel())) {
      Ptr<FilePort> stream = new FilePort(stdout);
      display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
      t.toPort(stream);
      displayln(stream, "");
    }
  }

  return t;
}


std::shared_ptr<AptNode>
Token2AptNodeCompilePass::apply(const Token& src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    auto n = doApply(src);
    if (doTrace && Properties::isTracePass(passLevel()) && n) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
      out->render(*n);
    }

    return n;
  }

  return nullptr;
}


std::shared_ptr<AptNode>
AptNodeCompilePass::apply(std::shared_ptr<AptNode> src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    auto n = doApply(src);

    if (doTrace && Properties::isTracePass(passLevel()) && n) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout),
                                             fShowNodeType);
      out->render(*n);
    }

    return n;
  }
  else {
    return src;
  }
}
