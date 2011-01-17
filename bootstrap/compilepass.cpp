/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "apt.h"
#include "compilepass.h"
#include "port.h"
#include "properties.h"
#include "ptr.h"
#include "token.h"
#include "xmlout.h"

//----------------------------------------------------------------------------

using namespace heather;


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


AptNode*
Token2AptNodeCompilePass::apply(const Token& src, bool doTrace)
{
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    Ptr<AptNode> n = doApply(src);
    if (doTrace && Properties::isTracePass(passLevel()) && n != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout));
      out->render(n);
    }

    return n.release();
  }

  return NULL;
}


AptNode*
AptNodeCompilePass::apply(AptNode* src, bool doTrace)
{
  Ptr<AptNode> n = src;
  bool doPass = true;
#if defined(UNITTESTS)
  doPass = Properties::test_passLevel() >= passLevel();
#endif

  if (doPass) {
    n = doApply(n);

    if (doTrace && Properties::isTracePass(passLevel()) && n != NULL) {
      Ptr<XmlRenderer> out = new XmlRenderer(new FilePort(stdout),
                                             fShowNodeType);
      out->render(n);
    }
  }

  return n.release();
}
