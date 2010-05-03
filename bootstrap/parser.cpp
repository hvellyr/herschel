/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <map>

#include "parser.h"
#include "tokenizer.h"
#include "pass1.h"
#include "pass2.h"
#include "properties.h"

using namespace heather;


//----------------------------------------------------------------------------

Parser::Parser()
  : fCharRegistry(new CharRegistry)
{
}


Parser::Parser(Parser* parent)
  : fParent(parent),
    fCharRegistry(parent->charRegistry())
{
}


CharRegistry*
Parser::charRegistry() const
{
  return fCharRegistry;
}


Token
Parser::nextToken()
{
  try {
    fToken = fPort->read();
  }
  catch (const EofException& e) {
    fToken = Token(kEOF);
  }
  return fToken;
}


AptNode*
Parser::parse(Port<Char>* port)
{
  fPort = new FileTokenPort(port, fCharRegistry);

  try {
    FirstPass firstPass(this, fToken);

    Pexpr parsedExprs = firstPass.parse();

    if (Properties::isTracePass1()) {
      Ptr<FilePort> stream = new FilePort(stdout);
      display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
      parsedExprs.toPort(stream);
      displayln(stream, "");
    }

    SecondPass secondPass(this);

    Ptr<AptNode> apt = secondPass.parse(parsedExprs);
    if (Properties::isTracePass2() && apt != NULL) {
      Ptr<FilePort> stream = new FilePort(stdout);
      display(stream, "<?xml version='1.0' encoding='utf-8'?>\n");
      apt->display(stream);
      displayln(stream, "");
    }

    return apt.release();
  }
  catch (const Exception& e) {
    fprintf(stderr, "Parse error: %s\n", (const char*)StrHelper(e.message()));
  }

  return NULL;
}
