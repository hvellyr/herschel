/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <map>

#include "properties.h"
#include "str.h"
#include "ptr.h"
#include "token.h"
#include "parsertypes.h"
#include "registry.h"


using namespace heather;

static bool sVerbose = false;
static bool sIsTokenizerTracing = false;
static bool sIsPass1Tracing = false;
static bool sIsPass2Tracing = false;
static bool sIsImportFileTracing = false;
static bool sDontImport = false;
static String sOutdir;
static Ptr<ConfigVarRegistry> sConfigVarRegistry;

void
Properties::setIsVerbose(bool value)
{
  sVerbose = value;
}


bool
Properties::isVerbose()
{
  return sVerbose;
}


void
Properties::setOutdir(const String& outdir)
{
  sOutdir = outdir;
}


String
Properties::outdir()
{
  return sOutdir;
}


void
Properties::setTrace(const String& key, bool value)
{
  if (key == String("tokenizer"))
    sIsTokenizerTracing = value;
  else if (key == String("pass1"))
    sIsPass1Tracing = value;
  else if (key == String("pass2"))
    sIsPass2Tracing = value;
  else if (key == String("import"))
    sIsImportFileTracing = value;
}


void
Properties::setTraces(const String& argument)
{
  String tmp = argument;
  String key;

  while (tmp.split(',', key, tmp) >= 0) {
    // String key = first.trim();
    setTrace(key, true);
  }
  if (!tmp.isEmpty())
    setTrace(tmp, true);
}


bool
Properties::isTraceTokenizer()
{
  return sIsTokenizerTracing;
}


bool
Properties::isTracePass1()
{
  return sIsPass1Tracing;
}


bool
Properties::isTracePass2()
{
  return sIsPass2Tracing;
}


bool
Properties::isTraceImportFile()
{
  return sIsImportFileTracing;
}


void
Properties::setConfigVar(const String& keyValuePair)
{
  String key;
  String value;
  int idx = keyValuePair.split('=', key, value);
  if (idx < 0)
    idx = keyValuePair.split(':', key, value);

  if (idx >= 0) {
    if (sConfigVarRegistry == NULL)
      sConfigVarRegistry = new ConfigVarRegistry;
    sConfigVarRegistry->registerValue(key, Token(SrcPos("<commandline>", 0),
                                                 kString, value));
  }
  else
    fprintf(stderr, "ERROR: bad key-value pair for config key.  Ignored\n");
}


ConfigVarRegistry*
Properties::globalConfigVarRegistry()
{
  if (sConfigVarRegistry == NULL)
    sConfigVarRegistry = new ConfigVarRegistry;
  return sConfigVarRegistry;
}


#if defined(UNITTESTS)
void
Properties::test_setDontImport(bool value)
{
  sDontImport = value;
}


bool
Properties::test_dontImport()
{
  return sDontImport;
}
#endif
