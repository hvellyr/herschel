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
#include "pexpr.h"
#include "parsertypes.h"
#include "registry.h"


using namespace heather;

static bool sVerbose = false;
static bool sIsTokenizerTracing = false;
static bool sIsPass1Tracing = false;
static bool sIsPass2Tracing = false;
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
    sConfigVarRegistry->registerValue(key, Token(kString, value));
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
