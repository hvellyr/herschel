/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"
#include "properties.h"
#include "str.h"

using namespace heather;

static bool sVerbose = false;
static bool sIsTokenizerTracing = false;
static bool sIsParserTracing = false;
static String sOutdir;


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
  else if (key == String("parser"))
    sIsParserTracing = value;
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
Properties::isTraceParser()
{
  return sIsParserTracing;
}
