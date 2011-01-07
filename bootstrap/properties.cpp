/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "common.h"

#include <map>

#include "properties.h"
#include "log.h"
#include "str.h"
#include "ptr.h"
#include "token.h"
#include "parsertypes.h"
#include "registry.h"


using namespace heather;

#if defined(UNITTESTS)
static bool sDontImport = false;
#endif

static CompileOutFormat sCompileOutFormat = kLLVM_IR;
static OptimizeLevel sOptLevel = kOptLevelBasic;
static Ptr<ConfigVarRegistry> sConfigVarRegistry;
static String sOutdir;
static StringVector sInputSearchPath;
static bool sIsImportFileTracing = false;
static bool sIsMacroTracing = false;
static bool sIsPass1Tracing = false;
static bool sIsPass2Tracing = false;
static bool sIsAnnotateTracing = false;
static bool sIsTransformTracing = false;
static bool sIsTokenizerTracing = false;
static int sPassLevel = 4;
static bool sShouldIgnoreDocStrings = true;
static bool sVerbose = false;

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
Properties::setCompileOutFormat(CompileOutFormat format)
{
  sCompileOutFormat = format;
}


CompileOutFormat
Properties::compileOutFormat()
{
  return sCompileOutFormat;
}


void
Properties::setOptimizeLevel(OptimizeLevel optLevel)
{
  sOptLevel = optLevel;
}


OptimizeLevel
Properties::optimizeLevel()
{
  return sOptLevel;
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
  else if (key == String("annotate"))
    sIsAnnotateTracing = value;
  else if (key == String("transform"))
    sIsTransformTracing = value;
  else if (key == String("import"))
    sIsImportFileTracing = value;
  else if (key == String("macro"))
    sIsMacroTracing = value;
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
Properties::isTraceAnnotate()
{
  return sIsAnnotateTracing;
}


bool
Properties::isTraceTransform()
{
  return sIsTransformTracing;
}


bool
Properties::isTraceImportFile()
{
  return sIsImportFileTracing;
}


bool
Properties::isTraceMacro()
{
  return sIsMacroTracing;
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
    logf(kError, "bad key-value pair for config key.  Ignored\n");
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


void
Properties::test_setPassLevel(int level)
{
  sPassLevel = level;
}


int
Properties::test_passLevel()
{
  return sPassLevel;
}

#endif


bool
Properties::shouldIgnoreDocStrings()
{
  return sShouldIgnoreDocStrings;
}

void
Properties::setShouldIgnoreDocStrings(bool value)
{
  sShouldIgnoreDocStrings = value;
}


void
Properties::addInputDir(const String& dir)
{
  sInputSearchPath.push_back(dir);
}


const StringVector&
Properties::inputDirSearchPath()
{
  if (sInputSearchPath.empty())
    sInputSearchPath.push_back(String("."));

  return sInputSearchPath;
}
