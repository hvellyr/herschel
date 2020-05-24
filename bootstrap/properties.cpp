/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "properties.hpp"

#include "log.hpp"
#include "parsertypes.hpp"
#include "registry.hpp"
#include "str.hpp"
#include "token.hpp"

#include <map>


namespace herschel {

#if defined(UNITTESTS)
static bool sDontImport = false;
#endif

static CompileOutFormat sCompileOutFormat = kLLVM_IR;
static OptimizeLevel sOptLevel = kOptLevelBasic;
static std::shared_ptr<ConfigVarRegistry> sConfigVarRegistry;
static String sOutdir;
static StringVector sInputSearchPath;
static StringVector sSystemSearchPath;
static bool sIsImportFileTracing = false;
static bool sIsFileSearchTracing = false;
static bool sIsMacroTracing = false;
static bool sIsCodeDump = false;
static bool sIsPass1Tracing = false;
static bool sIsPass2Tracing = false;
static bool sIsAnnotateTracing = false;
static bool sIsTransformTracing = false;
static bool sIsTokenizerTracing = false;
static bool sIsTypifyTracing = false;
static bool sIsTypeConvDump = false;
#if defined(UNITTESTS)
static int sPassLevel = 5;
#endif
static bool sShouldIgnoreDocStrings = true;
static bool sVerbose = false;


void Properties::setIsVerbose(bool value)
{
  sVerbose = value;
}


bool Properties::isVerbose()
{
  return sVerbose;
}


void Properties::setOutdir(const String& outdir)
{
  sOutdir = outdir;
}


String Properties::outdir()
{
  return sOutdir;
}


void Properties::setCompileOutFormat(CompileOutFormat format)
{
  sCompileOutFormat = format;
}


CompileOutFormat Properties::compileOutFormat()
{
  return sCompileOutFormat;
}


void Properties::setOptimizeLevel(OptimizeLevel optLevel)
{
  sOptLevel = optLevel;
}


OptimizeLevel Properties::optimizeLevel()
{
  return sOptLevel;
}


void Properties::setTrace(const String& key, bool value)
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
  else if (key == String("typify"))
    sIsTypifyTracing = value;
  else if (key == String("import"))
    sIsImportFileTracing = value;
  else if (key == String("filesearch"))
    sIsFileSearchTracing = value;
  else if (key == String("macro"))
    sIsMacroTracing = value;
  else if (key == String("codedump"))
    sIsCodeDump = value;
  else if (key == String("typeconv"))
    sIsTypeConvDump = value;
}


void Properties::setTraces(const String& argument)
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


bool Properties::isTraceTokenizer()
{
  return sIsTokenizerTracing;
}


bool Properties::isTracePass(int level)
{
  switch (level) {
  case 0: return false;
  case 1: return sIsPass1Tracing;
  case 2: return sIsPass2Tracing;
  case 3: return sIsTransformTracing;
  case 4: return sIsAnnotateTracing;
  case 5: return sIsTypifyTracing;
  default: hr_invalid("Missing pass level setting");
  }
  return false;
}


bool Properties::isTracePass1()
{
  return sIsPass1Tracing;
}


bool Properties::isTracePass2()
{
  return sIsPass2Tracing;
}


bool Properties::isTraceAnnotate()
{
  return sIsAnnotateTracing;
}


bool Properties::isTraceTransform()
{
  return sIsTransformTracing;
}


bool Properties::isTraceTypify()
{
  return sIsTypifyTracing;
}


bool Properties::isTraceImportFile()
{
  return sIsImportFileTracing;
}


bool Properties::isTraceFileSearch()
{
  return sIsFileSearchTracing;
}


bool Properties::isTraceMacro()
{
  return sIsMacroTracing;
}


bool Properties::isCodeDump()
{
  return sIsCodeDump;
}


bool Properties::isTypeConvDump()
{
  return sIsTypeConvDump;
}


void Properties::setConfigVar(const String& keyValuePair)
{
  String key;
  String value;
  int idx = keyValuePair.split('=', key, value);
  if (idx < 0)
    idx = keyValuePair.split(':', key, value);

  if (idx >= 0) {
    if (!sConfigVarRegistry)
      sConfigVarRegistry = std::make_shared<ConfigVarRegistry>();
    sConfigVarRegistry->registerValue(
        key, Token(SrcPos("<commandline>", 0, 0), kString, value));
  }
  else
    HR_LOG(kError) << "bad key-value pair for config key.  Ignored";
}


std::shared_ptr<ConfigVarRegistry> Properties::globalConfigVarRegistry()
{
  if (!sConfigVarRegistry)
    sConfigVarRegistry = std::make_shared<ConfigVarRegistry>();
  return sConfigVarRegistry;
}


#if defined(UNITTESTS)
void Properties::test_setDontImport(bool value)
{
  sDontImport = value;
}

bool Properties::test_dontImport()
{
  return sDontImport;
}


void Properties::test_setPassLevel(int level)
{
  sPassLevel = level;
}


int Properties::test_passLevel()
{
  return sPassLevel;
}

#endif


bool Properties::shouldIgnoreDocStrings()
{
  return sShouldIgnoreDocStrings;
}

void Properties::setShouldIgnoreDocStrings(bool value)
{
  sShouldIgnoreDocStrings = value;
}


void Properties::addInputDir(const String& dir)
{
  sInputSearchPath.push_back(dir);
}


const StringVector& Properties::inputDirSearchPath()
{
  if (sInputSearchPath.empty())
    sInputSearchPath.push_back(String("."));

  return sInputSearchPath;
}


void Properties::addSystemDir(const String& dir)
{
  sSystemSearchPath.push_back(dir);
}


const StringVector& Properties::systemDirSearchPath()
{
  return sSystemSearchPath;
}

}  // namespace herschel
