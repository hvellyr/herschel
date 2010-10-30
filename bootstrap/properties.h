/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_properties_h
#define bootstrap_properties_h

#include <vector>

#include "parsertypes.h"

namespace heather
{
  class Token;
  class String;

  typedef std::vector<String> StringVector;

  enum CompileOutFormat
  {
    kNativeObject,
    kLLVM_IR,
    kLLVM_BC
  };

  enum OptimizeLevel
  {
    kOptLevelNone,
    kOptLevelBasic
  };

  class Properties
  {
  public:
    static void setIsVerbose(bool value);
    static bool isVerbose();

    static void setOutdir(const String& outdir);
    static String outdir();

    static void setCompileOutFormat(CompileOutFormat format);
    static CompileOutFormat compileOutFormat();

    static void setOptimizeLevel(OptimizeLevel optLevel);
    static OptimizeLevel optimizeLevel();

    static void setTrace(const String& key, bool value);
    static void setTraces(const String& argument);

    static bool isTraceTokenizer();
    static bool isTracePass1();
    static bool isTracePass2();
    static bool isTraceAnnotate();
    static bool isTraceTransform();
    static bool isTraceTypify();

    static bool isTraceImportFile();
    static bool isTraceMacro();

    static void setConfigVar(const String& keyValuePair);
    static ConfigVarRegistry* globalConfigVarRegistry();

    static void addInputDir(const String& dir);
    static const StringVector& inputDirSearchPath();

#if defined(UNITTESTS)
    static void test_setDontImport(bool value);
    static bool test_dontImport();

    static void test_setPassLevel(int level);
    static int test_passLevel();
#endif

    static bool shouldIgnoreDocStrings();
    static void setShouldIgnoreDocStrings(bool value);
  };
};                              // namespace

#endif   // bootstrap_properties_h
