/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

#include <vector>

#if defined(UNITTESTS)
#  include <iostream>
#  include <UnitTest++.h>
#  include <TestReporterStdout.h>
#  include <XmlTestReporter.h>
#endif

#include "apt.h"
#include "common.h"
#include "compiler.h"
#include "log.h"
#include "option.h"
#include "properties.h"
#include "ptr.h"
#include "str.h"
#include "setup.h"


using namespace herschel;

#if defined(UNITTESTS)
static int runUnitTests();
#endif

static void
displayVersion()
{
  printf("%s %s - herschel compiler\n", "hrc", VERSION);
  printf("Copyright (c) %s, %s\n", COPYRIGHTYEAR, COPYRIGHTOWNER);
  printf("(base revision: %s)\n", HR_BASE_REVISION);
}


static void
displayHelp()
{
  displayVersion();
  /*      123456789012345678901234567890123456789012345678901234567890123456789012*/
  /*               1         2         3         4         5         6         7  */
  printf("\n");
  printf("Usage: hrc [options] files...\n");
  printf("Options:\n");
  printf("  -h,      --help              Display this information\n");
  printf("  -v,      --version           Display the version\n");
  printf("           --verbose           Be verbose\n");
  printf("  -D VAR=VALUE                 Define config VAR to be VALUE\n");
  printf("     --define=VAR=VALUE\n");
  printf("  -T KEYS, --trace=KEYS        Trace various aspects:\n");
  printf("                               {tokenizer|pass1|pass2|annotate|\n");
  printf("                                transform|typify|import|macro|\n");
  printf("                                codedump|typeconv}\n");
  printf("  -d DIR,  --outdir=DIR        Output all generated files to DIR\n");
  printf("  -I DIR,  --input=DIR         Add DIR to the input searchlist\n");
  printf("  -O                           Optimize code more\n");
  printf("  -On                          Turn off any (even basic) optimization\n");
#if defined(UNITTESTS)
  printf("  -UT,     --run-unit-tests    Run unit tests for the compiler\n");
  printf("           --ut-format=FORMAT  Output format of unit tests {xml|txt}\n");
  printf("           --parse-1           Only do pass1 phase\n");
  printf("           --parse-2           Only do pass1 + pass2 phase\n");
  printf("           --parse-3           Only do pass1, 2, 3 phase\n");
  printf("           --parse-4           Only do pass1..4 phase\n");
#endif
  printf("  -P,      --parse             Only parse the source files\n");
  printf("  -c                           Only compile the source files, no link\n");
  printf("  -s                           Compile to LLVM IR\n");
}


enum CompileFunction {
  kDisplayHelp,
#if defined(UNITTESTS)
  kRunUnitTests,
#endif
  kParseFiles,
  kCompileFiles,
};


enum {
  kOptHelp = 1,
  kOptVersion,
  kOptOutdir,
  kOptOutput,
  kOptVerbose,
  kOptTrace,
  kOptParse,
  kOptDefine,
  kOptInputDir,
  kOptInputSysDir,
  kOptCompile,
  kOptCompileToIR,
  kOptOptimizeMore,
  kOptOptimizeNone,

#if defined(UNITTESTS)
  kOptRunUnitTests,
  kOptUTFormat,
  kOptDontImport,
  kOptParse1,
  kOptParse2,
  kOptParse3,
  kOptParse4,
#endif
};

#if defined(UNITTESTS)
static String sUnitTestFormat;
#endif

namespace herschel
{
  static void setupDefaultPath();
};


int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine herschelOptions[] = {
    { kOptHelp,         "-h",  "--help",           !K(argument) },
    { kOptVersion,      "-v",  "--version",        !K(argument) },
    { kOptOutdir,       "-d",  "--outdir",          K(argument) },
    { kOptOutput,       "-o",  "--output",          K(argument) },
    { kOptVerbose,      NULL,  "--verbose",        !K(argument) },
    { kOptTrace,        "-T",  "--trace",           K(argument) },
    { kOptParse,        "-P",  "--parse",          !K(argument) },
    { kOptDefine,       "-D",  "--define",          K(argument) },
    { kOptInputDir,     "-I",  "--input",           K(argument) },
    { kOptInputSysDir,  NULL,  "--isys",            K(argument) },
    { kOptCompileToIR,  "-s",  NULL,               !K(argument) },
    { kOptCompile,      "-c",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O1", NULL,               !K(argument) },
    { kOptOptimizeNone, "-On", NULL,               !K(argument) },
#if defined(UNITTESTS)
    { kOptRunUnitTests, "-UT", "--run-unit-tests", !K(argument) },
    { kOptUTFormat,     NULL,  "--ut-format",       K(argument) },
    { kOptDontImport,   NULL,  "--dont-import",    !K(argument) },
    { kOptParse1,       NULL,  "--parse-1",        !K(argument) },
    { kOptParse2,       NULL,  "--parse-2",        !K(argument) },
    { kOptParse3,       NULL,  "--parse-3",        !K(argument) },
    { kOptParse4,       NULL,  "--parse-4",        !K(argument) },
#endif
    { 0,                NULL,  NULL,               !K(argument) } // sentinel
  };

  String outputFile;

  CompileFunction func = kDisplayHelp;
  std::vector<String> files;
  OptionsParser::ArgumentType type;
  OptionsParser::Option option;
  OptionsParser optp(herschelOptions, argc, (const char**)argv);

  setupDefaultPath();

  while ((type = optp.nextOption(&option)) != OptionsParser::kNoMoreArgs) {
    switch (type) {
    case OptionsParser::kOption:
      switch (option.fId) {
      case kOptHelp:
        displayHelp();
        exit(0);
        break;

      case kOptVersion:
        displayVersion();
        exit(0);
        break;

      case kOptOutdir:
        Properties::setOutdir(option.fArgument);
        break;

      case kOptOutput:
        outputFile = option.fArgument;
        break;

      case kOptVerbose:
        Properties::setIsVerbose(true);
        break;

      case kOptTrace:
        Properties::setTraces(option.fArgument);
        break;

      case kOptParse:
        func = kParseFiles;
        break;
      case kOptCompile:
        func = kCompileFiles;
        Properties::setCompileOutFormat(kLLVM_BC);
        break;
      case kOptCompileToIR:
        func = kCompileFiles;
        Properties::setCompileOutFormat(kLLVM_IR);
        break;

      case kOptDefine:
        Properties::setConfigVar(option.fArgument);
        break;

      case kOptInputDir:
        Properties::addInputDir(option.fArgument);
        break;

      case kOptInputSysDir:
        Properties::addSystemDir(option.fArgument);
        break;

      case kOptOptimizeMore:
        Properties::setOptimizeLevel(kOptLevelBasic);
        break;
      case kOptOptimizeNone:
        Properties::setOptimizeLevel(kOptLevelNone);
        break;

#if defined(UNITTESTS)
      case kOptUTFormat:
        sUnitTestFormat = option.fArgument;
        break;
      case kOptRunUnitTests:
        func = kRunUnitTests;
        break;
      case kOptDontImport:
        Properties::test_setDontImport(true);
        break;
      case kOptParse1:
        Properties::test_setPassLevel(1);
        break;
      case kOptParse2:
        Properties::test_setPassLevel(2);
        break;
      case kOptParse3:
        Properties::test_setPassLevel(3);
        break;
      case kOptParse4:
        Properties::test_setPassLevel(4);
        break;
#endif
      }
      break;

    case OptionsParser::kUnknownOption:
      logf(kError, "Unknown option: %s\n", (const char*)StrHelper(option.fOption));
      break;

    case OptionsParser::kMissingArgument:
      logf(kError, "Missing value for option: %s\n",
           (const char*)StrHelper(option.fOption));
      break;

    case OptionsParser::kNotAnOption:
      files.push_back(option.fArgument);
      break;

    default: ;
    }
  }

  switch (func) {
  case kDisplayHelp:
    displayHelp();
    break;

#if defined(UNITTESTS)
  case kRunUnitTests:
    return runUnitTests();
#endif

  case kParseFiles:
    parseFiles(files, outputFile);
    break;

  case kCompileFiles:
    compileFiles(files, outputFile);
    break;
  }

  return 0;
}


static void
herschel::setupDefaultPath()
{
  Setup setup = findResources("hrc");

  for (StringVector::iterator it = setup.fSysPath.begin(),
         e = setup.fSysPath.end();
       it != e; ++it)
  {
    if (!it->isEmpty())
      Properties::addSystemDir(*it);
  }
}


#if defined(UNITTESTS)
static int
runUnitTestsWithRunner(UnitTest::TestRunner& runner)
{
  return runner.RunTestsIf(UnitTest::Test::GetTestList(),
                           NULL, UnitTest::True(), 0);
}


static int
runUnitTests()
{
  // return UnitTest::RunAllTests();
  if (sUnitTestFormat == String("xml")) {
    UnitTest::XmlTestReporter reporter(std::cerr);
    UnitTest::TestRunner runner(reporter);
    return runUnitTestsWithRunner(runner);
  }
  else {
    UnitTest::TestReporterStdout reporter;
    UnitTest::TestRunner runner(reporter);
    return runUnitTestsWithRunner(runner);
  }
}
#endif


