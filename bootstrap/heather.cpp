#include <stdio.h>
#include <stdlib.h>

#include <vector>

#if defined(UNITTESTS)
#  include <iostream>
#  include <UnitTest++.h>
#  include <TestReporterStdout.h>
#  include <XmlTestReporter.h>
#endif

#include "common.h"
#include "str.h"
#include "option.h"
#include "properties.h"
#include "ptr.h"
#include "apt.h"
#include "log.h"
#include "parser.h"
#include "codegen.h"
#include "file.h"


using namespace heather;

#if defined(UNITTESTS)
static int runUnitTests();
#endif

static void
displayVersion()
{
  printf("%s %s - heather compiler\n", "heather", VERSION);
  printf("Copyright (c) %s, %s\n", COPYRIGHTYEAR, COPYRIGHTOWNER);
  printf("(base revision: %s)\n", HEA_BASE_REVISION);
}


static void
displayHelp()
{
  displayVersion();
  /*      123456789012345678901234567890123456789012345678901234567890123456789012*/
  /*               1         2         3         4         5         6         7  */
  printf("\n");
  printf("Usage: heather [options] files...\n");
  printf("Options:\n");
  printf("  -h,      --help              Display this information\n");
  printf("  -v,      --version           Display the version\n");
  printf("           --verbose           Be verbose\n");
  printf("  -D VAR=VALUE                 Define config VAR to be VALUE\n");
  printf("     --define=VAR=VALUE\n");
  printf("  -T KEYS, --trace=KEYS        Trace various aspects:\n");
  printf("                               {tokenizer|pass1|pass2|import|macro}\n");
  printf("  -d DIR,  --outdir=DIR        Output all generated files to DIR\n");
  printf("  -I DIR,  --input=DIR         Add DIR to the input searchlist\n");
  printf("  -O                           Optimize code more\n");
  printf("  -On                          Turn off any (even basic) optimization\n");
#if defined(UNITTESTS)
  printf("  -UT,     --run-unit-tests    Run unit tests for the compiler\n");
  printf("           --ut-format=FORMAT  Output format of unit tests {xml|txt}\n");
  printf("           --parse-1           Only do pass1 phase\n");
#endif
  printf("  -P,      --parse             Only parse the source files\n");
  printf("  -c                           Only compile the source files, no link\n");
  printf("  -s                           Compile to LLVM IR\n");
  printf("  -b                           Compile to LLVM bitcode\n");
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
  kOptCompile,
  kOptCompileToBC,
  kOptCompileToIR,
  kOptOptimizeMore,
  kOptOptimizeNone,

#if defined(UNITTESTS)
  kOptRunUnitTests,
  kOptUTFormat,
  kOptDontImport,
  kOptParse1,
#endif
};

#if defined(UNITTESTS)
static String sUnitTestFormat;
#endif


static String
makeCompileOutputFileExt()
{
  switch (Properties::compileOutFormat()) {
  case kNativeObject:
    return String("o");
  case kLLVM_IR:
    return String("ll");
  case kLLVM_BC:
    return String("bc");
  }
  assert(0);
  return String();
}


static String
makeOutputFileName(const String& outdir, const String& outfileName,
                   const String& file,
                   const String& outExt)
{
  if (!outfileName.isEmpty())
    return outfileName;

  if (!outdir.isEmpty())
    return file::append(outdir,
                        file::appendExt(file::baseName(file::namePart(file)),
                                        outExt));

  return file::appendExt(file::baseName(file), outExt);
}


static void
compileFile(const String& file, bool doParse, bool doCompile, bool doLink,
            const String& outfileName)
{
  try {
    if (doParse) {
      Ptr<Parser> parser = new Parser;
      Ptr<AptNode> apt = parser->parse(new CharPort(new FilePort(file, "rb")),
                                       file);

      if (doCompile) {
        CompileUnitNode* unit = dynamic_cast<CompileUnitNode*>(apt.obj());
        assert(unit != NULL);

        if (unit != NULL) {
          String outExt = makeCompileOutputFileExt();
          String outFile = makeOutputFileName(Properties::outdir(),
                                              outfileName, file, outExt);

          Ptr<CodeGenerator> codegen = new CodeGenerator();
          codegen->compileToCode(unit, outFile);
        }

        if (doLink) {
        }
      }
    }
  }
  catch (const Exception& e) {
    fprintf(stderr, "ERROR: compilation of '%s' failed: %s\n",
            (const char*)StrHelper(file),
            (const char*)StrHelper(e.message()));
  }
}


int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine heatherOptions[] = {
    { kOptHelp,         "-h",  "--help",           false },
    { kOptVersion,      "-v",  "--version",        false },
    { kOptOutdir,       "-d",  "--outdir",         true  },
    { kOptOutput,       "-o",  "--output",         true  },
    { kOptVerbose,      NULL,  "--verbose",        false },
    { kOptTrace,        "-T",  "--trace",          true  },
    { kOptParse,        "-P",  "--parse",          false },
    { kOptDefine,       "-D",  "--define",         true  },
    { kOptInputDir,     "-I",  "--input",          true  },
    { kOptCompileToBC,  "-b",  NULL,               false },
    { kOptCompileToIR,  "-s",  NULL,               false },
    { kOptCompile,      "-c",  NULL,               false },
    { kOptOptimizeMore, "-O",  NULL,               false },
    { kOptOptimizeMore, "-O1", NULL,               false },
    { kOptOptimizeNone, "-On", NULL,               false },
#if defined(UNITTESTS)
    { kOptRunUnitTests, "-UT", "--run-unit-tests", false },
    { kOptUTFormat,     NULL,  "--ut-format",      true },
    { kOptDontImport,   NULL,  "--dont-import",    false },
    { kOptParse1,       NULL,  "--parse-1",        false },
#endif
    { 0,                NULL,  NULL,               false } // sentinel
  };

  String outputFile;

  CompileFunction func = kDisplayHelp;
  std::vector<String> files;
  OptionsParser::ArgumentType type;
  OptionsParser::Option option;
  OptionsParser optp(heatherOptions, argc, (const char**)argv);

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
        Properties::setCompileOutFormat(kNativeObject);
        break;
      case kOptCompileToBC:
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
        Properties::test_setPass1Only(true);
        break;
#endif
      }
      break;

    case OptionsParser::kUnknownOption:
      fprintf(stderr, "ERROR: Unknown option: %s\n", (const char*)StrHelper(option.fOption));
      break;

    case OptionsParser::kMissingArgument:
      fprintf(stderr, "ERROR: Missing value for option: %s\n", (const char*)StrHelper(option.fOption));
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
    for (std::vector<String>::iterator it = files.begin(), e = files.end();
         it != e;
         it++)
    {
      compileFile(*it, true, false, false, outputFile);
    }
    break;

  case kCompileFiles:
    if (!outputFile.isEmpty() && files.size() > 1)
      logf(kError, "Outputfile and multiple compile files are given.");
    for (std::vector<String>::iterator it = files.begin(), e = files.end();
         it != e;
         it++)
    {
      compileFile(*it, true, true, false, outputFile);
    }
    break;
  }

  return 0;
}


#if defined(UNITTESTS)
static int
runUnitTestsWithRunner(UnitTest::TestRunner& runner)
{
    return runner.RunTestsIf(UnitTest::Test::GetTestList(), NULL, UnitTest::True(), 0);
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


