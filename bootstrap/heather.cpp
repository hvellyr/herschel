#include <stdio.h>
#include <stdlib.h>

#include <vector>

#include "common.h"
#include "str.h"
#include "option.h"
#include "properties.h"
#include "unittests.h"
#include "ptr.h"
#include "apt.h"
#include "parser.h"


using namespace heather;

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
  printf("  -h,      --help            Display this information\n");
  printf("  -v,      --version         Display the version\n");
  printf("           --verbose         Be verbose\n");
  printf("  -D VAR=VALUE               Define config VAR to be VALUE\n");
  printf("     --define=VAR=VALUE\n");
  printf("  -T KEYS, --trace=KEYS      Trace various aspects:\n");
  printf("                             {tokenizer|pass1|pass2|import|macro}\n");
  printf("  -d DIR,  --outdir=DIR      Output all generated files to DIR\n");
  printf("  -I DIR,  --input=DIR       Add DIR to the input searchlist\n");
#if defined(UNITTESTS)
  printf("  -UT,     --run-unit-tests  Run unit tests for the compiler\n");
  printf("           --parse-1         Only do pass1 phase\n");
#endif
  printf("  -P,      --parse           Only parse the source files\n");
}


enum CompileFunction {
  kDisplayHelp,
#if defined(UNITTESTS)
  kRunUnitTests,
#endif
  kParseFiles,
};

enum {
  kOptHelp = 1,
  kOptVersion,
  kOptOutdir,
  kOptVerbose,
  kOptTrace,
  kOptParse,
  kOptDefine,
  kOptInputDir,

#if defined(UNITTESTS)
  kOptRunUnitTests,
  kOptDontImport,
  kOptParse1,
#endif
};

int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine heatherOptions[] = {
    { kOptHelp,         "-h",  "--help",           false },
    { kOptVersion,      "-v",  "--version",        false },
    { kOptOutdir,       "-d",  "--outdir",         true  },
    { kOptVerbose,      NULL,  "--verbose",        false },
    { kOptTrace,        "-T",  "--trace",          true  },
    { kOptParse,        "-P",  "--parse",          false },
    { kOptDefine,       "-D",  "--define",         true  },
    { kOptInputDir,     "-I",  "--input",          true  },
#if defined(UNITTESTS)
    { kOptRunUnitTests, "-UT", "--run-unit-tests", false },
    { kOptDontImport,   NULL,  "--dont-import",    false },
    { kOptParse1,       NULL,  "--parse-1",        false },
#endif
    { 0,                NULL,  NULL,               false } // sentinel
  };

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

      case kOptVerbose:
        Properties::setIsVerbose(true);
        break;

      case kOptTrace:
        Properties::setTraces(option.fArgument);
        break;

      case kOptParse:
        func = kParseFiles;
        break;

      case kOptDefine:
        Properties::setConfigVar(option.fArgument);
        break;

      case kOptInputDir:
        Properties::addInputDir(option.fArgument);
        break;

#if defined(UNITTESTS)
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
    UnitTest::runUnitTests();
    break;
#endif

  case kParseFiles:
    for (std::vector<String>::iterator it = files.begin();
         it != files.end();
         it++)
    {
      try {
        Ptr<Parser> parser = new Parser;
        Ptr<AptNode> apt = parser->parse(new CharPort(new FilePort(*it, "rb")),
                                         *it);
      }
      catch (const Exception& e) {
        fprintf(stderr, "ERROR: compilation of '%s' failed: %s\n",
                (const char*)StrHelper(*it),
                (const char*)StrHelper(e.message()));
      }
    }
    break;
  }

  return 0;
}
