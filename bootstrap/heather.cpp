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
  printf("Copyright (c) %s Gregor Klinke\n", COPYRIGHTYEAR);
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
  printf("  --help            Display this information\n");
  printf("  --version         Display the version\n");
  printf("  --verbose         Be verbose\n");
  printf("  --trace=KEYS      Trace various aspects: {tokenizer|pass1|pass2}\n");
  printf("  --outdir=DIR      Output all generated files to DIR\n");
#if defined(UNITTESTS)
  printf("  --run-unit-tests  Run unit tests for the compiler\n");
#endif
  printf("  --parse           Only parse the source files\n");
}


enum CompileFunction {
  kDisplayHelp,
#if defined(UNITTESTS)
  kRunUnitTests,
#endif
  kParseFiles,
};

int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine heatherOptions[] = {
    { 1, "-h",  "--help",           false },
    { 2, "-v",  "--version",        false },
    { 3, "-d",  "--outdir",         true  },
    { 4, NULL,  "--verbose",        false },
    { 5, "-T",  "--trace",          true  },
#if defined(UNITTESTS)
    { 6, "-UT", "--run-unit-tests", false },
#endif
    { 7, "-P",  "--parse",          false },
    { NULL }                    // sentinel
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
      case 1:                   // help
        displayHelp();
        exit(0);
        break;

      case 2:                   // version
        displayVersion();
        exit(0);
        break;

      case 3:                   // outdir
        Properties::setOutdir(option.fArgument);
        break;

      case 4:                   // verbose
        Properties::setIsVerbose(true);
        break;

      case 5:                   // trace
        Properties::setTraces(option.fArgument);
        break;

#if defined(UNITTESTS)
      case 6:                   // unittests
        func = kRunUnitTests;
        break;
#endif
      case 7:                   // parse
        func = kParseFiles;
        break;
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
        Ptr<AptNode> apt = parser->parse(new CharPort(new FilePort(*it, "rb")));
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
