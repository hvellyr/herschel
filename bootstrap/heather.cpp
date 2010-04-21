#include <stdio.h>
#include <stdlib.h>

#include <vector>

#include "common.h"
#include "str.h"
#include "option.h"
#include "properties.h"

#define VERSION "0.0.1"
#define COPYRIGHT "2010"

using namespace heather;


static void
displayVersion()
{
  printf("%s %s - heather compiler\n", "heather", VERSION);
  printf("Copyright (c) %s Gregor Klinke\n", COPYRIGHT);
}


static void
displayHelp()
{
  displayVersion();
  printf("\n");
  printf("Usage: heather [options] file...\n");
  printf("Options:\n");
  printf("  --help            Display this information\n");
  printf("  --version         Display the version\n");
  printf("  --verbose         Be verbose\n");
  printf("  --outdir=DIR      Output all generated files to DIR\n");
}


int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine heatherOptions[] = {
    { 1, "-h", "--help",    false },
    { 2, "-v", "--version", false },
    { 3, "-d", "--outdir",  true  },
    { 4, NULL, "--verbose", false },
    { NULL }                    // sentinel
  };

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
      }
      break;

    case OptionsParser::kUnknownOption:
      {
        char tmp[256];
        option.fOption.toUtf8(tmp, 256);

        fprintf(stderr, "ERROR: Unknown option: %s\n", tmp);
      }
      break;

    case OptionsParser::kMissingArgument:
      {
        char tmp[256];
        option.fOption.toUtf8(tmp, 256);

        fprintf(stderr, "ERROR: Missing value for option: %s\n", tmp);
      }
      break;

    case OptionsParser::kNotAnOption:
      files.push_back(option.fArgument);
      break;

    default: ;
    }
  }

  return 0;
}
