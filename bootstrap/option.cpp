/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include <assert.h>
#include <stdio.h>

#include "option.h"

using namespace heather;

//----------------------------------------------------------------------------

OptionsParser::OptionsParser(const OptionsDefine optDefs[],
                             int argc,
                             const char* argv[])
  : fOptionDefines(optDefs),
    fArgc(argc),
    fArguments(argv),
    fArgp(0)
{
  assert(fOptionDefines);

  // start from 1 to ignore the app name itself
  fArgp++;
}


OptionsParser::OptionType
OptionsParser::determineOptionType(const String& arg) const
{
  if (arg.startsWith(String("--")))
    return kLongForm;
  else if (arg.startsWith(String("-")))
    return kShortForm;

  return kNoOption;
}


const OptionsParser::OptionsDefine*
OptionsParser::findOption(const String& option,
                          bool compareShortForm) const
{
  for (const OptionsDefine* it = &fOptionDefines[0]; it->fId != 0; it++) {
    if (compareShortForm) {
      if (it->fShortForm != NULL && String(it->fShortForm) == option)
        return it;
    }
    else if (it->fLongForm != NULL && String(it->fLongForm) == option)
      return it;
  }

  return NULL;
}


OptionsParser::ArgumentType
OptionsParser::nextOption(Option* option)
{
  assert(option);

  option->fId = -1;
  option->fOption = String();
  option->fArgument = String();

  if (fArgp >= fArgc)
  {
    return kNoMoreArgs;
  }

  String arg(fArguments[fArgp]);

  OptionType optType = determineOptionType(arg);

  fArgp++;

  const OptionsDefine* optDefine = NULL;
  if (optType == kShortForm) {
    if ((optDefine = findOption(arg, true)) != NULL) {
      option->fId = optDefine->fId;
      option->fOption = arg;

      if (optDefine->fExpectsArgument) {
        if (fArgp < fArgc) {
          String nextArg(fArguments[fArgp]);
          if (determineOptionType(nextArg) == kNoOption) {
            option->fArgument = nextArg;
            fArgp++;

            return kOption;
          }
          return kMissingArgument;
        }
        return kMissingArgument;
      }

      return kOption;
    }
    return kUnknownOption;
  }
  else if (optType == kLongForm) {
    String key;
    String value;

    if (arg.split('=', key, value) >= 0) {
      option->fOption = key;
      option->fArgument = value;

      if ((optDefine = findOption(key, false)) != NULL) {
        option->fId = optDefine->fId;
        return kOption;
      }
      return kUnknownOption;
    }

    option->fOption = arg;
    if ((optDefine = findOption(arg, false)) != NULL) {
      if (optDefine->fExpectsArgument)
        return kMissingArgument;

      option->fId = optDefine->fId;
      return kOption;
    }
    return kUnknownOption;
  }

  option->fArgument = arg;
  return kNotAnOption;
}



//----------------------------------------------------------------------------

class OptionsParserUnitTest
{
public:
  OptionsParserUnitTest()
  {
    fprintf(stderr, "Run options parser tests ...\n");

    static const OptionsParser::OptionsDefine options[] = {
      { 1, "-h", "--help",    false },
      { 2, "-v", "--version", false },
      { 3, "-d", "--outdir",  true },
      { 4, NULL, "--verbose", false },
      { 5, "-U", NULL,        false },
      { 6, NULL, "--level",   true },
      { 7, "-P", NULL,        true },
      { NULL }                    // sentinel
    };

    OptionsParser::Option option;

    {
      static const char* args[] = { "heather", "-h" };

      OptionsParser p(options, 2, args);
      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 1);
      assert(option.fOption == String("-h"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }

    {
      static const char* args[] = { "heather",
                                    "-d", "tmp/test.log",
                                    "--level=5",
                                    "--verbose",
                                    "-P", "8080",
                                    "-U",
                                    "abc.hea", "xyz.hea" };

      OptionsParser p(options, 10, args);
      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 3);
      assert(option.fOption == String("-d"));
      assert(option.fArgument == String("tmp/test.log"));

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 6);
      assert(option.fOption == String("--level"));
      assert(option.fArgument == String("5"));

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 4);
      assert(option.fOption == String("--verbose"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 7);
      assert(option.fOption == String("-P"));
      assert(option.fArgument == String("8080"));

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 5);
      assert(option.fOption == String("-U"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kNotAnOption);
      assert(option.fOption.isEmpty());
      assert(option.fArgument == String("abc.hea"));

      assert(p.nextOption(&option) == OptionsParser::kNotAnOption);
      assert(option.fOption.isEmpty());
      assert(option.fArgument == String("xyz.hea"));

      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }

    {
      static const char* args[] = { "heather",
                                     "--outdir=tmp/test.log" };
      OptionsParser p(options, 2, args);
      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 3);
      assert(option.fOption == String("--outdir"));
      assert(option.fArgument == String("tmp/test.log"));

      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }

    {
      static const char* args[] = { "heather", "-d", "-U" };
      OptionsParser p(options, 3, args);
      assert(p.nextOption(&option) == OptionsParser::kMissingArgument);
      assert(option.fId == 3);        // return the proper option id anyway
      assert(option.fOption == String("-d"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 5);        // return the proper option id anyway
      assert(option.fOption == String("-U"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }

    {
      static const char* args[] = { "heather",
                                    "--version", "Something", "-U" };
      OptionsParser p(options, 4, args);
      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 2);
      assert(option.fOption == String("--version"));
      assert(option.fArgument.isEmpty());
      
      assert(p.nextOption(&option) == OptionsParser::kNotAnOption);
      assert(option.fOption.isEmpty());
      assert(option.fArgument == String("Something"));
      
      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 5);
      assert(option.fOption == String("-U"));
      assert(option.fArgument.isEmpty());
  
      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }

    {
      static const char* args[] = { "heather",
                                    "--port=7111", "--host=www.eyestep.org",
                                    "--output",
                                    "--level=DEBUG" };
      OptionsParser p(options, 5, args);
      assert(p.nextOption(&option) == OptionsParser::kUnknownOption);
      assert(option.fId == -1);
      assert(option.fOption == String("--port"));
      assert(option.fArgument == String("7111"));

      assert(p.nextOption(&option) == OptionsParser::kUnknownOption);
      assert(option.fId == -1);
      assert(option.fOption == String("--host"));
      assert(option.fArgument == String("www.eyestep.org"));

      assert(p.nextOption(&option) == OptionsParser::kUnknownOption);
      assert(option.fId == -1);
      assert(option.fOption == String("--output"));
      assert(option.fArgument.isEmpty());

      assert(p.nextOption(&option) == OptionsParser::kOption);
      assert(option.fId == 6);
      assert(option.fOption == String("--level"));
      assert(option.fArgument == String("DEBUG"));

      assert(p.nextOption(&option) == OptionsParser::kNoMoreArgs);
    }
  }
};

static OptionsParserUnitTest unitTest;

//****************************************************************************************
//                                     E O F
//****************************************************************************************
