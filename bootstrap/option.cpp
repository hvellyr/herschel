/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <stdio.h>

#include "require.h"
#include "option.h"


using namespace herschel;

//----------------------------------------------------------------------------

OptionsParser::OptionsParser(const OptionsDefine optDefs[],
                             int argc,
                             const char* argv[])
  : fOptionDefines(optDefs),
    fArgc(argc),
    fArguments(argv),
    fArgp(0)
{
  hr_assert(fOptionDefines);

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
  hr_assert(option);

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
    option->fOption = arg;

    if ((optDefine = findOption(arg, K(compareShortForm))) != NULL) {
      option->fId = optDefine->fId;

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

      if ((optDefine = findOption(key, !K(compareShortForm))) != NULL) {
        option->fId = optDefine->fId;
        return kOption;
      }
      return kUnknownOption;
    }

    option->fOption = arg;
    if ((optDefine = findOption(arg, !K(compareShortForm))) != NULL) {
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



#if defined(UNITTESTS)
//----------------------------------------------------------------------------

#include <UnitTest++.h>


SUITE(OptionsParser)
{
  static const OptionsParser::OptionsDefine options[] = {
    { 1, "-h", "--help",    !K(argument) },
    { 2, "-v", "--version", !K(argument) },
    { 3, "-d", "--outdir",   K(argument) },
    { 4, NULL, "--verbose", !K(argument) },
    { 5, "-U", NULL,        !K(argument) },
    { 6, NULL, "--level",    K(argument) },
    { 7, "-P", NULL,         K(argument) },
    { 0, NULL, NULL,        !K(argument) } // sentinel
  };

  OptionsParser::Option option;

  TEST(One)
  {
    static const char* args[] = { "herschel", "-h" };

    OptionsParser p(options, 2, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 1);
    CHECK_EQUAL(option.fOption, String("-h"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }

  TEST(Two)
  {
    static const char* args[] = { "herschel",
                                  "-d", "tmp/test.log",
                                  "--level=5",
                                  "--verbose",
                                  "-P", "8080",
                                  "-U",
                                  "abc.h7", "xyz.h7" };

    OptionsParser p(options, 10, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 3);
    CHECK_EQUAL(option.fOption, String("-d"));
    CHECK_EQUAL(option.fArgument, String("tmp/test.log"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 6);
    CHECK_EQUAL(option.fOption, String("--level"));
    CHECK_EQUAL(option.fArgument, String("5"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 4);
    CHECK_EQUAL(option.fOption, String("--verbose"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 7);
    CHECK_EQUAL(option.fOption, String("-P"));
    CHECK_EQUAL(option.fArgument, String("8080"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 5);
    CHECK_EQUAL(option.fOption, String("-U"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNotAnOption);
    CHECK(option.fOption.isEmpty());
    CHECK_EQUAL(option.fArgument, String("abc.h7"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNotAnOption);
    CHECK(option.fOption.isEmpty());
    CHECK_EQUAL(option.fArgument, String("xyz.h7"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }

  TEST(Three)
  {
    static const char* args[] = { "herschel",
                                  "--outdir=tmp/test.log" };
    OptionsParser p(options, 2, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 3);
    CHECK_EQUAL(option.fOption, String("--outdir"));
    CHECK_EQUAL(option.fArgument, String("tmp/test.log"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }

  TEST(Four)
  {
    static const char* args[] = { "herschel", "-d", "-U" };
    OptionsParser p(options, 3, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kMissingArgument);
    CHECK_EQUAL(option.fId, 3);        // return the proper option id anyway
    CHECK_EQUAL(option.fOption, String("-d"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 5);        // return the proper option id anyway
    CHECK_EQUAL(option.fOption, String("-U"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }

  TEST(Five)
  {
    static const char* args[] = { "herschel",
                                  "--version", "Something", "-U" };
    OptionsParser p(options, 4, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 2);
    CHECK_EQUAL(option.fOption, String("--version"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNotAnOption);
    CHECK(option.fOption.isEmpty());
    CHECK_EQUAL(option.fArgument, String("Something"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 5);
    CHECK_EQUAL(option.fOption, String("-U"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }

  TEST(Six)
  {
    static const char* args[] = { "herschel",
                                  "--port=7111", "--host=www.eyestep.org",
                                  "--output",
                                  "--level=DEBUG",
                                  "-XYZ" };
    OptionsParser p(options, 6, args);
    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kUnknownOption);
    CHECK_EQUAL(option.fId, -1);
    CHECK_EQUAL(option.fOption, String("--port"));
    CHECK_EQUAL(option.fArgument, String("7111"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kUnknownOption);
    CHECK_EQUAL(option.fId, -1);
    CHECK_EQUAL(option.fOption, String("--host"));
    CHECK_EQUAL(option.fArgument, String("www.eyestep.org"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kUnknownOption);
    CHECK_EQUAL(option.fId, -1);
    CHECK_EQUAL(option.fOption, String("--output"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kOption);
    CHECK_EQUAL(option.fId, 6);
    CHECK_EQUAL(option.fOption, String("--level"));
    CHECK_EQUAL(option.fArgument, String("DEBUG"));

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kUnknownOption);
    CHECK_EQUAL(option.fId, -1);
    CHECK_EQUAL(option.fOption, String("-XYZ"));
    CHECK(option.fArgument.isEmpty());

    CHECK_EQUAL(p.nextOption(&option), OptionsParser::kNoMoreArgs);
  }
}

#endif  // #if defined(UNITTESTS)
