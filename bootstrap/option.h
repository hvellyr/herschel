/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_option_h
#define bootstrap_option_h

#include "common.h"
#include "str.h"


namespace herschel
{

class OptionsParser
{
public:
  enum ArgumentType
  {
    kNoMoreArgs,
    kOption,
    kNotAnOption,
    kUnknownOption,
    kMissingArgument
  };

  enum OptionType
  {
    kNoOption,
    kShortForm,
    kLongForm
  };

  struct OptionsDefine
  {
    int         fId;
    const char* fShortForm;
    const char* fLongForm;
    bool        fExpectsArgument;
  };

  struct Option
  {
    int    fId;
    String fOption;
    String fArgument;
  };


  OptionsParser(
    const OptionsDefine optDefs[],
    int argc,
    const char* argv[]);

  ArgumentType nextOption(Option* option);

private:
  OptionType determineOptionType(const String& arg) const;

  const OptionsDefine* findOption(
    const String& option,
    bool compareShortForm) const;

  //-------- data members

  const OptionsDefine* fOptionDefines;
  const int            fArgc;
  const char**         fArguments;
  int                  fArgp;
};

};

#endif  // bootstrap_option_h
