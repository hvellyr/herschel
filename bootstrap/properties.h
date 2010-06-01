/* -*-c++-*-

   This file is part of the heather package 

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_properties_h
#define bootstrap_properties_h

#include "parsertypes.h"


namespace heather
{
  class Token;
  class String;

  class Properties
  {
  public:
    static void setIsVerbose(bool value);
    static bool isVerbose();

    static void setOutdir(const String& outdir);
    static String outdir();

    static void setTrace(const String& key, bool value);
    static void setTraces(const String& argument);

    static bool isTraceTokenizer();
    static bool isTracePass1();
    static bool isTracePass2();
    static bool isTraceImportFile();

    static void setConfigVar(const String& keyValuePair);
    static ConfigVarRegistry* globalConfigVarRegistry();

#if defined(UNITTESTS)
    static void test_setDontImport(bool value);
    static bool test_dontImport();
#endif
  };
};                              // namespace

#endif bootstrap_properties_h
