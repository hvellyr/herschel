/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_compile_h
#define bootstrap_compile_h

#include <vector>

namespace heather
{
  class String;

  void compileFile(const String& file,
                   bool doParse, bool doCompile, bool doLink,
                   const String& outfileName);

  void parseFiles(const std::vector<String>& files,
                  const String& outputfile);

  void compileFiles(const std::vector<String>& files,
                    const String& outputfile);

};                              // namespace

#endif                          // bootstrap_compile_h
