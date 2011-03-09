/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_job_h
#define bootstrap_job_h

#include <vector>

namespace herschel
{
  class String;

  int startProcess(const String& cmd, const std::vector<String>& args);

};                              // namespace

#endif                          // bootstrap_job_h
