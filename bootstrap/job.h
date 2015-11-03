/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include <vector>

namespace herschel
{
  class String;

  int startProcess(const String& cmd, const std::vector<String>& args,
                   bool logCalls = false);

} // namespace

