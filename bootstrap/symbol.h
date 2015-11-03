/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

namespace herschel
{
  class String;

  String qualifyId(const String& ns, const String& name);
  bool isQualified(const String& sym);

  String baseName(const String& sym);
  String nsName(const String& sym);

  String mangleToC(const String& qualId);
};                              // namespace

