/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_symbol_h
#define bootstrap_symbol_h

namespace herschel
{
  String qualifyId(const String& ns, const String& name);
  bool isQualified(const String& sym);

  String baseName(const String& sym);
  String nsName(const String& sym);

  String mangleToC(const String& qualId);
};                              // namespace

#endif                          // bootstrap_symbol_h
