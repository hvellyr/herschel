/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_symbol_h
#define bootstrap_symbol_h

namespace heather
{
  String qualifiedId(const String& ns, const String& name);
  bool isQualified(const String& sym);

  String baseName(const String& sym);
  String nsName(const String& sym);

};                              // namespace

#endif                          // bootstrap_symbol_h
