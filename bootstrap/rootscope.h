/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_rootscope_h
#define bootstrap_rootscope_h

namespace heather
{
  class Scope;

  namespace type
  {
    Scope* newRootScope(bool forUnitTests = false);
  };
};                              // namespace

#endif                          // bootstrap_rootscope_h
