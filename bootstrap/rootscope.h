/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_rootscope_h
#define bootstrap_rootscope_h

#include <memory>


namespace herschel
{
  class Scope;

  namespace type
  {
    std::shared_ptr<Scope> newRootScope(bool forUnitTests = false);
  };
};                              // namespace

#endif                          // bootstrap_rootscope_h
