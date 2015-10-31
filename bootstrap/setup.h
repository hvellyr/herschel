/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_setup_h
#define bootstrap_setup_h

#include <vector>
#include "str.h"

namespace herschel
{
  using StringVector = std::vector<String>;

  class Setup
  {
  public:
    //! system lookup path
    StringVector fSysPath;

    //! path to hrc executable
    String fHrcPath;

    //! path to the llc executable (from LLVM)
    String fLlcPath;

    //! path to the system linker executable
    String fLdPath;
    //! additional flags to the linker
    StringVector fLdFlags;

    //! path to the langkit library
    String fLangKit;

    //! path to runtime library
    String fRuntimeLib;
  };

  extern Setup findResources(zstring exeName);
};                              // namespace

#endif                          // bootstrap_setup_h
