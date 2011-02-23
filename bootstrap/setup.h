/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_setup_h
#define bootstrap_setup_h

#include <vector>
#include "str.h"

namespace herschel
{
  typedef std::vector<String> StringVector;

  class Setup
  {
  public:
    //! system lookup path
    StringVector fSysPath;

    //! path to hrc executable
    String fHrcPath;

    //! path to assembler executable
    String fAsPath;
    //! additional flags to the assembler
    String fAsFlags;

    //! path to linker executable
    String fLdPath;
    //! additional flags to the linker
    StringVector fLdFlags;

    //! path to runtime library
    String fRuntimeLib;
  };

  Setup findResources(const char* exeName);
};                              // namespace

#endif                          // bootstrap_setup_h
