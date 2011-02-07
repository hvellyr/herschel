/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "common.h"
#include "setup.h"
#include "properties.h"
#include "str.h"
#include "file.h"

#include <vector>

using namespace herschel;


/* ----------------------------------------------------------------------
   navigate in installation, find default resource files
   ---------------------------------------------------------------------- */
#if defined(OS_mac) || defined(OS_win) || defined(OS_linux)

namespace herschel
{
extern Setup findSysResources(const char* exeName);
};

#else

namespace herschel
{
  /* a generic implementation */
  Setup
  findSysResources(const char* exeName)
  {
    Setup setup;

#if defined(IS_DEBUG)
    /* check whether we run from the development folder */
    {
      String cwd = file::workingDir();
      String binpath;

      if (file::isFile(file::appendFile(cwd, String("temp/debug/") + exeName)))
        binpath = file::appendFile(cwd, String("temp/debug/"));
      else if (file::isFile(file::appendFile(cwd, String("temp/release/") + exeName)))
        binpath = file::appendFile(cwd, String("temp/release/"));

      if (!binpath.isEmpty()) {
        setup.fSysPath.push_back(file::appendDir(cwd, String("lib")));
        setup.fHrcPath = file::appendFile(binpath, String("hrc"));
        // setup.fAsPath = String();
        // setup.fAsFlags = String();
        setup.fLdPath = file::appendFile(cwd, String("external/llvm/Release/bin/llvm-ld"));
        setup.fLdFlags.push_back(String("-native"));
        return setup;
      }
    }
#endif

    setup.fSysPath.push_back(file::appendDir(
                               file::makeDir(String(HR_INSTDIR_pkglibdir)),
                               String(VERSION)),
                             String("include"));
    setup.fHrcPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                      String("hrc"));
    // setup.fAsPath = String();
    // setup.fAsFlags = String();
    setup.fLdPath = String("llvm-ld");
    setup.fLdFlags.push_back(String("-native"));

    return setup;
  }
};

#endif


Setup
herschel::findResources(const char* exeName)
{
  return findSysResources(exeName);
}
