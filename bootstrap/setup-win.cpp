/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#if defined(OS_win)

#include <Windows.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

#include "str.h"
#include "file.h"
#include "log.h"
#include "setup.h"


using namespace herschel;


namespace herschel
{
  static String
  getExeLocation()
  {
    char buf[MAX_PATH];
    DWORD len = ::GetModuleFileName(NULL, buf, sizeof(buf));
    hr_assert(len > 0 && "GetModuleFileName failed");

    return file::canonicalPathName(String(buf));
  }


  static bool
  exeFromDevpath(const char* exeName, const String& exedir,
                 String& syspath, String& binpath)
  {
    static char* possible_paths[] = {
      "/temp/debug/",
      "/temp/release/",
      NULL
    };
    char** p = possible_paths;

    for ( ; *p; p++) {
      String fullpath = String(*p) + exeName + ".exe";
      if (exedir.endsWith(fullpath)) {
        binpath = exedir.part(0, exedir.length() - (strlen(exeName) + 4));
        syspath = exedir.part(0, exedir.length() - fullpath.length() + 1);
        return true;
      }
    }

    return false;
  }


  StringVector
  findSysResources(const char* exeName)
  {
    Setup setup;
    String binspath;
    String syspath;

    String exedir = getExeLocation();
    if (!exedir.isEmpty()) {
      /* check whether we run from build environment */
      if (exeFromDevpath(exeName, exedir, syspath, binpath)) {
        setup.fSysPath.push_back(file::appendDir(file::makeDir(syspath),
                                                 String("lib")));
        setup.fHrcPath = file::appendFile(file::makeDir(binpath),
                                          String("hrc"));
        setup.fLdPath = file::appendFile(file::makeDir(syspath),
                                         String("external/llvm/llvm-ld"));
        setup.fLdFlags.push_back(String("-native"));
      }
      else {
        String fullExeName = String(exeName) + ".exe";
        String basepath = exedir.part(0, exedir.length() - fullExeName.length());
        setup.fSysPath.push_back(file::appendDir(file::makeDir(basepath),
                                                 String("lib")));
        setup.fHrcPath = file::appendFile(file::makeDir(basepath),
                                          String("hrc"));
        setup.fLdPath = file::appendFile(file::makeDir(basepath),
                                         String("llvm-ld"));
        setup.fLdFlags.push_back(String("-native"));
      }
      else {
        setup.fSysPath.push_back(file::appendDir(file::makeDir(String(HR_INSTDIR_pkglibdir)),
                                                 String(VERSION),
                                                 String("include")));
        setup.fHrcPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                          String("hrc"));
        // setup.fAsPath = String();
        // setup.fAsFlags = String();
        setup.fLdPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                         String("llvm-ld"));
        setup.fLdFlags.push_back(String("-native"));
      }
    }

    return setup;
  }
};                              // namespace

#endif // OS_win
