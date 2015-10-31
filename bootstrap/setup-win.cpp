/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#if defined(OS_win)

#include <Windows.h>

#include "str.h"
#include "file.h"
#include "log.h"
#include "setup.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <vector>


using namespace herschel;


namespace herschel
{
  static String
  getExeLocation()
  {
    char buf[MAX_PATH];
    DWORD len = ::GetModuleFileName(nullptr, buf, sizeof(buf));
    hr_assert(len > 0 && "GetModuleFileName failed");

    return file::canonicalPathName(String(buf));
  }


  static bool
  exeFromDevpath(zstring exeName, const String& exedir,
                 String& syspath, String& binpath)
  {
    static std::vector<String> possible_paths = {
      String("/temp/debug/"),
      String("/temp/release/"),
    };
    for (const auto& p : possible_paths) {
      String fullpath = p + exeName + ".exe";
      if (exedir.endsWith(fullpath)) {
        binpath = exedir.part(0, exedir.length() - (strlen(exeName) + 4));
        syspath = exedir.part(0, exedir.length() - fullpath.length() + 1);
        return true;
      }
    }

    return false;
  }


  StringVector
  findSysResources(zstring exeName)
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
        setup.fLlcPath = file::appendFile(file::makeDir(syspath),
                                          String("external/llvm/llc"));
        setup.fLdPath = String("*linker*");
      }
      else {
        setup.fSysPath.push_back(file::appendDir(file::makeDir(String(HR_INSTDIR_pkglibdir)),
                                                 String(VERSION),
                                                 String("include")));
        setup.fHrcPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                          String("hrc"));
        setup.fLLcPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                          String("llc"));
        setup.fLdPath = String("*linker*");
      }
    }

    return setup;
  }
};                              // namespace

#endif // OS_win
