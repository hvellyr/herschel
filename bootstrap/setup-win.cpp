/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
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


using namespace herschel;


namespace herschel
{
  static String
  getExeLocation()
  {
    char buf[MAX_PATH];
    DWORD len = ::GetModuleFileName(NULL, buf, sizeof(buf));
    assert(len > 0 && "GetModuleFileName failed");

    return file::canonicalPathName(String(buf));
  }


  #define APPFILE "hrc.exe"

  static bool
  exeFromDevpath(const String& exedir, String& syspath)
  {
    static char* possible_paths[] = {
      "/temp/debug/" APPFILE,
      "/temp/release/" APPFILE,
      NULL
    };
    char** p = possible_paths;

    for ( ; *p; p++) {
      if (exedir.endsWith(String(*p))) {
        String basepath = exedir.part(0, exedir.length() - strlen(*p) + 1);
        syspath = file::appendDir(file::makeDir(basepath), String("lib"));
        return true;
      }
    }

    return false;
  }


  StringVector
  findSysResourceBundle()
  {
    String syspath;
    StringVector result;

    String exedir = getExeLocation();
    if (!exedir.isEmpty()) {
      /* check whether we run from build environment */
      if (exeFromDevpath(exedir, syspath)) {
        result.push_back(syspath);
      }
      else {
        String basepath = exedir.part(0, exedir.length() - strlen(APPFILE));
        syspath = file::appendDir(file::makeDir(basepath), String("lib"));
        result.push_back(syspath);
      }
      else {
        syspath = file::appendDir(file::makeDir(String(HR_INSTDIR_pkglibdir)),
                                  String(VERSION),
                                  String("include"));
        result.push_back(syspath);
      }
    }

    return result;
  }

};                              // namespace

#endif // OS_win
