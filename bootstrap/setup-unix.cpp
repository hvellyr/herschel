/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include <stdlib.h>
#include <string.h>
#if defined(HAVE_LIMITS_H)
#include <limits.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>

#include "str.h"
#include "file.h"
#include "log.h"
#include "setup.h"
#include "setup-unix.h"


using namespace herschel;

bool
SetupUnix::exeFromDevpath(const char* exeName, const String& exedir,
                          Paths& paths) const
{
  static const char* possible_paths[] = {
    "/temp/debug/",
    "/temp/release/",
    NULL
  };
  const char** p = possible_paths;

  for ( ; *p; p++) {
    String fullpath = String(*p) + exeName;

    if (exedir.endsWith(fullpath)) {
      String basepath = exedir.part(0, exedir.length() - fullpath.length() + 1);

      paths.fLibExec = exedir.part(0, exedir.length() - strlen(exeName));
      paths.fHrIncludes = file::appendDir(file::makeDir(basepath),
                                          String("lib"));
      paths.fRtLib = paths.fLibExec;

      paths.fLlvmLdPath = file::appendFile(file::makeDir(String(HR_LLVM_EXE)),
                                           String("llvm-ld"));
      return true;
    }
  }

  return false;
}


bool
SetupUnix::exeFromRuntimeInstallation(const char* exeName,
                                      const String& exedir,
                                      Paths& paths) const
{
  static const char* possible_paths[] = {
    "/bin/",
    "/sbin/",
    NULL
  };
  const char** p = possible_paths;

  for ( ; *p; p++) {
    String fullpath = String(*p) + exeName;
    if (exedir.endsWith(fullpath)) {
      String basepath = exedir.part(0, exedir.length() - fullpath.length() + 1);

      String basePkgPath = file::appendDir(file::makeDir(basepath),
                                           String("lib/herschel"),
                                           String(VERSION));
      // only accept this as installation path, if $(prefix)/bin matches
      // a $(prefix)/lib/herschel/x.y.z folder
      if (file::isDir(basePkgPath)) {
        paths.fLibExec = basePkgPath;
        paths.fHrIncludes = file::appendDir(file::makeDir(basePkgPath),
                                            String("include"));
        paths.fRtLib = basePkgPath;

        paths.fLlvmLdPath = file::appendFile(file::makeDir(basePkgPath),
                                             String("llvm-ld"));
        return true;
      }
    }
  }

  return false;
}


Setup
SetupUnix::findSysResources(const char* exeName) const
{
  Setup setup;

  String syspath;
  String binpath;
  StringVector result;

  String exedir = getExeLocation();
  if (!exedir.isEmpty()) {
    Paths paths;

    /* check whether we run from build environment */
    if (exeFromDevpath(exeName, exedir, paths) ||
        exeFromRuntimeInstallation(exeName, exedir, paths))
    {
      setup.fSysPath.push_back(paths.fHrIncludes);
      setup.fHrcPath = file::appendFile(file::makeDir(paths.fLibExec),
                                        String("hrc"));
      setup.fLdPath = paths.fLlvmLdPath;
      setup.fLangKit = file::appendFile(file::makeDir(paths.fRtLib),
                                        String("langkit.hlib"));
      setup.fRuntimeLib = file::appendFile(file::makeDir(paths.fRtLib),
                                           String("libhr.a"));
    }
    else {
      String basePkgPath = file::appendDir(file::makeDir(String(HR_INSTDIR_pkglibdir)),
                                           String(VERSION));

      setup.fSysPath.push_back(file::appendDir(file::makeDir(basePkgPath),
                                               String("include")));
      setup.fHrcPath = file::appendFile(file::makeDir(basePkgPath),
                                        String("hrc"));
      setup.fLdPath = file::appendFile(file::makeDir(basePkgPath),
                                       String("llvm-ld"));

      setup.fLangKit = file::appendFile(file::makeDir(basePkgPath),
                                        String("langkit.hlib"));
      setup.fRuntimeLib = file::appendFile(file::makeDir(basePkgPath),
                                           String("libhr.a"));
    }

    setup.fLdFlags.push_back(String("-native"));
  }

  return setup;
}


Setup
herschel::findResources(const char* exeName)
{
  return getSetupUnixSetup().findSysResources(exeName);
}
