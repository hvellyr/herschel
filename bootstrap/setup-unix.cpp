/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include "str.h"
#include "file.h"
#include "log.h"
#include "setup.h"
#include "setup-unix.h"

#include <stdlib.h>
#include <string.h>
#if defined(HAVE_LIMITS_H)
#include <limits.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <vector>


using namespace herschel;

bool
SetupUnix::exeFromDevpath(zstring /*exeName*/, const String& exedir,
                          Paths& paths) const
{
  static std::vector<String> possible_paths = {
    String("/temp/debug/"),
    String("/temp/release/"),
  };

  for (const auto& subpath : possible_paths) {
    if (exedir.endsWith(subpath)) {
      String basepath = exedir.part(0, exedir.length() - subpath.length() + 1);

      paths.fLibExec = exedir;
      paths.fHrIncludes = file::appendDir(file::makeDir(basepath),
                                          String("lib"));
      paths.fRtLib = paths.fLibExec;

      paths.fLlvmLlcPath = file::appendFile(file::makeDir(String(HR_LLVM_EXE)),
                                            String("llc"));
      paths.fLdPath = String("cc");
      return true;
    }
  }

  return false;
}


bool
SetupUnix::exeFromRuntimeInstallation(zstring exeName, const String& exedir,
                                      Paths& paths) const
{
  static std::vector<String> possible_paths = {
    String("/bin/"),
    String("/sbin/"),
  };
  for (const auto& subpath : possible_paths) {
    if (exedir.endsWith(subpath)) {
      String basepath = exedir.part(0, exedir.length() - subpath.length() + 1);

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

        paths.fLlvmLlcPath = file::appendFile(file::makeDir(basePkgPath),
                                              String("llc"));
        paths.fLdPath = String("cc");
        return true;
      }
    }
  }

  return false;
}


Setup
SetupUnix::findSysResources(zstring exeName) const
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
      setup.fLlcPath = paths.fLlvmLlcPath;
      setup.fLdPath = paths.fLdPath;
      setup.fLangKit = file::appendFile(file::makeDir(paths.fRtLib),
                                        String("langkit.a"));
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
      setup.fLlcPath = file::appendFile(file::makeDir(basePkgPath),
                                       String("llc"));
      setup.fLdPath = String("cc");

      setup.fLangKit = file::appendFile(file::makeDir(basePkgPath),
                                        String("langkit.a"));
      setup.fRuntimeLib = file::appendFile(file::makeDir(basePkgPath),
                                           String("libhr.a"));
    }
  }

  return setup;
}


Setup
herschel::findResources(zstring exeName)
{
  return getSetupUnixSetup().findSysResources(exeName);
}
