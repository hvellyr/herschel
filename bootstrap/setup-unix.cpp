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


using namespace herschel;

namespace herschel
{
  #define MAX_LOOP_LEVEL 10

  static String
  getExeLocation()
  {
    char path[PATH_MAX];
    char path2[PATH_MAX];
    char *line, *result, *abspath;
    size_t buf_size;
    FILE *stream = NULL;
    int loop_level = 0;
    String retv;

    /* Read from /proc/self/exe (symlink) */
    buf_size = PATH_MAX - 1;

    strncpy(path2, "/proc/self/exe", buf_size - 1);

    while (true) {
      int i;
      ssize_t size;
      struct stat stat_buf;

      size = readlink(path2, path, buf_size - 1);
      if (size == -1)
        break;

      /* readlink() does not set a terminating 0 */
      path[size] = '\0';

      /* when symlink's target is a symlink too, continue searching. */
      i = stat(path, &stat_buf);
      if (i == -1)
        break;

      if (!S_ISLNK(stat_buf.st_mode)) {
        /* path is not a symlink. Done. */
        return String(path);
      }

      /* break cyclic symlinks */
      if (loop_level > MAX_LOOP_LEVEL)
        break;

      /* path is a symlink. Continue loop and resolve this. */
      strncpy(path, path2, buf_size - 1);
    }

    /* readlink() or stat() failed; this can happen when the program is
     * running in Valgrind 2.2. Read from /proc/self/maps as fallback. */

    buf_size = PATH_MAX + 128;
    line = (char*)malloc(buf_size);
    strcpy(line, path);

    if (!(stream = fopen("/proc/self/maps", "r")))
      goto errhd;

    /* The first entry should be the executable name. */
    if (!(result = fgets(line, (int)buf_size, stream)))
      goto errhd;

    /* Get rid of newline characters */
    buf_size = strlen(line);
    if (buf_size <= 0) {
      /* Huh? An empty string? */
      goto errhd;
    }
    if (line[buf_size - 1] == 10)
      line[buf_size - 1] = 0;

    /* Extract the filename; it is always an absolute path. */
    abspath = strchr(line, '/');

    /* Sanity check. */
    if (strstr(line, " r-xp ") == NULL || !abspath)
      goto errhd;

    retv = String(abspath);

  errhd:
    free(line);
    if (stream)
      fclose(stream);

    return file::canonicalPathName(retv);
  }


  struct Paths
  {
    // path to where additional tools are installed (llvm-ld, hrc, etc.)
    String fLibExec;
    String fLlvmLdPath;

    // h7 header path
    String fH7Includes;

    // runtime library path
    String fRtLib;
  };

  static bool
  exeFromDevpath(const char* exeName, const String& exedir, Paths& paths)
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
        paths.fH7Includes = file::appendDir(file::makeDir(basepath),
                                            String("lib"));
        paths.fRtLib = paths.fLibExec;

        paths.fLlvmLdPath = file::appendFile(file::makeDir(String(HR_LLVM_EXE)),
                                             String("llvm-ld"));
        return true;
      }
    }

    return false;
  }


  static bool
  exeFromRuntimeInstallation(const char* exeName, const String& exedir,
                             Paths& paths)
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
          paths.fH7Includes = file::appendDir(file::makeDir(basePkgPath),
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
  findSysResources(const char* exeName)
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
        setup.fSysPath.push_back(paths.fH7Includes);
        setup.fHrcPath = file::appendFile(file::makeDir(paths.fLibExec),
                                          String("hrc"));
        setup.fLdPath = paths.fLlvmLdPath;
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

        setup.fRuntimeLib = file::appendFile(file::makeDir(basePkgPath),
                                             String("libhr.a"));
      }

      setup.fLdFlags.push_back(String("-native"));
    }

    return setup;
  }
};                              // namespace
