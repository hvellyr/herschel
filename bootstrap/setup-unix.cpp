/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#if defined(OS_mac)
#include "Carbon/Carbon.h"
#endif

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

#if defined(OS_mac)

  static String
  getExeLocation()
  {
    ProcessSerialNumber psn = { 0, kCurrentProcess };
    char buffer[2048];
    FSRef ref;
    OSErr err;

    if ((err = GetProcessBundleLocation(&psn, &ref)) != noErr) {
      logf(kError, "Mac error: %d\n", err);
      return String();
    }

    if ((err = FSRefMakePath(&ref, (UInt8*)buffer, 2048)) == noErr)
      return file::canonicalPathName(String(buffer));

    return String();
  }


#elif defined(OS_Linux)

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
    Char8* retv = NULL;

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
    line = malloc(buf_size);
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

    retv = strdup(abspath);

  errhd:
    free(line);
    if (stream)
      fclose(stream);

    return file::canonicalPathName(String(retv));
  }


#else

  static String
  getExeLocation()
  {
    return String();
  }

#endif


  static bool
  exeFromDevpath(const char* exeName, const String& exedir,
                 String& syspath, String& binpath)
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
        binpath = exedir.part(0, exedir.length() - strlen(exeName));
        syspath = exedir.part(0, exedir.length() - fullpath.length() + 1);
        return true;
      }
    }

    return false;
  }


#if defined(OS_mac) || defined(OS_linux)

  static bool
  exeFromRuntimeInstallation(const char* exeName, const String& exedir,
                             String& syspath, String& binpath)
  {
    static const char* possible_paths[] = {
      "/bin/",
      "/sbin/",
      NULL
    };
    const char** p = possible_paths;

    for ( ; *p; p++) {
      if (exedir.endsWith(String(*p) + exeName)) {
        String basepath = exedir.part(0, exedir.length() - strlen(*p) + 1);

        syspath = file::appendDir(file::makeDir(basepath),
                                  String("lib/herschel"), 
                                  String(VERSION),
                                  String("include"));
        return true;
      }
    }

    return false;
  }

#else

  static bool
  exeFromRuntimeInstallation(const char* exeName, const String& exedir,
                             String& syspath, String& binpath)
  {
    return false;
  }

#endif

  Setup
  findSysResources(const char* exeName)
  {
    Setup setup;

    String syspath;
    String binpath;
    StringVector result;

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
        setup.fRuntimeLib = file::appendFile(file::makeDir(binpath),
                                             String("libhr.a"));
      }
      else if (exeFromRuntimeInstallation(exeName, exedir, syspath, binpath)) {
        // TODO
        setup.fSysPath.push_back(syspath);
        setup.fHrcPath = file::appendFile(file::makeDir(binpath),
                                          String("hrc"));
        // setup.fAsPath = String();
        // setup.fAsFlags = String();
        // TODO: configurable?
        setup.fLdPath = String("llvm-ld");
        setup.fLdFlags.push_back(String("-native"));
        // setup.fRuntimeLib = file::appendFile(file::makeDir(binpath),
        //                                      String("libhrt.a"));
      }
      else {
        setup.fSysPath.push_back(file::appendDir(file::makeDir(String(HR_INSTDIR_pkglibdir)),
                                                 String(VERSION),
                                                 String("include")));
        setup.fHrcPath = file::appendFile(file::makeDir(String(HR_INSTDIR_bindir)),
                                          String("hrc"));
        // setup.fAsPath = String();
        // setup.fAsFlags = String();
        setup.fLdPath = String("llvm-ld");
        setup.fLdFlags.push_back(String("-native"));
      }
    }

    return setup;
  }


  String
  findSysBasePath(const char* exeName)
  {
    return file::makeDir(String(HR_INSTDIR_bindir));
  }
};                              // namespace
