/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "setup-unix.hpp"

#include "file.hpp"
#include "log.hpp"
#include "setup.hpp"
#include "str.hpp"

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


namespace herschel {
#define MAX_LOOP_LEVEL 10

class SetupLinux : public SetupUnix {
public:
  String getExeLocation() const override
  {
    char path[PATH_MAX];
    char path2[PATH_MAX];
    char *line, *result, *abspath;
    size_t buf_size;
    FILE *stream = nullptr;
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
    line = (char *)malloc(buf_size);
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
    if (strstr(line, " r-xp ") == nullptr || !abspath)
      goto errhd;

    retv = String(abspath);

  errhd:
    free(line);
    if (stream)
      fclose(stream);

    return file::canonicalPathName(retv).dirPart();
  }
};

const SetupUnix &getSetupUnixSetup()
{
  static SetupLinux setup;
  return setup;
}

}  // namespace herschel
