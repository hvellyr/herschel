/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "Carbon/Carbon.h"

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

namespace herschel
{
  class SetupMac : public SetupUnix
  {
  public:
    virtual String getExeLocation() const
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
  };


  const SetupUnix&
  getSetupUnixSetup()
  {
    static SetupMac setup;
    return setup;
  }
}


