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

#include <CoreFoundation/CoreFoundation.h>

#include <stdlib.h>
#include <string.h>
#if defined(HAVE_LIMITS_H)
#  include <limits.h>
#endif

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>


namespace herschel {
class SetupMac : public SetupUnix {
public:
  String getExeLocation() const override
  {
    String result;

    CFBundleRef bundleRef = CFBundleGetMainBundle();
    if (bundleRef) {
      CFURLRef urlRef = CFBundleCopyBundleURL(bundleRef);
      if (urlRef) {
        UInt8 bundlePath[PATH_MAX];
        if (CFURLGetFileSystemRepresentation(urlRef, true, bundlePath,
                                             sizeof(bundlePath))) {
          result = file::canonicalPathName(String((zstring)bundlePath) + "/");
        }
        CFRelease(urlRef);
      }
      CFRelease(bundleRef);
    }

    return result;
  }
};


const SetupUnix& getSetupUnixSetup()
{
  static SetupMac setup;
  return setup;
}

}  // namespace herschel
