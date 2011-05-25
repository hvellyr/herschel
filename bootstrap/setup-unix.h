/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_setup_unix_h
#define bootstrap_setup_unix_h

#include "str.h"
#include "file.h"
#include "log.h"
#include "setup.h"

namespace herschel
{
  class SetupUnix
  {
  public:
    virtual ~SetupUnix() { }

    Setup findSysResources(const char* exeName) const;

  protected:
    virtual String getExeLocation() const = 0;

  private:
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

    bool exeFromRuntimeInstallation(const char* exeName,
                                    const String& exedir,
                                    Paths& paths) const;
    bool exeFromDevpath(const char* exeName,
                        const String& exedir, Paths& paths) const;
  };

  extern const SetupUnix& getSetupUnixSetup();
};                              // namespace

#endif                          // bootstrap_setup_unix_h
