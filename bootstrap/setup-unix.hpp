/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "setup.hpp"

#include "file.hpp"
#include "log.hpp"
#include "str.hpp"


namespace herschel {

class SetupUnix {
public:
  virtual ~SetupUnix() {}

  Setup findSysResources(zstring exeName) const;

protected:
  //! Returns the directory where the current executable is located
  virtual String getExeLocation() const = 0;

private:
  struct Paths {
    // path to where additional tools are installed (llvm-ld, hrc, etc.)
    String fLibExec;
    String fLlvmLlcPath;
    String fLdPath;

    // hr header path
    String fHrIncludes;

    // runtime library path
    String fRtLib;
  };

  bool exeFromRuntimeInstallation(zstring exeName, const String& exedir,
                                  Paths& paths) const;
  bool exeFromDevpath(zstring exeName, const String& exedir, Paths& paths) const;
};

const SetupUnix& getSetupUnixSetup();

}  // namespace herschel
