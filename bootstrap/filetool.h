/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_filetool_h
#define bootstrap_filetool_h

#include "common.h"

namespace herschel
{
  class String;

  String makeCompileOutputFileExt(CompileOutFormat format);
  String makeOutputFileName(const String& outdir,
                            const String& outfileName,
                            const String& file,
                            const String& outExt);

};                              // namespace

#endif                          // bootstrap_filetool_h
