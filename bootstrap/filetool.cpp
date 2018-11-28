/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "filetool.hpp"

#include "common.hpp"
#include "file.hpp"
#include "str.hpp"


namespace herschel {


String makeCompileOutputFileExt(CompileOutFormat format)
{
  switch (format) {
  case kLLVM_IR: return String("ll");
  case kLLVM_BC: return String("bc");
  }

  hr_invalid("invalid compile out format");
  return String();
}


String makeOutputFileName(const String& outdir, const String& outfileName,
                          const String& file, const String& outExt)
{
  if (!outfileName.isEmpty())
    return outfileName;

  if (!outdir.isEmpty())
    return file::append(outdir,
                        file::appendExt(file::baseName(file::namePart(file)), outExt));

  return file::appendExt(file::baseName(file), outExt);
}

}  // namespace herschel
