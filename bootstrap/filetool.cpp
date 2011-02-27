/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "filetool.h"
#include "file.h"
#include "str.h"
#include "common.h"


using namespace herschel;


String
herschel::makeCompileOutputFileExt(CompileOutFormat format)
{
  switch (format) {
  case kLLVM_IR:
    return String("ll");
  case kLLVM_BC:
    return String("bc");
  }

  hr_invalid("invalid compile out format");
  return String();
}


String
herschel::makeOutputFileName(const String& outdir, const String& outfileName,
                   const String& file,
                   const String& outExt)
{
  if (!outfileName.isEmpty())
    return outfileName;

  if (!outdir.isEmpty())
    return file::append(outdir,
                        file::appendExt(file::baseName(file::namePart(file)),
                                        outExt));

  return file::appendExt(file::baseName(file), outExt);
}
