/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.h"

namespace herschel
{
  class String;

  //! Returns the file extension of output files depending on the \p format.
  //! The extension does not contain the separator dot.
  String makeCompileOutputFileExt(CompileOutFormat format);

  //! Returns the path depending for output files.  If \p outfileName is not
  //! empty returns \p outfileName without any change.  If \p outdir is not
  //! empty returns a new path for a file named after the filename of \p file
  //! with the extension replaced with \p outExt in the folder \p outdir.
  //! Otherwise returns \p file with the extension replaced with \p outExt.
  //!
  //! For example
  //!
  //! <pre>
  //! outdir='', outfileName='', file='~/dev/abc.hr', outExt='bc'
  //! => '~/dev/abc.bc'
  //!
  //! outdir='', outfileName='~/tmp/build/abc.bc', file='~/dev/abc.hr', outExt='ll'
  //! => '~/tmp/build/abc.bc'
  //!
  //! outdir='~/dev/_build', outfileName='', file='~/dev/abc.hr', outExt='bc'
  //! => '~/dev/_build/abc.bc'
  //! </pre>
  String makeOutputFileName(const String& outdir,
                            const String& outfileName,
                            const String& file,
                            const String& outExt);

} // namespace

