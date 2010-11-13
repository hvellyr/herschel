/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#include "str.h"
#include "file.h"
#include "compiler.h"
#include "apt.h"
#include "ptr.h"
#include "log.h"
#include "properties.h"
#include "codegen.h"


//----------------------------------------------------------------------------

using namespace heather;

namespace heather
{

static String
makeCompileOutputFileExt()
{
  switch (Properties::compileOutFormat()) {
  case kNativeObject:
    return String("o");
  case kLLVM_IR:
    return String("ll");
  case kLLVM_BC:
    return String("bc");
  }
  assert(0);
  return String();
}


static String
makeOutputFileName(const String& outdir, const String& outfileName,
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


void
compileFile(const String& file, bool doParse, bool doCompile, bool doLink,
                     const String& outfileName)
{
  try {
    if (doParse) {
      Ptr<Compiler> compiler = new Compiler;
      Ptr<AptNode> apt = compiler->process(new CharPort(
                                             new FilePort(file, "rb")),
                                           file);
      if (doCompile) {
        assert(apt);
        CompileUnitNode* unit = dynamic_cast<CompileUnitNode*>(apt.obj());
        assert(unit != NULL);

        if (unit != NULL) {
          String outExt = makeCompileOutputFileExt();
          String outFile = makeOutputFileName(Properties::outdir(),
                                              outfileName, file, outExt);

          Ptr<CodeGenerator> codegen = new CodeGenerator();
          codegen->compileToCode(unit, outFile);
        }

        if (doLink) {
          // TODO
        }
      }
    }
  }
  catch (const Exception& e) {
    logf(kError, "compilation of '%s' failed: %s",
         (const char*)StrHelper(file),
         (const char*)StrHelper(e.message()));
  }
}



//----------------------------------------------------------------------------

void
parseFiles(const std::vector<String>& files, const String& outputFile)
{
  for (std::vector<String>::const_iterator it = files.begin(), e = files.end();
       it != e;
       it++)
  {
    compileFile(*it, true, false, false, outputFile);
  }
}


//----------------------------------------------------------------------------

void
compileFiles(const std::vector<String>& files, const String& outputFile)
{
  if (!outputFile.isEmpty() && files.size() > 1)
    logf(kError, "Outputfile and multiple compile files are given.");

  for (std::vector<String>::const_iterator it = files.begin(), e = files.end();
       it != e;
       it++)
  {
    compileFile(*it, true, true, false, outputFile);
  }
}

};                              // namespace
