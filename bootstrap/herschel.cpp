/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>

#include <vector>

#include "common.h"
#include "str.h"
#include "setup.h"
#include "option.h"
#include "log.h"
#include "job.h"
#include "filetool.h"

#include "llvm/Config/config.h"

using namespace herschel;

static void
displayVersion(bool verbose)
{
  printf("%s %s - herschel compiler\n", "herschel", VERSION);
  printf("Copyright (c) %s, %s\n", COPYRIGHTYEAR, COPYRIGHTOWNER);
  printf("(base revision: %s)\n", HR_BASE_REVISION);

  if (verbose) {
    printf("Using LLVM: %s (%s)\n", PACKAGE_STRING, LLVM_CONFIGTIME);
  }
}


static void
displayHelp()
{
  displayVersion(false);
  /*      123456789012345678901234567890123456789012345678901234567890123456789012*/
  /*               1         2         3         4         5         6         7  */
  printf("\n");
  printf("Usage: herschel [options] files...\n");
  printf("Options:\n");
  printf("  -h,      --help              Display this information\n");
  printf("  -v,      --version           Display the version\n");
  printf("           --verbose           Be verbose\n");
  printf("  -D VAR=VALUE                 Define config VAR to be VALUE\n");
  printf("     --define=VAR=VALUE\n");
  printf("  -T KEYS, --trace=KEYS        Trace various aspects:\n");
  printf("                               {tokenizer|pass1|pass2|annotate|\n");
  printf("                                transform|typify|import|macro|\n");
  printf("                                codedump}\n");
  printf("  -d DIR,  --outdir=DIR        Output all generated files to DIR\n");
  printf("  -I DIR,  --input=DIR         Add DIR to the input searchlist\n");
  printf("  -O                           Optimize code more\n");
  printf("  -On                          Turn off any (even basic) optimization\n");
  printf("  -P,      --parse             Only parse the source files\n");
  printf("  -c                           Only compile the source files, no link\n");
  printf("  -s                           Compile to LLVM IR, no link\n");
}


enum CompileFunction {
  kDisplayHelp,
  kDisplayVersion,
  kParseFiles,
  kCompileFiles,
  kCompileFilesToIR,
  kLinkAndCompileFiles,
};


enum {
  kOptHelp = 1,
  kOptVersion,
  kOptOutdir,
  kOptOutput,
  kOptVerbose,
  kOptTrace,
  kOptParse,
  kOptDefine,
  kOptInputDir,
  kOptCompile,
  kOptCompileToBC,
  kOptCompileToIR,
  kOptOptimizeMore,
  kOptOptimizeNone,
};


int
main(int argc, char** argv)
{
  static const OptionsParser::OptionsDefine herschelOptions[] = {
    { kOptHelp,         "-h",  "--help",           !K(argument) },
    { kOptVersion,      "-v",  "--version",        !K(argument) },
    { kOptOutdir,       "-d",  "--outdir",          K(argument) },
    { kOptOutput,       "-o",  "--output",          K(argument) },
    { kOptVerbose,      NULL,  "--verbose",        !K(argument) },
    { kOptTrace,        "-T",  "--trace",           K(argument) },
    { kOptParse,        "-P",  "--parse",          !K(argument) },
    { kOptDefine,       "-D",  "--define",          K(argument) },
    { kOptInputDir,     "-I",  "--input",           K(argument) },
    { kOptCompileToIR,  "-s",  NULL,               !K(argument) },
    { kOptCompile,      "-c",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O1", NULL,               !K(argument) },
    { kOptOptimizeNone, "-On", NULL,               !K(argument) },
    { 0,                NULL,  NULL,               !K(argument) } // sentinel
  };

  String outputFileName;
  String outdir;
  std::vector<String> hrcOptions;

  CompileFunction func = kDisplayHelp;
  std::vector<String> files;
  OptionsParser::ArgumentType type;
  OptionsParser::Option option;
  OptionsParser optp(herschelOptions, argc, (const char**)argv);

  bool verbose = false;

  while ((type = optp.nextOption(&option)) != OptionsParser::kNoMoreArgs) {
    switch (type) {
    case OptionsParser::kOption:
      switch (option.fId) {
      case kOptHelp:
        displayHelp();
        exit(0);

      case kOptVersion:
        func = kDisplayVersion;
        break;

      case kOptOutdir:
        outdir = option.fArgument;
        hrcOptions.push_back(String("-d"));
        hrcOptions.push_back(option.fArgument);
        break;

      case kOptOutput:
        outputFileName = option.fArgument;
        // don't pass outdir to hrc.  We handle it outselves
        break;

      case kOptVerbose:
        verbose = true;
        hrcOptions.push_back(String("-v"));
        break;

      case kOptTrace:
        hrcOptions.push_back(String("-T"));
        hrcOptions.push_back(option.fArgument);
        break;

      case kOptParse:
        func = kParseFiles;
        hrcOptions.push_back(String("-P"));
        break;
      case kOptCompile:
        hrcOptions.push_back(String("-c"));
        func = kCompileFiles;
        break;
      case kOptCompileToIR:
        hrcOptions.push_back(String("-s"));
        func = kCompileFilesToIR;
        break;

      case kOptDefine:
        hrcOptions.push_back(String("-D"));
        hrcOptions.push_back(option.fArgument);
        break;

      case kOptInputDir:
        hrcOptions.push_back(String("-I"));
        hrcOptions.push_back(option.fArgument);
        break;

      case kOptOptimizeMore:
        hrcOptions.push_back(String("-O"));
        break;
      case kOptOptimizeNone:
        hrcOptions.push_back(String("-On"));
        break;
      }
      break;

    case OptionsParser::kUnknownOption:
      logf(kError, "Unknown option: %s\n", (const char*)StrHelper(option.fOption));
      break;

    case OptionsParser::kMissingArgument:
      logf(kError, "Missing value for option: %s\n",
           (const char*)StrHelper(option.fOption));
      break;

    case OptionsParser::kNotAnOption:
      files.push_back(option.fArgument);

      if (func == kDisplayHelp)
        func = kLinkAndCompileFiles;
      break;

    default: ;
    }
  }


  Setup setup = herschel::findResources("herschel");

  for (size_t i = 0; i < files.size(); i++)
    hrcOptions.push_back(files[i]);

  switch (func) {
  case kDisplayHelp:
    displayHelp();
    break;
  case kDisplayVersion:
    displayVersion(verbose);
    exit(0);

  case kParseFiles:
  case kCompileFilesToIR:
  case kCompileFiles:
    startProcess(setup.fHrcPath, hrcOptions);
    break;

  case kLinkAndCompileFiles:
    hrcOptions.insert(hrcOptions.begin(), String("-c"));
    if (startProcess(setup.fHrcPath, hrcOptions) >= 0) {
      if (!setup.fAsPath.isEmpty()) {
        // TODO
      }

      std::vector<String> outFiles;
      for (size_t i = 0; i < files.size(); i++) {
        String outExt = makeCompileOutputFileExt(kLLVM_BC);
        String outFile = makeOutputFileName(outdir, String(),
                                            files[i], outExt);
        outFiles.push_back(outFile);
      }

      std::vector<String> ldOptions;
      ldOptions.insert(ldOptions.end(),
                       setup.fLdFlags.begin(),
                       setup.fLdFlags.end());

      if (!outputFileName.isEmpty())
      {
        ldOptions.push_back(String("-o"));
        ldOptions.push_back(outputFileName);
      }

      ldOptions.push_back(String("-b=") + makeOutputFileName(outdir, String(),
                                                             outputFileName, String("bc")));
      ldOptions.insert(ldOptions.end(),
                       outFiles.begin(), outFiles.end());
      ldOptions.push_back(setup.fRuntimeLib);
      startProcess(setup.fLdPath, ldOptions);
    }
    break;
  }

  if (verbose) {
    printf("------------------------------\n");
    printf("Setup:\n");
    printf("  hrc:    %s\n", (const char*)StrHelper(setup.fHrcPath));
    printf("  as:     %s %s\n", (const char*)StrHelper(setup.fAsPath),
           (const char*)StrHelper(setup.fAsFlags));
    printf("  linker: %s\n", (const char*)StrHelper(setup.fLdPath));
    printf("  rtlib:  %s\n", (const char*)StrHelper(setup.fRuntimeLib));
  }

  return 0;
}

