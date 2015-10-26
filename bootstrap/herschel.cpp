/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
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
#include "properties.h"

#include "llvm/Config/config.h"

using namespace herschel;

namespace
{
  //! Displays the tool's version and copyright information
  void
  displayVersion(bool verbose)
  {
    printf("%s %s - herschel compiler\n", "herschel", VERSION);
    printf("Copyright (c) %s, %s\n", COPYRIGHTYEAR, COPYRIGHTOWNER);
    printf("(base revision: %s)\n", HR_BASE_REVISION);

    if (verbose) {
      printf("Using LLVM: %s\n", PACKAGE_STRING);
    }
  }


  //! Displays the tool's usage and (main) command line options.
  void
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
    printf("                                codedump|typeconv}\n");
    printf("  -d DIR,  --outdir=DIR        Output all generated files to DIR\n");
    printf("  -I DIR,  --input=DIR         Add DIR to the input searchlist\n");
    printf("\n");
    printf("  -P,      --parse             Only parse the source files\n");
    printf("  -c                           Only compile the source files, no link\n");
    printf("  -s                           Compile to LLVM IR, no link\n");
    printf("\n");
    printf("  -O                           Optimize code more\n");
    printf("  -On                          Turn off any (even basic) optimization\n");
    printf("\n");
    printf("  -Xl,On                       disable (all) linker optimizations\n");
    printf("  --Xlinker=OPTS               pass OPTS to the clang linker.  Multiple\n");
    printf("                               options can be comma separated\n");
  }


  //! Splits the comma separated linker from \p arg options are returns as
  //! separate tokens in \p options.
  void
  splitLinkerOptions(std::vector<String>& options, const String& arg)
  {
    String tmp = arg;
    String lhs;

    while (tmp.split(',', lhs, tmp) >= 0) {
      if (!lhs.isEmpty())
        options.push_back(lhs);
    }

    if (!tmp.isEmpty())
      options.push_back(tmp);
  }


  //! The main functions which can be specified from the command line
  enum CompileFunction {
    kDisplayHelp,
    kDisplayVersion,
    kParseFiles,
    kCompileFiles,
    kCompileFilesToIR,
    kLinkAndCompileFiles,
  };


  //! Additional options which can specified from the command line
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
    kOptInputSysDir,
    kOptCompile,
    kOptCompileToBC,
    kOptCompileToIR,
    kOptOptimizeMore,
    kOptOptimizeNone,
    kOptLinkOptimizeNone,
    kOptDebugJobs,
    kOptLinkerOpt
  };


  std::vector<String>
  callHrc(const Setup& setup, bool doTraceJobCalls, const String& outdir,
          const std::vector<String>& files,
          std::vector<String> hrcOptions)
  {
    std::vector<String> outFiles;

    hrcOptions.insert(hrcOptions.end(), files.begin(), files.end());

    if (startProcess(setup.fHrcPath, hrcOptions, doTraceJobCalls) >= 0) {
      for (const String& file : files) {
        String outExt = makeCompileOutputFileExt(kLLVM_BC);
        String outFile = makeOutputFileName(outdir, String(), file, outExt);
        outFiles.emplace_back(outFile);
      }
    }

    return outFiles;
  }


  std::vector<String>
  callLlc(const Setup& setup, bool doTraceJobCalls, const String& outdir,
          const std::vector<String>& bcFiles,
          std::vector<String> llcOptions)
  {
    std::vector<String> objFiles;

    llcOptions.insert(llcOptions.begin(), String("-filetype=obj"));

    for (const String& file : bcFiles) {
      std::vector<String> options(llcOptions.begin(), llcOptions.end());

      String outFile = makeOutputFileName(outdir, String(), file, String("o"));
      options.push_back(String("-o"));
      options.push_back(outFile);
      options.push_back(file);

      if (startProcess(setup.fLlcPath, options, doTraceJobCalls) >= 0) {
        objFiles.emplace_back(outFile);
      }
    }

    return objFiles;
  }


  void
  callLink(const Setup& setup, bool doTraceJobCalls, const String& outputFileName,
           const std::vector<String>& objFiles,
           const std::vector<String>& linkOptions)
  {
    std::vector<String> options;

    options.emplace_back(String("-o"));
    options.emplace_back(outputFileName);

    options.insert(options.end(),
                   setup.fLdFlags.begin(),
                   setup.fLdFlags.end());

    options.insert(options.end(), linkOptions.begin(), linkOptions.end());

    options.insert(options.end(), objFiles.begin(), objFiles.end());

    options.emplace_back(setup.fLangKit);
    options.emplace_back(setup.fRuntimeLib);

    startProcess(setup.fLdPath, options, doTraceJobCalls);
  }
}                    // namespace end


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
    { kOptDebugJobs,    NULL,  "--jobs",           !K(argument) },
    { kOptParse,        "-P",  "--parse",          !K(argument) },
    { kOptDefine,       "-D",  "--define",          K(argument) },
    { kOptInputDir,     "-I",  "--input",           K(argument) },
    { kOptInputSysDir,  NULL,  "--isys",            K(argument) },
    { kOptCompileToIR,  "-s",  NULL,               !K(argument) },
    { kOptCompile,      "-c",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O",  NULL,               !K(argument) },
    { kOptOptimizeMore, "-O1", NULL,               !K(argument) },
    { kOptOptimizeNone, "-On", NULL,               !K(argument) },
    { kOptLinkOptimizeNone, "-Xl,On", NULL,        !K(argument) },
    { kOptLinkerOpt,    NULL,  "--Xl",              K(argument) },
    { 0,                NULL,  NULL,               !K(argument) } // sentinel
  };

  String outputFileName;
  String outdir;
  std::vector<String> hrcOptions;
  std::vector<String> llcOptions;
  std::vector<String> linkOptions;
  std::vector<String> specLdOptions;

  CompileFunction func = kDisplayHelp;
  std::vector<String> files;
  OptionsParser::ArgumentType type;
  OptionsParser::Option option;
  OptionsParser optp(herschelOptions, argc, (const char**)argv);

  bool verbose = false;
  bool doTraceJobCalls = false;

  // Decode the options and collect the passed arguments in local variables.
  // Options which are targeted for the subtool hrc are collected into \c
  // hrcOptions, options for the llvm linker into \c specLdOptions.
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
        // don't pass outdir to hrc here.  Depending on the compile mode We
        // handle it outselves
        break;

      case kOptVerbose:
        verbose = true;
        hrcOptions.push_back(String("-v"));
        break;

      case kOptTrace:
        hrcOptions.push_back(String("-T"));
        hrcOptions.push_back(option.fArgument);
        break;
      case kOptDebugJobs:
        doTraceJobCalls = true;
        break;

      case kOptLinkerOpt:
        splitLinkerOptions(specLdOptions, option.fArgument);
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
      case kOptInputSysDir:
        hrcOptions.push_back(String("--isys=") + option.fArgument);
        break;

      case kOptOptimizeMore:
        hrcOptions.push_back(String("-O"));
        break;
      case kOptOptimizeNone:
        hrcOptions.push_back(String("-On"));
        break;
      case kOptLinkOptimizeNone:
        //specLdOptions.push_back(String("-disable-inlining"));
        //specLdOptions.push_back(String("-disable-opt"));
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


  // Find system and installation specific setup information (i.e. where is
  // the linker we should use, which are local linker settings, etc.).
  Setup setup = herschel::findResources("herschel");

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
    if (!outputFileName.isEmpty())
    {
      hrcOptions.push_back(String("-o"));
      hrcOptions.push_back(outputFileName);
    }

    callHrc(setup, doTraceJobCalls, outdir, files, hrcOptions);
    break;

  case kLinkAndCompileFiles:
    {
      hrcOptions.insert(hrcOptions.begin(), String("-c"));
      std::vector<String> bcFiles = callHrc(setup, doTraceJobCalls, outdir, files, hrcOptions);

      std::vector<String> objFiles = callLlc(setup, doTraceJobCalls, outdir, bcFiles, llcOptions);

      callLink(setup, doTraceJobCalls, outputFileName, objFiles, linkOptions);
    }
    break;
  }

  if (verbose) {
    printf("------------------------------\n");
    printf("Setup:\n");
    printf("  hrc:     %s\n", (const char*)StrHelper(setup.fHrcPath));
    printf("  linker:  %s\n", (const char*)StrHelper(setup.fLdPath));
    printf("  langkit: %s\n", (const char*)StrHelper(setup.fLangKit));
    printf("  rtlib:   %s\n", (const char*)StrHelper(setup.fRuntimeLib));
  }

  return 0;
}
