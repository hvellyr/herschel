/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.h"
#include "str.h"
#include "setup.h"
#include "log.h"
#include "job.h"
#include "filetool.h"
#include "properties.h"

#include "llvm/Config/config.h"

#include "cxxopts.hpp"

#include <vector>


using namespace herschel;

namespace
{
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
    kParseFiles,
    kCompileFiles,
    kCompileFilesToIR,
    kLinkAndCompileFiles,
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
  String outputFileName;
  String outdir;
  std::vector<String> hrcOptions;
  std::vector<String> llcOptions;
  std::vector<String> linkOptions;
  std::vector<String> specLdOptions;

  CompileFunction func = kDisplayHelp;
  std::vector<String> files;

  bool verbose = false;
  bool doTraceJobCalls = false;

  const std::string prog_help = "a compiler for the herschel language";
  const std::string prog_name = argv[0];
  cxxopts::Options options(prog_name, std::string(" <inputs> - ") + prog_help);
  options.add_options()
    ("h;help", "Print help and exit")
    ("V;version", "Print version and exit")
    ("v;verbose", "Be verbose", cxxopts::value<bool>(verbose))
    ("d;outdir", "Output all generated files to DIR", cxxopts::value<std::string>())
    ("o;output", "", cxxopts::value<std::string>())
    ("T;trace", "Trace various aspects: tokenizer pass1 pass2 annotate transform typify "
                                        "import macro codedump typeconv",
                cxxopts::value<std::string>())
    ("jobs", "Trace process information", cxxopts::value<bool>(doTraceJobCalls))
    ("D;define", "Define config VAR to be VALUE", cxxopts::value<std::vector<std::string>>())
    ("I;input", "Add DIR to the input searchlist", cxxopts::value<std::vector<std::string>>())
    ("isys", "Root to the system library", cxxopts::value<std::vector<std::string>>())
    ("P;parse", "Only parse the source files")
    ("s;emit-bc", "Only compile the source files, no link")
    ("c;emit-llvm", "Compile to LLVM IR, no link")
    ("O;optimize", "Optimize code more")
    ("On;optimize-off", "Turn off any (even basic) optimization")
    ("Xl,On", "disable (all) linker optimizations")
    ("Xlinker", "pass OPTS to the clang linker.  Multiple options can be comma separated")
    ("f;file", "", cxxopts::value<std::vector<std::string>>())
    ;

  try {
    options.parse_positional("file");
    options.parse(argc, argv);
  }
  catch (const std::exception& opt) {
    logf(kError, "%s", opt.what());
    return 1;
  }

  if (options.count("help")) {
    std::cout << options.help({""}) << std::endl;
    return 0;
  }
  if (options.count("version")) {
    std::cout << prog_name << " - vr. " << VERSION << std::endl;
    std::cout << "Copyright (c) " << COPYRIGHTYEAR << ", " << COPYRIGHTOWNER << std::endl;
    std::cout << "(base revision: " << HR_BASE_REVISION << ")" << std::endl;

    if (verbose) {
      std::cout << "Using LLVM: " << PACKAGE_STRING << std::endl;
    }

    hrcOptions.emplace_back(String("-v"));
    return 0;
  }

  if (options.count("outdir")) {
    outdir = String(options["outdir"].as<std::string>());
    hrcOptions.emplace_back(String("-d"));
    hrcOptions.emplace_back(outdir);
  }
  if (options.count("output")) {
    outputFileName = String(options["output"].as<std::string>());
  }

  if (options.count("trace")) {
    hrcOptions.emplace_back(String("-T"));
    hrcOptions.emplace_back(String(options["trace"].as<std::string>()));
  }

  if (options.count("Xlinker")) {
    splitLinkerOptions(specLdOptions, String(options["Xlinker"].as<std::string>()));
  }

  if (options.count("parse")) {
    func = kParseFiles;
    hrcOptions.emplace_back(String("-P"));
  }
  if (options.count("emit-llvm")) {
    func = kCompileFiles;
    hrcOptions.emplace_back(String("-c"));
  }
  if (options.count("emit-bc")) {
    func = kCompileFilesToIR;
    hrcOptions.emplace_back(String("-s"));
  }

  if (options.count("define")) {
    for (const auto& var : options["define"].as<std::vector<std::string>>()) {
      hrcOptions.emplace_back(String("-D"));
      hrcOptions.emplace_back(String(var));
    }
  }
  if (options.count("input")) {
    for (const auto& var : options["input"].as<std::vector<std::string>>()) {
      hrcOptions.emplace_back(String("-I"));
      hrcOptions.emplace_back(String(var));
    }
  }
  if (options.count("isys")) {
    for (const auto& var : options["isys"].as<std::vector<std::string>>()) {
      hrcOptions.emplace_back(String("--isys=") + String(var));
    }
  }

  if (options.count("optimize")) {
    hrcOptions.emplace_back(String("-O"));
  }
  if (options.count("optimize-off")) {
    hrcOptions.emplace_back(String("-On"));
  }

  if (options.count("Xl,On")) {
    //specLdOptions.push_back(String("-disable-inlining"));
    //specLdOptions.push_back(String("-disable-opt"));
  }

  if (options.count("file")) {
    for (auto f : options["file"].as<std::vector<std::string>>())
      files.emplace_back(String(f));

    if (func == kDisplayHelp)
      func = kLinkAndCompileFiles;
  }

  // Find system and installation specific setup information (i.e. where is
  // the linker we should use, which are local linker settings, etc.).
  Setup setup = herschel::findResources("herschel");

  if (verbose) {
    std::cout << "Setup:" << std::endl
              << "  hrc:     " << StrHelper(setup.fHrcPath) << std::endl
              << "  llc:     " << StrHelper(setup.fLlcPath) << std::endl
              << "  linker:  " << StrHelper(setup.fLdPath) << std::endl
              << "  langkit: " << StrHelper(setup.fLangKit) << std::endl
              << "  rtlib:   " << StrHelper(setup.fRuntimeLib) << std::endl;
  }

  switch (func) {
  case kDisplayHelp:
    std::cout << options.help({""}) << std::endl;
    return 0;

  case kParseFiles:
  case kCompileFilesToIR:
  case kCompileFiles:
    if (!outputFileName.isEmpty())
    {
      hrcOptions.emplace_back(String("-o"));
      hrcOptions.emplace_back(outputFileName);
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

  return 0;
}
