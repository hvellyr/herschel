/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.hpp"

#include "ast.hpp"
#include "compiler.hpp"
#include "log.hpp"
#include "properties.hpp"
#include "setup.hpp"
#include "str.hpp"

#include "cxxopts.hpp"

#include <vector>


using namespace herschel;

enum CompileFunction {
  kParseFiles,
  kCompileFiles,
};


namespace herschel {
static void setupDefaultPath();
};


int main(int argc, char** argv)
{
  String outputFile;

  CompileFunction func = kCompileFiles;
  std::vector<String> files;

  std::string prog_name(argv[0]);
  cxxopts::Options options(prog_name, " <inputs> - herschel compiler");
  options.add_options()("h;help", "Print help and exit")(
      "V;version", "Print version and exit")("v;verbose", "Be verbose")(
      "d;outdir", "Output all generated files to DIR",
      cxxopts::value<std::string>())("o;output", "", cxxopts::value<std::string>())(
      "T;trace",
      "Trace various aspects: tokenizer pass1 pass2 annotate transform typify "
      "import macro codedump typeconv",
      cxxopts::value<std::string>())("D;define", "Define config VAR to be VALUE",
                                     cxxopts::value<std::vector<std::string>>())(
      "I;input", "Add DIR to the input searchlist",
      cxxopts::value<std::vector<std::string>>())(
      "isys", "Root to the system library", cxxopts::value<std::vector<std::string>>())(
      "P;parse", "Only parse the source files")("s;emit-bc",
                                                "Only compile the source files, no link")(
      "c;emit-llvm", "Compile to LLVM IR, no link")("O;optimize", "Optimize code more")(
      "On;optimize-off", "Turn off any (even basic) optimization")
#if defined(UNITTESTS)
      ("dont-import", "")("parse-1", "")("parse-2", "")("parse-3", "")("parse-4", "")
#endif
          ("f;file", "Input file", cxxopts::value<std::vector<std::string>>());

  setupDefaultPath();

  try {
    options.parse_positional("file");
    options.parse(argc, argv);
  }
  catch (const std::exception& opt) {
    HR_LOG(kError) << opt.what();
    return 1;
  }

  if (options.count("help")) {
    std::cout << options.help({ "" }) << std::endl;
    return 0;
  }
  if (options.count("version")) {
    std::cout << prog_name << " - vr. " << VERSION << std::endl;
    std::cout << "Copyright (c) " << COPYRIGHTYEAR << ", " << COPYRIGHTOWNER << std::endl;
    std::cout << "(base revision: " << HR_BASE_REVISION << ")" << std::endl;
    return 0;
  }

  if (options.count("verbose")) {
    Properties::setIsVerbose(true);
  }
  if (options.count("outdir")) {
    Properties::setOutdir(String(options["outdir"].as<std::string>()));
  }
  if (options.count("output")) {
    outputFile = String(options["output"].as<std::string>());
  }
  if (options.count("trace")) {
    Properties::setTraces(String(options["trace"].as<std::string>()));
  }
  if (options.count("parse")) {
    func = kParseFiles;
  }

  if (options.count("emit-llvm")) {
    func = kCompileFiles;
    Properties::setCompileOutFormat(kLLVM_BC);
  }
  if (options.count("emit-bc")) {
    func = kCompileFiles;
    Properties::setCompileOutFormat(kLLVM_IR);
  }

  if (options.count("define")) {
    for (const auto& var : options["define"].as<std::vector<std::string>>())
      Properties::setConfigVar(String(var));
  }
  if (options.count("input")) {
    for (const auto& var : options["input"].as<std::vector<std::string>>())
      Properties::addInputDir(String(var));
  }
  if (options.count("isys")) {
    for (const auto& var : options["isys"].as<std::vector<std::string>>())
      Properties::addSystemDir(String(var));
  }

  if (options.count("optimize")) {
    Properties::setOptimizeLevel(kOptLevelBasic);
  }
  if (options.count("optimize-off")) {
    Properties::setOptimizeLevel(kOptLevelNone);
  }

#if defined(UNITTESTS)
  if (options.count("dont-import")) {
    Properties::test_setDontImport(true);
  }
  if (options.count("parse-1")) {
    Properties::test_setPassLevel(1);
  }
  if (options.count("parse-2")) {
    Properties::test_setPassLevel(2);
  }
  if (options.count("parse-3")) {
    Properties::test_setPassLevel(3);
  }
  if (options.count("parse-4")) {
    Properties::test_setPassLevel(4);
  }
#endif

  if (options.count("file")) {
    for (auto f : options["file"].as<std::vector<std::string>>())
      files.emplace_back(String(f));
  }

  switch (func) {
  case kParseFiles: parseFiles(files, outputFile); break;

  case kCompileFiles: compileFiles(files, outputFile); break;
  }

  return 0;
}


static void herschel::setupDefaultPath()
{
  Setup setup = findResources("hrc");

  for (StringVector::iterator it = setup.fSysPath.begin(), e = setup.fSysPath.end();
       it != e; ++it) {
    if (!it->isEmpty())
      Properties::addSystemDir(*it);
  }
}
