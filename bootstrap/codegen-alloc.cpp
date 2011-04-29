/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#include "codegen.h"
#include "apt.h"
#include "log.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"
#include "predefined.h"

#include <vector>

#include "llvm/Analysis/Verifier.h"
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/PassManager.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/GlobalVariable.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"


//----------------------------------------------------------------------------

using namespace herschel;


llvm::Value*
CodeGenerator::emitTypeNameForAllocate(const AptNode* node)
{
  if (const TypeNode* typeNode = dynamic_cast<const TypeNode*>(node)) {
    Type ty = typeNode->type();
    hr_assert(ty.typeName() == String("lang|Class"));
    hr_assert(ty.hasGenerics());
    hr_assert(ty.generics().size() == 1);

    return makeGetTypeLookupCall(ty.generics()[0]);
  }
  else {
  //   llvm::Value* val = wrapLoad(codegenNode(args[i]));
    hr_invalid("todo");
    return NULL;
  }
}


llvm::Value*
CodeGenerator::emitAllocateApply(const ApplyNode* node)
{
  const SymbolNode* symNode = dynamic_cast<const SymbolNode*>(node->base());
  hr_assert(symNode->name() == Names::kLangAllocate);
  hr_assert(symNode->refersTo() == kGeneric);

  String funcnm = String("allocate");

  llvm::Function *allocFunc = fModule->getFunction(llvm::StringRef(funcnm));
  if (allocFunc == NULL) {
    std::vector<const llvm::Type*> sign;
    sign.push_back(fTypes.getAtomType()->getPointerTo());
    sign.push_back(fTypes.getTypeType()); // Type*

    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context()),
                                                     sign,
                                                     !K(isVarArg));

    allocFunc = llvm::Function::Create(ft,
                                     llvm::Function::ExternalLinkage,
                                     llvm::Twine("allocate"),
                                     module());
  }

  const NodeList& args = node->children();
  if (args.size() != 1) {
    errorf(node->srcpos(), 0, "Incorrect # arguments passed");
    return NULL;
  }

  std::vector<llvm::Value*> argv;
  llvm::Function* curFunction = fBuilder.GetInsertBlock()->getParent();
  llvm::AllocaInst* retv = createEntryBlockAlloca(curFunction,
                                                  String("local_retv"),
                                                  fTypes.getAtomType());
  hr_assert(retv != NULL);
  argv.push_back(retv);

  for (size_t i = 0, e = args.size(); i != e; ++i) {
    llvm::Value* val = emitTypeNameForAllocate(args[i]);
    hr_assert(val != NULL);
    argv.push_back(val);
  }

  hr_assert(allocFunc != NULL);
  fBuilder.CreateCall(allocFunc, argv.begin(), argv.end());

  // TODO: if in tail position enforce ATOM return type?
  return retv;
}
