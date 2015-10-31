/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/


#include "apt.h"
#include "codegen-apply.h"
#include "codegen-binnode.h"
#include "codegen-func.h"
#include "codegen-if.h"
#include "codegen-init.h"
#include "codegen-slot.h"
#include "codegen-tools.h"
#include "codegen-types.h"
#include "codegen-vardef.h"
#include "codegen-while.h"
#include "codegen.h"
#include "log.h"
#include "predefined.h"
#include "properties.h"
#include "symbol.h"
#include "xmlout.h"

#include <vector>
#include <system_error>
#include <typeinfo>

#include "llvm/ADT/StringRef.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
//#include "llvm/DIBuilder.h"

using namespace herschel;


//----------------------------------------------------------------------------

CodeGenerator::CodeGenerator(Compiler* compiler)
  : fCompiler(compiler),
    fContext(llvm::getGlobalContext()),
    fModule(nullptr),
    // fDIBuilder(nullptr),
    fBuilder(context()),
    fOptPassManager(nullptr),
    fDataLayout(nullptr),
    fInitializer(new ModuleRuntimeInitializer(this)),
    fTypes(new CodegenTypeUtils(this)),
    fTools(new CodegenTools(this)),
    fHasMainFunc(false)
{
  hr_assert(fCompiler);

  llvm::InitializeNativeTarget();

  fModule = new llvm::Module("compile-unit", fContext);
  fModule->setTargetTriple(llvm::sys::getDefaultTargetTriple());

  std::string error;
  const llvm::Target* target =
    llvm::TargetRegistry::lookupTarget(fModule->getTargetTriple(), error);
  if (!target) {
    logf(kError, "Compile setup failure: %s", error.c_str());
    exit(1);
  }

  llvm::TargetMachine* machine = target->createTargetMachine(fModule->getTargetTriple(),
                                                             "", // CPU
                                                             "", // Features
                                                             llvm::TargetOptions() );
  if (!machine) {
    logf(kError, "Compile setup failure: no matching TargetMachine found");
    exit(1);
  }
  fDataLayout = machine->getDataLayout();

  fModule->setDataLayout(machine->createDataLayout());

  // logf(kInfo, "Host triple: %s", fModule->getTargetTriple().c_str());
  // logf(kInfo, "PointerSize: %d", fDataLayout->getPointerSize());

  fOptPassManager = new llvm::legacy::FunctionPassManager(fModule);

  setupOptPassManager();
}


void
CodeGenerator::setupOptPassManager()
{
  if (Properties::optimizeLevel() > kOptLevelNone) {
    // Promote allocas to registers.
    fOptPassManager->add(llvm::createPromoteMemoryToRegisterPass());

    fOptPassManager->add(llvm::createScalarReplAggregatesPass());
    // fOptPassManager->add(llvm::createGlobalDCEPass());
    // fOptPassManager->add(llvm::createDeadArgEliminationPass());

    // fOptPassManager->add(llvm::createFunctionInliningPass());
    // fOptPassManager->add(llvm::createCondPropagationPass());
    fOptPassManager->add(llvm::createLoopRotatePass());

    // Hoist loop invariants
    fOptPassManager->add(llvm::createLICMPass());
    fOptPassManager->add(llvm::createLoopUnswitchPass());
    // Split loop index
    // fOptPassManager->add(llvm::createLoopIndexSplitPass());
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Canonicalize indvars
    fOptPassManager->add(llvm::createIndVarSimplifyPass());
    // Delete dead loops
    fOptPassManager->add(llvm::createLoopDeletionPass());
    // Unroll small loops
    fOptPassManager->add(llvm::createLoopUnrollPass());
    // Clean up after the unroller
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Remove redundancies
    fOptPassManager->add(llvm::createGVNPass());
    // Remove memcpy / form memset
    fOptPassManager->add(llvm::createMemCpyOptPass());
    // Constant prop with SCCP
    fOptPassManager->add(llvm::createSCCPPass());
    fOptPassManager->add(llvm::createTailCallEliminationPass());

    // Do simple "peephole" optimizations and bit-twiddling optzns.
    fOptPassManager->add(llvm::createInstructionCombiningPass());
    // Reassociate expressions.
    fOptPassManager->add(llvm::createReassociatePass());
    // Eliminate Common SubExpressions.
    fOptPassManager->add(llvm::createGVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc).
    fOptPassManager->add(llvm::createCFGSimplificationPass());

    fOptPassManager->doInitialization();
  }
}


CodeGenerator::~CodeGenerator()
{
  if (fOptPassManager) {
    delete fOptPassManager;
    fOptPassManager = nullptr;
  }
  if (fModule) {
    delete fModule;
    fModule = nullptr;
  }
}


bool
CodeGenerator::is64Bit() const
{
  return fDataLayout->getPointerSize() == 8;
}


const llvm::DataLayout*
CodeGenerator::dataLayout() const
{
  return fDataLayout;
}


llvm::LLVMContext&
CodeGenerator::context() const
{
  return fContext;
}


llvm::IRBuilder<>&
CodeGenerator::builder() const
{
  return const_cast<llvm::IRBuilder<>&>(fBuilder);
}


llvm::Module*
CodeGenerator::module() const
{
  return fModule;
}


llvm::legacy::FunctionPassManager*
CodeGenerator::optPassManager() const
{
  return fOptPassManager;
}


//------------------------------------------------------------------------------

bool
CodeGenerator::compileToCode(const CompileUnitNode* node,
                             const String& outputFile)
{
  node->codegen(this);

  fInitializer->finish();

  hr_assert(!outputFile.isEmpty());

  std::error_code errInfo;
  llvm::raw_fd_ostream outstream(llvm::StringRef(StrHelper(outputFile)),
                                 errInfo, llvm::sys::fs::OpenFlags::F_RW);
  if (errInfo) {
    logf(kError, "Failed to open output file '%s': %s",
         (zstring)StrHelper(outputFile), errInfo.message().c_str());
    return false;
  }

  switch (Properties::compileOutFormat()) {
  case kLLVM_IR:
    fModule->print(outstream, nullptr);
    break;
  case kLLVM_BC:
    llvm::WriteBitcodeToFile(fModule, outstream);
    break;
  }
  return true;
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegenNode(const AptNode* node, bool autoloadAllocInst)
{
  llvm::Value* val = node->codegen(this);
  if (autoloadAllocInst && llvm::AllocaInst::classof(val)) {
    val = fBuilder.CreateLoad(val);
  }
  return val;
}


llvm::Value*
CodeGenerator::codegen(const CompileUnitNode* node)
{
  const NodeList& nl = node->children();
  for (size_t i = 0; i < nl.size(); i++)
    codegenNode(nl[i].obj());

  if (fHasMainFunc) {
    createDefaultCMainFunc();
  }

  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const NodeList& nl)
{
  llvm::Value* lastVal = nullptr;

  for (size_t bidx = 0; bidx < nl.size(); bidx++) {
    llvm::Value* val = codegenNode(nl[bidx]);
    if (!val)
      return nullptr;
    lastVal = val;
  }
  return lastVal;
}


//------------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SymbolNode* node)
{
  if (node->name() == Names::kLangUnspecified) {
    // TODO
    return llvm::Constant::getNullValue(fTypes->getType(node->type()));
  }

  llvm::Value* val = nullptr;

  switch (node->refersTo()) {
  case kLocalVar:
  case kParam:
    val = fNamedValues[node->name()];
    break;
  case kGlobalVar:
    val = fGlobalVariables[node->name()];
    break;
  default:
    logf(kError, "Unexpected symbol reference '%s' (%d)",
         (zstring)StrHelper(node->name()), node->refersTo());
    hr_invalid("unexpected symbol reference");
  }

  if (!val) {
    logf(kError, "Unknown symbol '%s'", (zstring)StrHelper(node->name()));
    return nullptr;
  }

  return val;
}


llvm::Value*
CodeGenerator::codegen(const ArrayTypeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO

  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const TypeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO

  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const SlotdefNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const VardefNode* node, bool isLocal)
{
  return CodegenVardef(this).emit(node, isLocal);
}


llvm::Value*
CodeGenerator::codegen(const AssignNode* node)
{
  const AptNode* lvalue = node->lvalue();

  if (const SymbolNode* lsym = dynamic_cast<const SymbolNode*>(lvalue)) {
    llvm::Value* rvalue = codegenNode(node->rvalue());
    if (!rvalue)
      return nullptr;

    // Look up the name.
    llvm::AllocaInst* var = fNamedValues[lsym->name()];
    if (!var) {
      logf(kError, "Unknown symbol '%s'", (zstring)StrHelper(lsym->name()));
      return nullptr;
    }

    llvm::Value* val = fTools->emitPackCode(node->rvalue()->dstType(),
                                            node->rvalue()->typeConv(),
                                            fTools->wrapLoad(rvalue),
                                            node->rvalue()->type());
    fBuilder.CreateStore(fTools->wrapLoad(val), var);

    return rvalue;
  }
  else if (const SlotRefNode* lslot = dynamic_cast<const SlotRefNode*>(lvalue)) {
    return CodegenSlot(this).emitSlotRefAssignment(lslot, node->rvalue());
  }

  logf(kError, "Not supported yet: %s", typeid(node).name());
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const DefNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode)
    return codegen(vardefNode, !K(isLocal));

  const FuncDefNode* func = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (func)
    return codegen(func, !K(isLocal));

  const TypeDefNode* type = dynamic_cast<const TypeDefNode*>(node->defNode());
  if (type)
    return codegen(type);

  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const LetNode* node)
{
  const VardefNode* vardefNode = dynamic_cast<const VardefNode*>(node->defNode());
  if (vardefNode)
    return codegen(vardefNode, K(isLocal));

  const FuncDefNode* funcDefNode = dynamic_cast<const FuncDefNode*>(node->defNode());
  if (funcDefNode) {
    logf(kError, "Compiling local functions not supported yet: %s", typeid(node).name());
    return nullptr;
  }

  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const BlockNode* node)
{
  llvm::Function *curFunction = fBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock* bb = llvm::BasicBlock::Create(context(),
                                                  "inner", curFunction);
  llvm::BasicBlock* contBB = llvm::BasicBlock::Create(context(),
                                                      "next", curFunction);
  // Insert an explicit fall through from the current block to the loopBB.
  fBuilder.CreateBr(bb);
  fBuilder.SetInsertPoint(bb);

  llvm::Value* lastVal = codegen(node->children());

  fBuilder.CreateBr(contBB);
  fBuilder.SetInsertPoint(contBB);

  return lastVal;
}


llvm::Value*
CodeGenerator::codegen(const BoolNode* node)
{
// TODO plaintype?
  if (node->dstType().isPlainType()) {
    if (node->value())
      return fBuilder.getTrue();
    else
      return fBuilder.getFalse();
  }

  return fTools->makeBoolAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const CharNode* node)
{
  if (node->dstType().isPlainType()) {
    return llvm::ConstantInt::get(context(),
                                  llvm::APInt(32, node->value(), !K(issigned)));
  }
  return fTools->makeCharAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const KeywordNode* node)
{
  return fTools->makeKeywordAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const IntNode* node)
{
  return llvm::ConstantInt::get(fTypes->getType(node->type()),
                                node->value(),
                                node->type().isSigned());
}


llvm::Value*
CodeGenerator::codegen(const RealNode* node)
{
  return llvm::ConstantFP::get(context(),
                               llvm::APFloat(node->value()));
}


llvm::Value*
CodeGenerator::codegen(const StringNode* node)
{
  return fTools->makeStringAtom(node->value());
}


llvm::Value*
CodeGenerator::codegen(const RationalNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const ArrayNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const DictNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const VectorNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const RangeNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const FuncDefNode* node, bool isLocal)
{
  return CodegenFuncDef(this).emit(node, isLocal);
}


llvm::Value*
CodeGenerator::codegen(const FunctionNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const ApplyNode* node)
{
  return CodegenApply(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const KeyargNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const ParamNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const BinaryNode* node)
{
  return CodegenBinaryNode(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const UnaryNode* node)
{
  llvm::Value *base = fTools->wrapLoad(codegenNode(node->base()));
  if (!base)
    return nullptr;

  switch (node->op()) {
  case kUnaryOpNegate:
    return fBuilder.CreateMul(base,
                              llvm::ConstantInt::get(context(),
                                                     llvm::APInt(32, (uint64_t)-1, true)),
                              "negtmp");
  case kUnaryOpNot:
    return fBuilder.CreateNot(base, "nottmp");

  case kUnaryOpInvalid:
    hr_invalid("");
  }

  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const IfNode* node)
{
  return CodegenIf(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const MatchNode* node)
{
  hr_invalid("there should be no match mode anymore in this phase");
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const OnNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const SelectNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const TypeDefNode* node)
{
  fInitializer->addTypeDef(node);
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const UnitConstNode* node)
{
  logf(kError, "Not supported yet: %s", typeid(node).name());
  // TODO
  return nullptr;
}


llvm::Value*
CodeGenerator::codegen(const WhileNode* node)
{
  return CodegenWhile(this).emit(node);
}


llvm::Value*
CodeGenerator::codegen(const CastNode* node)
{
  llvm::Value *val = fTools->wrapLoad(codegenNode(node->base()));
  if (!val)
    return nullptr;

  return fTools->emitPackCode(node->base()->dstType(),
                              node->base()->typeConv(),
                              val,
                              node->base()->type());
}


llvm::Value*
CodeGenerator::codegen(const UndefNode* node)
{
  hr_invalid("You shouldn't be here");
  return nullptr;
}


//----------------------------------------------------------------------------

llvm::Value*
CodeGenerator::codegen(const SlotRefNode* node)
{
  return CodegenSlot(this).emitSlotRefAccess(node);
}
