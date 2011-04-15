/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_codegen_h
#define bootstrap_codegen_h

#include "refcountable.h"
#include "ptr.h"
#include "apt.h"

#include <map>
#include <vector>
#include <string>

#include "llvm/Support/IRBuilder.h"

namespace llvm
{
  class AllocaInst;
  class BasicBlock;
  class Function;
  class FunctionPassManager;
  class LLVMContext;
  class Module;
  class Value;
  // class DIBuilder;
};


namespace herschel
{
  class ApplyNode;
  class AptNode;
  class ArrayNode;
  class ArrayTypeNode;
  class AssignNode;
  class BinaryNode;
  class BlockNode;
  class BoolNode;
  class CastNode;
  class CharNode;
  class CompileUnitNode;
  class DefNode;
  class DictNode;
  class FuncDefNode;
  class FunctionNode;
  class IfNode;
  class IntNode;
  class KeyargNode;
  class KeywordNode;
  class LetNode;
  class MatchNode;
  class NegateNode;
  class OnNode;
  class ParamNode;
  class RangeNode;
  class RationalNode;
  class RealNode;
  class SelectNode;
  class SlotdefNode;
  class StringNode;
  class SymbolNode;
  class TypeDefNode;
  class TypeNode;
  class Type;
  class UnitConstNode;
  class VardefNode;
  class VectorNode;
  class UndefNode;
  class WhileNode;

  class String;

  typedef std::vector<Ptr<AptNode> > NodeList;


  //----------------------------------------------------------------------------

  //! The code generation pass.
  class CodeGenerator : public RefCountable
  {
  public:
    CodeGenerator();
    ~CodeGenerator();

    bool compileToCode(const CompileUnitNode* node, const String& outputFile);

    llvm::Value* codegenNode(const AptNode* node, bool autoloadAllocInst = false);

    llvm::Value* codegen(const ApplyNode* node);
    llvm::Value* codegen(const ArrayNode* node);
    llvm::Value* codegen(const ArrayTypeNode* node);
    llvm::Value* codegen(const AssignNode* node);
    llvm::Value* codegen(const BinaryNode* node);
    llvm::Value* codegen(const BlockNode* node);
    llvm::Value* codegen(const BoolNode* node);
    llvm::Value* codegen(const CastNode* node);
    llvm::Value* codegen(const CharNode* node);
    llvm::Value* codegen(const CompileUnitNode* node);
    llvm::Value* codegen(const DefNode* node);
    llvm::Value* codegen(const DictNode* node);
    llvm::Value* codegen(const FunctionNode* node);
    llvm::Value* codegen(const IfNode* node);
    llvm::Value* codegen(const IntNode* node);
    llvm::Value* codegen(const KeyargNode* node);
    llvm::Value* codegen(const KeywordNode* node);
    llvm::Value* codegen(const LetNode* node);
    llvm::Value* codegen(const MatchNode* node);
    llvm::Value* codegen(const NegateNode* node);
    llvm::Value* codegen(const OnNode* node);
    llvm::Value* codegen(const ParamNode* node);
    llvm::Value* codegen(const RangeNode* node);
    llvm::Value* codegen(const RationalNode* node);
    llvm::Value* codegen(const RealNode* node);
    llvm::Value* codegen(const SelectNode* node);
    llvm::Value* codegen(const SlotdefNode* node);
    llvm::Value* codegen(const StringNode* node);
    llvm::Value* codegen(const SymbolNode* node);
    llvm::Value* codegen(const TypeDefNode* node);
    llvm::Value* codegen(const TypeNode* node);
    llvm::Value* codegen(const UnitConstNode* node);
    llvm::Value* codegen(const VectorNode* node);
    llvm::Value* codegen(const WhileNode* node);
    llvm::Value* codegen(const UndefNode* node);

  private:
    llvm::Value* codegen(const FuncDefNode* node, bool isLocal);
    llvm::Value* codegen(const VardefNode* node, bool isLocal);

    llvm::LLVMContext& context();

    llvm::FunctionType* createFunctionSignature(const FunctionNode* node,
                                                bool inlineRetv,
                                                const Type& retty);

    llvm::Value* codegen(const NodeList& nl);

    void createDefaultCMainFunc();

    void addGlobalCtor(llvm::Function* ctor, int priority);
    void addGlobalDtor(llvm::Function* dtor, int priority);

    typedef std::vector<std::pair<llvm::Constant*, int> > CtorList;
    void emitCtorList(const CtorList &fns, const char *globalName);
    llvm::Function*
    createGlobalInitOrDtorFunction(const llvm::FunctionType *ft,
                                   const String& name);
    llvm::Value* codegenForGlobalVars(const VardefNode* node);

    llvm::AllocaInst* createEntryBlockAlloca(llvm::Function *func,
                                             const String& name,
                                             const llvm::Type* type);

    const llvm::Type* getAtomType();
    const llvm::Type* getTypeType();
    const llvm::Type* getTypeSlotPairType();
    const llvm::Type* getType(const Type& type);

    llvm::Value* makeTypeCastAtomToPlain(llvm::Value* val, const Type& dstType);

    enum Typeid {
      kAtomInt  = 0,
      kAtomBool = 1,
      kAtomChar = 2,
    };

    void setAtom(llvm::AllocaInst* atom, Typeid typid, llvm::Value* value);

    void assignAtom(llvm::Value* src, llvm::Value* dst);

    llvm::Function* getIntrinsic(unsigned int iid,
                 const llvm::Type** tys, unsigned int numTys);
    llvm::Function* getMemCpyFn(const llvm::Type* dstType,
                                const llvm::Type* srcType,
                                const llvm::Type* sizeType);

    llvm::Value* wrapLoad(llvm::Value* val);

    llvm::Value* makeIntAtom(int val);
    llvm::Value* makeIntAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(llvm::Value* val);
    llvm::Value* makeBoolAtom(bool val);

    llvm::Value* makeTypeRegisterCall(llvm::Value* newType);
    llvm::Value* makeClassAllocCall(const Type& ty);
    llvm::Value* makeTypeAllocCall(const Type& ty);
    llvm::Value* makeTypeOrCallRegistration(const TypeDefNode* tdnode);
    llvm::Value* makeTypeOrCallRegistration(const Type& ty);
    llvm::Value* makeTypeLookupCall(const Type& ty);
    llvm::Value* makeIsaTypeLookupCall(const Type& ty);

    void emitRuntimeInitFunc();
    void emitClassInitFunc();
    void emitGlobalVarInitFunc();
    void emitTypeGetterFunctions();

    //------------------------------ emit operators
    bool isPlainInt(const Type& type) const;

    llvm::Value* wrapInt(llvm::Value* value, const Type& type);
    llvm::Value* wrapBool(llvm::Value* value, const Type& type);

    llvm::Value* coerceIntOperand(const Type& dstType, const Type& isType,
                                  llvm::Value* value);

    llvm::Value* codegenOpIntInt(const BinaryNode* node,
                                 llvm::Value* left,
                                 llvm::Value* right);

    llvm::Value* emitPackCode(const Type& dstType, TypeConvKind convKind,
                              llvm::Value* value,
                              const Type& valType);

    //-------- data members

    llvm::LLVMContext& fContext;
    llvm::Module*     fModule;
    // llvm::DIBuilder*  fDIBuilder;
    llvm::IRBuilder<> fBuilder;
    llvm::FunctionPassManager* fOptPassManager;
    llvm::AllocaInst *fCurrentValue;
    bool fHasMainFunc;
    CtorList fGlobalCtors;
    CtorList fGlobalDtors;
    // takes llvm::Value or llvm::GlobalVariable
    std::map<String, llvm::AllocaInst*> fNamedValues;
    std::map<String, llvm::GlobalVariable*> fGlobalVariables;

    std::vector<const TypeDefNode*> fClassInitFuncs;
    std::vector<const VardefNode*> fGlobalInitVars;

    std::vector<Type> fTypesGetters;
  };
};                              // namespace

#endif                          // bootstrap_codegen_h
