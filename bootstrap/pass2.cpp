/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include <map>

#include "errcodes.h"
#include "log.h"
#include "compiler.h"
#include "parsertypes.h"
#include "pass2.h"
#include "predefined.h"
#include "properties.h"
#include "scope.h"
#include "symbol.h"
#include "token.h"
#include "tokeneval.h"
#include "tokenizer.h"
#include "typeenum.h"
#include "utils.h"
#include "xmlout.h"


using namespace herschel;

//----------------------------------------------------------------------------

NodifyPass::NodifyPass(int level, Compiler* compiler,
                       std::shared_ptr<Scope> scope)
  : Token2AptNodeCompilePass(level),
    fScope(std::move(scope)),
    fCompiler(compiler),
    fPass(new SecondPass(fCompiler, fScope))
{ }


AptNode*
NodifyPass::doApply(const Token& src)
{
  return fPass->parse(src);
}


std::shared_ptr<Scope>
NodifyPass::currentScope()
{
  return fPass->scope();
}


//----------------------------------------------------------------------------

SecondPass::SecondPass(Compiler* compiler, std::shared_ptr<Scope> scope)
  : AbstractPass(compiler, std::move(scope))
{
}


void
SecondPass::parseTopExprlist(const Token& expr)
{
  hr_assert(fRootNode != NULL);

  for (TokenVector::const_iterator it = expr.children().begin();
       it != expr.children().end();
       it++)
  {
    NodeList nl = parseExpr(*it);
    fRootNode->appendNodes(nl);
  }
}


AptNode*
SecondPass::parseModule(const Token& expr)
{
  hr_assert(expr.isSeq() && expr.count() >= 2);
  hr_assert(expr[0] == kModuleId);
  hr_assert(expr[1].isSymbol());

  String modName = expr[1].idValue();

  if (expr.count() > 2) {
    hr_assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

    {
      ScopeHelper scopeHelper(fScope,
                              K(doExport), K(isInnerScope),
                              kScopeL_Module);

      ModuleHelper moduleHelper(this, modName);
      parseTopExprlist(expr[2]);
    }
  }
  else {
    fScope = makeScope(kScopeL_Module, fScope);
    fCurrentModuleName = qualifyId(fCurrentModuleName, modName);
  }

  return NULL;
}


AptNode*
SecondPass::parseExport(const Token& expr)
{
  hr_assert(expr.isSeq() && expr.count() >= 2);
  hr_assert(expr[0] == kExportId);

  size_t symbolOfs = 1;
  VizType vizType = kPrivate;
  if (expr[1].isSymbol()) {
    if (expr[1] == Compiler::publicToken)
      vizType = kPublic;
    else if (expr[1] == Compiler::innerToken)
      vizType = kInner;
    else if (expr[1] == Compiler::outerToken)
      vizType = kOuter;
    else if (expr[1] == Compiler::privateToken)
      vizType = kPrivate;
    else {
      error(expr[1].srcpos(), E_UnknownVisibility,
            String("unknown visibility level: ") + expr[1]);
    }

    symbolOfs = 2;
  }

  hr_assert(expr.count() > symbolOfs);
  std::vector<Scope::ScopeName> symbols;

  if (expr[symbolOfs].isNested()) {
    Token symbolExprs = expr[symbolOfs];

    for (size_t j = 0; j < symbolExprs.count(); j++) {
      if (symbolExprs[j].isSymbol()) {
        symbols.push_back(Scope::ScopeName(Scope::kNormal, symbolExprs[j].idValue()));
      }
      else if (symbolExprs[j].isSeq()) {
        hr_assert(symbolExprs[j].count() == 3);
        hr_assert(symbolExprs[j][1] == kColon);
        hr_assert(symbolExprs[j][2] == kSymbol);

        Scope::ScopeDomain domain = Scope::kNormal;
        if (symbolExprs[j][2] == Compiler::unitToken)
          domain = Scope::kUnit;
        else if (symbolExprs[j][2] == Compiler::charToken)
          domain = Scope::kChar;
        else {
          warning(symbolExprs[j][2].srcpos(), E_UnknownSymbolDomain,
                  String("unknown symbol domain: ") + symbolExprs[j][2].idValue());
        }

        symbols.push_back(Scope::ScopeName(domain, symbolExprs[j][0].idValue()));
      }
    }
  }

  bool isFinal = false;
  if (expr.count() >= symbolOfs + 2) {
    hr_assert(expr[symbolOfs + 1] == kAs);
    hr_assert(expr[symbolOfs + 2] == Compiler::finalToken);

    isFinal = true;
  }


  for (std::vector<Scope::ScopeName>::const_iterator it = symbols.begin();
       it != symbols.end();
       it++)
  {
    Scope::ScopeDomain domain = it->fDomain;
    String fullId = ( isQualified(it->fName) || it->fName == String("*")
                      ? it->fName
                      : qualifyId(currentModuleName(), it->fName) );
    fScope->registerSymbolForExport(domain, fullId, vizType, isFinal);
  }

  return NULL;
}


AptNode*
SecondPass::parseImport(const Token& expr)
{
  hr_assert(expr.isSeq() && expr.count() >= 2);
  hr_assert(expr[0] == kImportId);
  hr_assert(expr[1].isString());

  String importFile = expr[1].stringValue();

  bool canImport = true;
#if defined(UNITTESTS)
  canImport = !Properties::test_dontImport();
#endif

  if (canImport) {
    try
    {
      fCompiler->importFile(expr.srcpos(), importFile, !K(isPublic), fScope);
    }
    catch (const Exception& e) {
      error(expr.srcpos(), E_UnknownInputFile, e.message());
    }
  }

  return NULL;
}


//------------------------------------------------------------------------------

void
SecondPass::parseExtendImpl(NodeList* functions, const Token& expr)
{
  hr_assert(expr.isSeq() && expr.count() == 4);
  hr_assert(expr[0] == kExtendId);
  hr_assert(expr[1] == kModuleId);
  hr_assert(expr[2] == kSymbol);
  hr_assert(expr[3].isNested());

  String moduleName = expr[2].idValue();

  {
    // temporarily change the current module name
    ModuleHelper modHelper(this, moduleName, K(setName));

    const TokenVector& children = expr[3].children();
    for (size_t i = 0; i < children.size(); i++) {
      NodeList nl = parseExpr(children[i]);
      appendNodes(*functions, nl);
    }
  }
}


AptNode*
SecondPass::parseExtend(const Token& expr)
{
  NodeList nodeList;

  parseExtendImpl(&nodeList, expr);

  for (size_t i = 0; i < nodeList.size(); i++) {
    AptNode* n = nodeList[i];
    if (n != NULL)
      fRootNode->appendNode(n);
  }

  return NULL;
}


//------------------------------------------------------------------------------

void
SecondPass::parseTypeVector(TypeVector* generics, const Token& expr,
                            bool forceOpenType)
{
  hr_assert(expr.isNested());

  for (size_t i = 0; i < expr.children().size(); i++) {
    if (expr[i] == kComma)
      continue;
    Type ty = parseTypeSpec(expr[i], forceOpenType);
    if (ty.isDef())
      generics->push_back(ty);
  }
}


Type
SecondPass::parseTypeSpec(const Token& expr, bool forceOpenType)
{
  Type ty = parseTypeSpecImpl(expr, forceOpenType);
  if (forceOpenType)
    return ty;
  return fScope->normalizeType(ty);
}


Type
SecondPass::parseBinaryTypeSpec(const Token& expr, bool forceGeneric,
                                bool isValue)
{
  hr_assert(expr.count() == 3);
  hr_assert(expr[0] == kSymbol);

  TypeVector dummyGenerics;
  TypeConstVector constraints;
  bool isGeneric = (fCurrentGenericTypes.find(expr[0].idValue())
                    != fCurrentGenericTypes.end());

  if (expr[1] == kIsa) {
    Type rightType = parseTypeSpec(expr[2]);
    if (!rightType.isValueType()) {
      errorf(expr[1].srcpos(), E_InheritsRefType,
             "isa-constraints must not be reference types. Ignored");
      rightType.setIsValueType(true);
    }

    constraints.push_back(TypeConstraint::newType(kConstOp_isa,
                                                  rightType));

    if (isGeneric || forceGeneric)
      return Type::newTypeRef(expr[0].idValue(), K(isOpen),
                              constraints, isValue);
    else
      return Type::newTypeRef(expr[0].idValue(),
                              dummyGenerics, constraints, isValue);
  }

  TypeConstOperator op = kConstOp_equal;
  if (expr[1] == kIn)
    op = kConstOp_in;
  else if (expr[1] == kEqual)
    op = kConstOp_equal;
  else if (expr[1] == kUnequal)
    op = kConstOp_notEqual;
  else if (expr[1] == kLess)
    op = kConstOp_less;
  else if (expr[1] == kLessEqual)
    op = kConstOp_lessEqual;
  else if (expr[1] == kGreater)
    op = kConstOp_greater;
  else if (expr[1] == kGreaterEqual)
    op = kConstOp_greaterEqual;
  else
    hr_invalid("");

  constraints.push_back(TypeConstraint::newValue(op, expr[2]));
  if (isGeneric || forceGeneric)
    return Type::newTypeRef(expr[0].idValue(), K(isOpen),
                            constraints, isValue);

  return Type::newTypeRef(expr[0].idValue(),
                          dummyGenerics, constraints, isValue);
}


Type
SecondPass::genericTypeRef(const String& id, bool isValue) const
{
  TSharedGenericTable::const_iterator it = fSharedGenericTable.find(id);
  if (it != fSharedGenericTable.end())
    return it->second.clone().setIsValueType(isValue);

  TypeConstVector dummyConstraints;
  return Type::newTypeRef(id, K(isOpen), dummyConstraints, isValue);
}


Type
SecondPass::rephraseRefType(const SrcPos& srcpos, const Type& inType, bool isValue)
{
  if (!isValue) {
    if (!inType.isValueType())
      warning(srcpos, k_DoubleRefType,
              String("Double reference notation on singleton type group is ignored"));
    else
      return inType.clone().setIsValueType(isValue);
  }
  return inType;
}


Type
SecondPass::parseGroupType(const Token& expr, bool isValue)
{
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kParanOpen || expr.leftToken() == kUnionOpen);

  if (expr.children().size() == 1)
    return rephraseRefType(expr.srcpos(), parseTypeSpec(expr[0]), isValue);

  // seq
  TypeVector tyvect;
  parseTypeVector(&tyvect, expr);
  if (tyvect.empty()) {
    errorf(expr.srcpos(), E_EmptySeqType, "Empty sequence type.");
    return Type();
  }

  bool firstIsValue = tyvect.begin()->isValueType();
  for (TypeVector::iterator it = tyvect.begin(); it != tyvect.end(); it++) {
    if (it->isValueType() != firstIsValue) {
      errorf(expr.srcpos(), E_MixedValueType, "Group type with mixed value types");
      return Type();
    }
  }

  if (!isValue && firstIsValue == isValue) {
    warning(expr.srcpos(), k_DoubleRefType,
            String("Double reference notation on group type is ignored"));
    for (TypeVector::iterator it = tyvect.begin(); it != tyvect.end(); it++)
      it->setIsValueType(true);
  }

  if (expr.leftToken() == kParanOpen)
    return Type::newSeq(tyvect, isValue);
  else
    return Type::newUnion(tyvect, isValue);
}


Type
SecondPass::parseTypeSpecImpl(const Token& expr, bool forceOpenType)
{
  if (expr.isSeq() && expr[0] == kReference) {
    hr_assert(expr.count() == 2);
    hr_assert(!forceOpenType);

    return parseTypeSpecImpl2(expr[1], !K(isValue), forceOpenType);
  }

  return parseTypeSpecImpl2(expr, K(isValue), forceOpenType);
}


Type
SecondPass::parseTypeSpecImpl2(const Token& expr, bool isValue, bool forceOpenType)
{
  if (expr == kSymbol) {
    if (fCurrentGenericTypes.find(expr.idValue()) != fCurrentGenericTypes.end())
      return genericTypeRef(expr.idValue(), isValue);
    else if (forceOpenType)
      return Type::newTypeRef(expr.idValue(), K(isOpen), TypeConstVector(), isValue);
    else
      return Type::newTypeRef(expr.idValue(), isValue);
  }
  else if (expr.isSeq()) {
    if (expr.count() == 2) {
      if (forceOpenType) {
        errorf(expr.srcpos(), E_BadGenericType, "Unexpected generic type notation");
        return Type();
      }

      if (expr[0] == kSymbol &&
          expr[1].isNested() && expr[1].leftToken() == kGenericOpen)
      {
        // identifier with generic arguments
        if (fCurrentGenericTypes.find(expr[0].idValue()) != fCurrentGenericTypes.end())
          errorf(expr[0].srcpos(), E_SuperGenericType,
                 "Generic type reference '%s' with parameters",
                 (const char*)StrHelper(expr[0].idValue()));

        TypeVector generics;
        TypeConstVector dummyConstraints;
        parseTypeVector(&generics, expr[1]);
        return Type::newTypeRef(expr[0].idValue(), generics, dummyConstraints,
                                isValue);
      }
      else if (expr[0] == kFUNCTIONId &&
               expr[1].isNested() && expr[1].leftToken() == kParanOpen)
      {
        if (isValue)
          warning(expr.srcpos(), E_RefToFunc,
                  String("References to function types have no effect.  Ignored"));

        NodeList defaultApplyParams;
        parseParameters(&defaultApplyParams, expr[1].children());

        FunctionParamVector funcParams;
        paramsNodeListToType(&funcParams, defaultApplyParams);

        FunctionSignature sign(!K(isGeneric), String(), Type(), funcParams);
        return Type::newFunction(sign);
      }
      else if (expr.count() == 2 &&
               expr[1].isNested() && expr[1].leftToken() == kBracketOpen)
      {
        // array
        Type baseType = parseTypeSpec(expr[0]);

        int sizeInd = 0;
        if (expr[1].count() > 0) {
          TokenEvalContext ctx(fCompiler->configVarRegistry());
          Token p = ctx.evalToken(expr[1][0]);
          if (p.isInt()) {
            sizeInd = p.intValue();
          }
          else {
            errorf(expr[1][0].srcpos(), E_InvalidArraySize,
                   "array size expression did not evaluate to integer. Treat it as 0");
          }
        }

        if (baseType.isArray())
        {
          errorf(expr.srcpos(), E_MultiDimenArray,
                 "Multi-dimensional array types are not defined");
          return baseType;
        }

        return Type::newArray(baseType, sizeInd, isValue);
      }
      else if (expr[0] == kQuote) {
        hr_assert(expr[1] == kSymbol);
        return genericTypeRef(expr[1].idValue(), isValue);
      }
      else
        hr_invalid("");
    }
    else if (expr.count() == 3) {
      return parseBinaryTypeSpec(expr, !K(forceGeneric), isValue);
    }
    else if (expr.count() == 4) {
      if (expr[0] == kFUNCTIONId &&
          expr[1].isNested() && expr[1].leftToken() == kParanOpen)
      {
        hr_assert(expr[2] == kColon);

        if (!isValue)
          warning(expr.srcpos(), E_RefToFunc,
                  String("References to function types have no effect.  Ignored"));

        NodeList defaultApplyParams;
        parseParameters(&defaultApplyParams, expr[1].children());

        FunctionParamVector funcParams;
        paramsNodeListToType(&funcParams, defaultApplyParams);

        Type retType = parseTypeSpec(expr[3]);

        FunctionSignature sign(!K(isGeneric), String(), retType, funcParams);
        return Type::newFunction(sign);
      }
    }
    else
      hr_invalid("");
  }
  else if (expr.isNested()) {
    hr_assert(!forceOpenType);
    return parseGroupType(expr, isValue);
  }
  else
    hr_invalid("");

  return Type();
}


//------------------------------------------------------------------------------

void
SecondPass::paramsNodeListToType(FunctionParamVector* funcParams,
                                 const NodeList& nl) const
{
  for (size_t i = 0; i < nl.size(); i++) {
    const ParamNode* pnd = dynamic_cast<const ParamNode*>(nl[i].obj());
    if (pnd != NULL) {
      switch (pnd->flags()) {
      case kPosArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamPos,
                            !K(isSpec), String(), pnd->type()));
        break;
      case kSpecArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamPos,
                            K(isSpec), String(), pnd->type()));
        break;
      case kNamedArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamNamed,
                            !K(isSpec), pnd->key(), pnd->type()));
        break;
      case kRestArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamRest,
                            !K(isSpec), String(), pnd->type()));
        break;
      }
    }
  }
}


Type
SecondPass::parseWhereConstraint(const Token& whereConstrSeq)
{
  hr_assert(whereConstrSeq.isSeq());
  hr_assert(whereConstrSeq.count() == 3);
  hr_assert(whereConstrSeq[0] == kSymbol);

  return parseBinaryTypeSpec(whereConstrSeq, K(forceGeneric), K(isValue));
}


void
SecondPass::parseWhereClause(const Token& whereSeq)
{
  const TokenVector& whereClause = whereSeq.children();
  hr_assert(whereClause[0] == kWhereId);
  hr_assert(whereClause.size() > 1);

  for (size_t i = 1; i < whereClause.size(); i++) {
    if (whereClause[i] == kComma)
      continue;
    Type ty = parseWhereConstraint(whereClause[i]);

    TSharedGenericTable::iterator it = fSharedGenericTable.find(ty.typeName());
    if (it != fSharedGenericTable.end())
      it->second = ty;
    else
      fSharedGenericTable.insert(std::make_pair(ty.typeName(), ty));
  }
}


size_t
SecondPass::getWhereOfs(const TokenVector& seq, size_t ofs) const
{
  for (size_t i = ofs; i < seq.size(); i++) {
    if (seq[i].isSeq() && seq[i].count() > 1 &&
        seq[i][0] == kWhereId)
    {
      hr_assert(i > ofs);
      return i;
    }
  }

  return 0;
}


SecondPass::PrimeTuple
SecondPass::parsePrime(const Token& primeToken)
{
  PrimeTuple result;

  if (primeToken.isSeq() &&
      primeToken.count() == 2 &&
      primeToken[1].isNested() &&
      primeToken[1].leftToken() == kParanOpen)
  {
    String primeName;
    if (primeToken[0] == kSymbol) {
      primeName = primeToken[0].idValue();
    }
    else if (primeToken[0].isSeq() && primeToken[0].count() == 2) {
      if (primeToken[0][0] == kSymbol) {
        if (primeToken[0][1].isNested() && primeToken[0][1].leftToken() == kGenericOpen)
        {
          primeName = primeToken[0][0].idValue();
        }
        else {
          errorf(primeToken[0][1].srcpos(), E_GenericTypeList,
                 "Expected '<' for generic class prime expression");
          return result;
        }
      }
      else {
        errorf(primeToken[0][0].srcpos(), E_SymbolExpected,
               "Expected generic class prime expression");
        return result;
      }
    }
    else {
      errorf(primeToken[0][0].srcpos(), E_SymbolExpected,
             "Expected generic class prime expression");
      return result;
    }

    if (primeToken[1].isNested() && primeToken[1].leftToken() == kParanOpen)
    {
      hr_assert(primeToken[1].rightToken() == kParanClose);
      //NodeList args = parseFunCallArgs(primeToken[1].children());

      Type referedType = fScope->lookupType(primeName, K(showAmbiguousSymDef));
      if (!referedType.isDef())
        referedType = Type::newTypeRef(primeName, K(isValue));

      result.fType = referedType;
      result.fPrime = generateInitObjectCall(primeToken.srcpos(),
                                             new SymbolNode(primeToken.srcpos(),
                                                            String("self")),
                                             referedType, primeToken[1].children());
      return result;
    }
    else {
      errorf(primeToken[1].srcpos(), E_MissingParanOpen,
             "Expected generic class prime expression");
      return result;
    }
  }
  else {
    errorf(primeToken.srcpos(), E_BadClassOnAlloc,
           "Unexpected 'on alloc' specifications");
    return result;
  }

  return result;
}


std::vector<SecondPass::PrimeTuple>
SecondPass::parseOnAllocExpr(const Token& expr)
{
  hr_assert(expr.count() == 4);
  hr_assert(expr[0] == kOnId);
  hr_assert(expr[1] == Compiler::allocToken);
  hr_assert(expr[2].isNested());

  if (expr[2].count() > 0) {
    warningf(expr[2].srcpos(), E_BadFunctionArity,
             "'on alloc' takes no parameters");
  }

  std::vector<PrimeTuple> primeTuples;
  if (expr[3].isNested() && expr[3].leftToken() == kBraceOpen) {
    const TokenVector& primeTokens = expr[3].children();

    for (size_t i = 0; i < primeTokens.size(); i++) {
      PrimeTuple tuple = parsePrime(primeTokens[i]);
      if (tuple.fPrime != NULL)
        primeTuples.push_back(tuple);
    }
  }
  else {
    PrimeTuple tuple = parsePrime(expr[3]);
    if (tuple.fPrime != NULL)
      primeTuples.push_back(tuple);
  }

  return primeTuples;
}


size_t
SecondPass::getWhereOfs(const Token& expr) const
{
  return getWhereOfs(expr.children(), 0);
}


NodeList
SecondPass::parseTypeDef(const Token& expr, size_t ofs, bool isClass,
                         bool isLocal)
{
  hr_assert(fCurrentGenericTypes.empty());
  TSharedGenericScopeHelper SharedTable(fSharedGenericTable);

  hr_assert(expr.isSeq());
  hr_assert(expr.count() >= ofs + 2);
  hr_assert(expr[ofs] == Compiler::typeToken || expr[ofs] == Compiler::classToken);
  hr_assert(expr[ofs + 1] == kSymbol);

  ofs++;

  const TokenVector& seq = expr.children();
  String typeName = seq[ofs].idValue();
  String fullTypeName = qualifyId(currentModuleName(), typeName);

  ofs++;

  size_t whereOfs = ofs;
  if ((whereOfs = getWhereOfs(expr)) >= ofs)
    parseWhereClause(seq[whereOfs]);

  TypeVector generics;
  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kGenericOpen)
  {
    // type parameters
    parseTypeVector(&generics, expr[ofs], K(forceOpenType));

    for (size_t i = 0; i < generics.size(); i++) {
      hr_assert(generics[i].isRef());
      fCurrentGenericTypes.insert(generics[i].typeName());
    }
    ofs++;
  }

  NodeList defaultApplyParams;
  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kParanOpen)
  {
    // default apply signature
    hr_assert(isClass);

    parseParameters(&defaultApplyParams, seq[ofs].children());
    ofs++;
  }

  Type inheritsFrom;
  if (ofs + 1 < seq.size() && seq[ofs] == kColon) {
    // inheritance type spec
    inheritsFrom = parseTypeSpec(seq[ofs + 1]);

    if (!inheritsFrom.isValueType()) {
      errorf(seq[ofs + 1].srcpos(), E_InheritsRefType,
             "Can't inherit from reference type.  Reference ignored.");
      inheritsFrom.setIsValueType(true);
    }

    ofs += 2;
  }

  if (ofs == whereOfs)
    ofs++;

  std::vector<PrimeTuple> primeTuples;
  NodeList slotDefs;
  NodeList onExprs;
  TypeSlotList slotTypes;

  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kBraceOpen)
  {
    const TokenVector& defs = seq[ofs].children();

    for (size_t i = 0; i < defs.size(); i++) {
      hr_assert(defs[i].isSeq() && defs[i].count() > 1);
      hr_assert(defs[i][0] == kDefId ||
                defs[i][0] == kOnId ||
                defs[i][0] == kExtendId);

      if (defs[i][0] == kDefId) {
        if (defs[i][1] == Compiler::slotToken) {
          hr_assert(isClass);

          NodeList nl = parseExpr(defs[i]);
          appendNodes(slotDefs, nl);

          for (size_t i = 0; i < slotDefs.size(); i++) {
            if (slotDefs[i] != NULL) {
              const BaseDefNode* basedef =
                dynamic_cast<const BaseDefNode*>(slotDefs[i].obj());
              hr_assert(basedef != NULL);

              const SlotdefNode* slotDef =
                dynamic_cast<const SlotdefNode*>(basedef->defNode());
              hr_assert(slotDef != NULL);

              if (slotDef != NULL) {
                slotTypes.push_back(TypeSlot(slotDef->name(),
                                             slotDef->type(),
                                             slotDef->flags()));
              }
            }
          }
        }
        else {
          errorf(defs[i].srcpos(), E_UnexpectedDefExpr,
                 "Unexpected definition in type body");
        }
      }
      else if (defs[i][0] == kOnId) {
        if (!isClass) {
          errorf(defs[i].srcpos(), E_OnExprInType,
                 "Unexpected on expression in type body");
        }
        else if (defs[i].count() > 1 && defs[i][1] == Compiler::allocToken) {
          std::vector<PrimeTuple> tuples = parseOnAllocExpr(defs[i]);
          primeTuples.insert(primeTuples.begin(),
                             tuples.begin(), tuples.end());
        }
        else if (defs[i].count() > 1 && defs[i][1] == Compiler::initToken) {
          NodeList nl = parseExpr(defs[i]);
          if (!nl.empty()) {
            if (OnNode* onNode = dynamic_cast<OnNode*>(nl[0].obj())) {
              if (onNode->params().size() != 1) {
                errorf(onNode->srcpos(), E_BadFunctionArity,
                       "wrong number of parameters to 'on init' hook.  Expected 1, found %d",
                       onNode->params().size());
              }
              else if (ParamNode* param = dynamic_cast<ParamNode*>(onNode->params()[0].obj())) {
                if (!param->isPositional()) {
                  errorf(param->srcpos(), E_BadFunctionArity,
                         "non-positional parameter detected in 'on init' hook");
                }
                else
                  appendNodes(onExprs, nl);
              }
              else
                appendNodes(onExprs, nl);
            }
          }
        }
        else {
          NodeList nl = parseExpr(defs[i]);
          appendNodes(onExprs, nl);
        }
      }
      else {
        errorf(defs[i].srcpos(), E_UnexpectedDefExpr,
               "Unexpected expression in type body");
      }
    }

    ofs++;
  }

  std::vector<PrimeTuple> primes;
  for (size_t i = 0; i < primeTuples.size(); i++) {
    if (inheritsFrom.isSequence()) {
      if (!inheritsFrom.containsType(primeTuples[i].fType)) {
        errorf(primeTuples[i].fPrime->srcpos(), E_BadClassOnAlloc,
               "Super class initialization for unknown type %s",
               (const char*)StrHelper(primeTuples[i].fType.typeId()));
      }
      else {
        primes.push_back(primeTuples[i]);
      }
    }
    else if (inheritsFrom.isDef()) {
      if (inheritsFrom.typeName() != primeTuples[i].fType.typeName()) {
        errorf(primeTuples[i].fPrime->srcpos(), E_BadClassOnAlloc,
               "Super class initialization for unknown type %s",
               (const char*)StrHelper(primeTuples[i].fType.typeId()));
      }
      else {
        primes.push_back(primeTuples[i]);
      }
    }
  }

  Type defType;
  if (isClass) {
    FunctionParamVector funcParams;
    paramsNodeListToType(&funcParams, defaultApplyParams);

    String ctorFuncName = qualifyId(fullTypeName, Names::kInitFuncName);
    FunctionSignature sign = FunctionSignature(!K(isGeneric),
                                               ctorFuncName, // func name
                                               // rettype
                                               Type::newTypeRef(fullTypeName,
                                                                generics, K(isValue)),
                                               funcParams);

    TypeVector genGenerics;
    for (size_t i = 0; i < generics.size(); i++) {
      hr_assert(generics[i].isRef());
      genGenerics.push_back(genericTypeRef(generics[i].typeName(), K(isValue)));
    }

    defType = Type::newClass(fullTypeName, generics, inheritsFrom, sign,
                             slotTypes);
  }
  else {
    defType = Type::newType(fullTypeName, generics, inheritsFrom);
  }


  fCurrentGenericTypes.clear();

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullTypeName))
    return NodeList();

  fScope->registerType(expr.srcpos(), fullTypeName, defType);

  NodeList result;

  result.push_back(newDefNode(new TypeDefNode(expr.srcpos(),
                                              fullTypeName,
                                              isClass,
                                              defType,
                                              defaultApplyParams,
                                              slotDefs,
                                              onExprs),
                              isLocal));

  if (isClass) {
    Ptr<AptNode> ctor = generateConstructor(expr, fullTypeName, defType,
                                            defaultApplyParams,
                                            slotDefs,
                                            primes,
                                            onExprs);
    result.push_back(ctor);
  }

  return result;
}


AptNode*
SecondPass::defaultSlotInitValue(const SlotdefNode* slot)
{
  if (slot->initExpr() != NULL)
    return slot->initExpr()->clone();
  else if (slot->type().isDef()) {
    if (slot->type().isArray()) {
      AptNode* node = new ArrayNode(slot->srcpos());
      node->setType(slot->type());
      return node;
    }
    else {
      if (slot->type().isAnyInt())
        return new IntNode(slot->srcpos(), 0, slot->type().isImaginary(),
                           slot->type());
      else if (slot->type().isAnyFloat())
        return new RealNode(slot->srcpos(), 0, slot->type().isImaginary(),
                            slot->type());
      else if (slot->type().isRational())
        return new RationalNode(slot->srcpos(), Rational(0, 1),
                                slot->type().isImaginary(), slot->type());
      else if (slot->type().isString())
        return new StringNode(slot->srcpos(), String());
      else if (slot->type().isBool())
        return new BoolNode(slot->srcpos(), false);
      else if (slot->type().isChar())
        return new CharNode(slot->srcpos(), Char(0));
      else if (slot->type().isKeyword())
        return new KeywordNode(slot->srcpos(), String());
    }
  }

  // TODO

  return new SymbolNode(slot->srcpos(), Names::kLangNil);
}


static String
findAutoParamName(const NodeList& params, const String& slotName)
{
  String resultingSlotName = slotName;

  for (size_t i = 0; i < params.size(); i++) {
    const ParamNode* prm = dynamic_cast<const ParamNode*>(params[i].obj());
    if (prm->name() == resultingSlotName) {
      warningf(prm->srcpos(), E_CtorArgNameConflict,
               "conflict names in class init and auto slot configuration: %s",
               (const char*)StrHelper(slotName));
      resultingSlotName = resultingSlotName + "-1";
    }
  }

  return resultingSlotName;
}


static void
insertKeyedArg(NodeList& params, ParamNode* prm)
{
  for (size_t i = 0; i < params.size(); i++) {
    ParamNode* nl = dynamic_cast<ParamNode*>(params[i].obj());
    if (nl->isRestArg()) {
      params.insert(params.begin() + i, prm);
      return;
    }
  }

  params.push_back(prm);
}


AptNode*
SecondPass::generateConstructor(const Token& typeExpr,
                                const String& fullTypeName,
                                const Type& defType,
                                const NodeList& defaultApplyParams,
                                const NodeList& slotDefs,
                                const std::vector<PrimeTuple>& primes,
                                const NodeList& onExprs)
{
  const SrcPos& srcpos = typeExpr.srcpos();

  String ctorFuncName = qualifyId(fullTypeName, Names::kInitFuncName);
  String selfParamSym = uniqueName("obj");

  NodeList params = copyNodes(defaultApplyParams);
  params.insert(params.begin(),
                new ParamNode(srcpos, String(), selfParamSym,
                              kPosArg, defType, NULL));

  Ptr<ListNode> body = new BlockNode(srcpos);

  generatePrimeInits(srcpos, body, defType, primes, selfParamSym);

  // initialize slots
  for (unsigned int i = 0; i < slotDefs.size(); i++) {
    const BaseDefNode* basedef = dynamic_cast<const BaseDefNode*>(slotDefs[i].obj());
    hr_assert(basedef != NULL);

    const SlotdefNode* slot = dynamic_cast<const SlotdefNode*>(basedef->defNode());
    hr_assert(slot != NULL);

    if (slot->isAuto()) {
      String autoPrmName = findAutoParamName(params, slot->name());

      Ptr<ParamNode> prm = new ParamNode(srcpos, slot->name(), autoPrmName,
                                         kNamedArg, slot->type(),
                                         defaultSlotInitValue(slot));

      Ptr<AptNode> slotInit = new AssignNode(srcpos,
                                             new SlotRefNode(srcpos,
                                                             new SymbolNode(srcpos,
                                                                            selfParamSym),
                                                             slot->name()),
                                             new SymbolNode(srcpos, autoPrmName));
      body->appendNode(slotInit);

      insertKeyedArg(params, prm);
    }
    else {
      Ptr<AptNode> slotInit = new AssignNode(srcpos,
                                             new SlotRefNode(srcpos,
                                                             new SymbolNode(srcpos,
                                                                            selfParamSym),
                                                             slot->name()),
                                             defaultSlotInitValue(slot));
      body->appendNode(slotInit);
    }
  }

  // inline a possible on init expr.
  for (unsigned int i = 0; i < onExprs.size(); i++) {
    const OnNode* onNode = dynamic_cast<const OnNode*>(onExprs[i].obj());
    if (onNode != NULL && onNode->key() == Compiler::initToken.idValue()) {
      NodeList onNodeParams = copyNodes(onNode->params());
      hr_assert(onNodeParams.size() == 1);
      if (!onNodeParams[0]->type().isDef() ||
          onNodeParams[0]->type().isAny())
      {
        onNodeParams[0]->setType(defType);
      }

      Ptr<AptNode> func = new FunctionNode(srcpos, onNodeParams,
                                           Type(),
                                           onNode->body()->clone());
      Ptr<ApplyNode> initCall = new ApplyNode(srcpos, func);
      initCall->appendNode(new SymbolNode(srcpos, selfParamSym));

      body->appendNode(initCall);
    }
  }

  body->appendNode(new SymbolNode(srcpos, selfParamSym));


  // register constructor function
  Ptr<FuncDefNode> ctorFunc = new FuncDefNode(srcpos,
                                              ctorFuncName,
                                              0, // flags
                                              params,
                                              defType,
                                              body);
  fScope->registerFunction(typeExpr.srcpos(), ctorFuncName, ctorFunc);

  fScope->attachSymbolForExport(Scope::kNormal, fullTypeName, ctorFuncName);

  return newDefNode(ctorFunc.release(), !K(isLocal));
}


struct ReqTypeInitTuple
{
  Type fType;
  bool fReqExplicitPrime;
  bool fIsClass;
};


static ReqTypeInitTuple
reqTypeInitTupleForType(const Type& type, std::shared_ptr<Scope> scope)
{
  Type superType = scope->lookupType(type.typeName(), K(showAmbiguousSymDef));
  if (!superType.isDef()) {
    errorf(SrcPos(), E_UnknownType, "Unknown super type: %s",
           (const char*)StrHelper(type.typeId()));
  }
  else if (superType.isClass()) {
    ReqTypeInitTuple tuple;
    tuple.fType = type;
    tuple.fIsClass = true;
    tuple.fReqExplicitPrime = superType.applySignature().hasPositionalParam();
    return tuple;
  }
  else {
    ReqTypeInitTuple tuple;
    tuple.fType = type;
    tuple.fIsClass = false;
    tuple.fReqExplicitPrime = false;
    return tuple;
  }

  return ReqTypeInitTuple();
}


static std::vector<ReqTypeInitTuple>
getDirectInheritedTypes(const Type& defType, std::shared_ptr<Scope> scope)
{
  std::vector<ReqTypeInitTuple> reqTypeInits;

  if (defType.typeInheritance().isDef()) {
    if (defType.typeInheritance().isSequence()) {
      const TypeVector& inheritedTypes = defType.typeInheritance().seqTypes();

      for (size_t i = 0; i < inheritedTypes.size(); i++) {
        ReqTypeInitTuple tuple = reqTypeInitTupleForType(inheritedTypes[i], scope);
        if (tuple.fType.isDef())
          reqTypeInits.push_back(tuple);
      }
    }
    else {
      ReqTypeInitTuple tuple = reqTypeInitTupleForType(defType.typeInheritance(), scope);
      if (tuple.fType.isDef())
        reqTypeInits.push_back(tuple);
    }
  }

  return reqTypeInits;
}


AptNode*
SecondPass::findPrimeForType(const Type& reqTypeInit,
                             const std::vector<PrimeTuple>& primes)
{
  String reqTypeId = reqTypeInit.typeId();

  // call prime functions of super classes.  Before that replace the arguments
  // to these functions with selfParamSym.
  for (size_t i = 0; i < primes.size(); i++) {
    if (primes[i].fType.typeId() == reqTypeId)
      return primes[i].fPrime;
  }

  return NULL;
}


AptNode*
SecondPass::getPrimeForType(const Type& reqTypeInit,
                            const std::vector<PrimeTuple>& primes,
                            const String& selfParamSym)
{
  Ptr<AptNode> prime0 = findPrimeForType(reqTypeInit, primes);
  if (prime0 != NULL) {
    Ptr<AptNode> prime = prime0->clone();
    ApplyNode* apply = dynamic_cast<ApplyNode*>(prime.obj());
    hr_assert(apply != NULL);
    hr_assert(apply->children().size() > 0);

    apply->children()[0] = new SymbolNode(apply->children()[0]->srcpos(),
                                          selfParamSym);
    return prime.release();
  }

  return NULL;
}


void
SecondPass::generatePrimeInits(const SrcPos& srcpos,
                               ListNode* body,
                               const Type& defType,
                               const std::vector<PrimeTuple>& primes,
                               const String& selfParamSym)
{
  std::vector<ReqTypeInitTuple> reqTypeInits = getDirectInheritedTypes(defType, fScope);

  for (size_t i = 0; i < reqTypeInits.size(); i++) {
    // does the super type requires an explicit prime?
    if (reqTypeInits[i].fIsClass) {
      if (reqTypeInits[i].fReqExplicitPrime) {
        Ptr<AptNode> apply = getPrimeForType(reqTypeInits[i].fType,
                                             primes,
                                             selfParamSym);
        if (apply == NULL) {
          errorf(srcpos, E_BadClassOnAlloc,
                 "No 'on alloc' prime call for super class '%s'",
                 (const char*)StrHelper(reqTypeInits[i].fType.typeId()));
        }
        else
          body->appendNode(apply);
      }
      else {
        Ptr<AptNode> apply = getPrimeForType(reqTypeInits[i].fType,
                                             primes,
                                             selfParamSym);
        if (apply == NULL)
          apply = generateInitObjectCall(SrcPos(),
                                         new SymbolNode(SrcPos(), selfParamSym),
                                         reqTypeInits[i].fType, TokenVector());
        body->appendNode(apply);
      }
    }
    else if (reqTypeInits[i].fType.isDef()) {
      Ptr<AptNode> prime = findPrimeForType(reqTypeInits[i].fType, primes);
      if (prime != NULL) {
        errorf(srcpos, E_BadClassOnAlloc,
               "Explicit 'on alloc' prime call for non allocable type '%s'",
               (const char*)StrHelper(reqTypeInits[i].fType.typeId()));
      }
    }
  }
}


AptNode*
SecondPass::parseAliasDef(const Token& expr, size_t ofs, bool isLocal)
{
  hr_assert(fCurrentGenericTypes.empty());
  TSharedGenericScopeHelper SharedTable(fSharedGenericTable);

  hr_assert(expr.isSeq());
  hr_assert(expr.count() > ofs + 3);
  hr_assert(expr[ofs] == Compiler::aliasToken);
  hr_assert(expr[ofs + 1] == kSymbol);

  ofs++;

  const TokenVector& seq = expr.children();
  String aliasName = seq[ofs].idValue();
  if (isLocal && isQualified(aliasName)) {
    errorf(seq[ofs].srcpos(), E_QualifiedLocalSym,
           "Local symbol in definition must not be qualified.  "
           "Ignore namespace");
    aliasName = baseName(aliasName);
  }
  ofs++;

  size_t whereOfs = ofs;
  if ((whereOfs = getWhereOfs(expr)) >= ofs)
    parseWhereClause(seq[whereOfs]);

  TypeVector generics;
  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kGenericOpen)
  {
    // type parameters
    parseTypeVector(&generics, expr[ofs]);

    for (size_t i = 0; i < generics.size(); i++) {
      hr_assert(generics[i].isRef());
      fCurrentGenericTypes.insert(generics[i].typeName());
    }
    ofs++;
  }

  Type referedType;
  if (ofs + 1 < seq.size() && seq[ofs] == kAssign) {
    referedType = parseTypeSpec(seq[ofs + 1]);
    ofs += 2;
  }

  if (ofs == whereOfs)
    ofs++;

  String fullAliasName = ( isLocal
                           ? aliasName
                           : qualifyId(currentModuleName(), aliasName) );

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullAliasName)) {
    fCurrentGenericTypes.clear();
    return NULL;
  }

  Type aliasType = Type::newAlias(fullAliasName, generics, referedType);

  fScope->registerType(expr.srcpos(), fullAliasName, aliasType);

  fCurrentGenericTypes.clear();

  return NULL;
}


AptNode*
SecondPass::parseSlotDef(const Token& expr, size_t ofs)
{
  hr_assert(expr.isSeq());
  hr_assert(expr.count() >= ofs + 2);
  hr_assert(expr[ofs] == Compiler::slotToken);

  ofs++;

  const TokenVector& seq = expr.children();
  String slotName = seq[ofs].idValue();
  if (isQualified(slotName)) {
    errorf(seq[ofs].srcpos(), E_QualifiedSlot,
           "Slotnames must not be qualified.  Ignore namespace");
    slotName = baseName(slotName);
  }
  ofs++;

  Type slotType;
  if (ofs + 1 < seq.size() && seq[ofs] == kColon) {
    slotType = parseTypeSpec(seq[ofs + 1]);
    ofs += 2;
  }

  Ptr<AptNode> initExpr;
  if (ofs + 1 < seq.size() && seq[ofs] == kAssign) {
    NodeList nl = parseExpr(seq[ofs + 1]);
    initExpr = singletonNodeListOrNull(nl);
    ofs += 2;
  }

  unsigned int slotFlags = kSimpleSlot;
  if (ofs < seq.size() && seq[ofs] == kComma) {
    ofs++;
    for ( ; ofs < seq.size(); ofs++) {
      if (seq[ofs] == kComma)
        continue;
      if (seq[ofs] == Compiler::publicToken) {
        slotFlags |= kPublicSlot;
      }
      else if (seq[ofs] == Compiler::outerToken) {
        slotFlags |= kOuterSlot;
      }
      else if (seq[ofs] == Compiler::innerToken) {
        slotFlags |= kInnerSlot;
      }
      else if (seq[ofs] == Compiler::transientToken) {
        slotFlags |= kTransientSlot;
      }
      else if (seq[ofs] == Compiler::readonlyToken) {
        slotFlags |= kReadonlySlot;
      }
      else if (seq[ofs] == Compiler::autoToken) {
        slotFlags |= kAutoSlot;
      }
      else {
        hr_assert(seq[ofs] == kSymbol);
        errorf(seq[ofs].srcpos(), E_UnknownSlotFlag,
               "Unknown slot flag '%s' ignored",
               (const char*)StrHelper(seq[ofs].toString()));
      }
    }
  }

  return new SlotdefNode(expr.srcpos(), slotName, slotFlags, slotType,
                         initExpr);
}


AptNode*
SecondPass::nextEnumInitValue(const SrcPos& srcpos,
                              const Token& enumItemSym,
                              const Type& baseType, Token& lastInitToken)
{
  Ptr<AptNode> initExpr;

  Ptr<TypeEnumMaker> maker = baseType.newBaseTypeEnumMaker();
  if (maker != NULL) {
    lastInitToken = maker->nextEnumItem(srcpos, enumItemSym, lastInitToken);
    if (lastInitToken.isSet())
      initExpr = singletonNodeListOrNull(parseExpr(lastInitToken));
  }
  else {
    errorf(srcpos, E_EnumNotBaseType, "Enum init value is not a base type");
    tyerror(baseType, "Enum Basetype");
  }

  return initExpr.release();
}


AptNode*
SecondPass::parseEnumDef(const Token& expr, size_t ofs, bool isLocal)
{
  hr_assert(fCurrentGenericTypes.empty());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == ofs + 3 || expr.count() == ofs + 5);
  hr_assert(expr[ofs] == Compiler::enumToken);
  hr_assert(expr[ofs + 1] == kSymbol);

  ofs++;

  const TokenVector& seq = expr.children();
  String enumTypeName = seq[ofs].idValue();
  String fullEnumName = ( isLocal
                          ? enumTypeName
                          : qualifyId(currentModuleName(), enumTypeName) );
  ofs++;

  Type baseType;
  if (ofs + 1 < seq.size() && seq[ofs] == kColon) {
    baseType = parseTypeSpec(seq[ofs + 1]);

    if (!baseType.isValueType()) {
      errorf(seq[ofs + 1].srcpos(), E_InheritsRefType,
             "Can't inherit from reference type.  Reference ignored.");
      baseType.setIsValueType(true);
    }

    ofs += 2;
  }
  else
    baseType = Type::newInt32();

  if (!baseType.isBaseType()) {
    errorf(expr.srcpos(), E_EnumNotBaseType, "Enum base is not a base type.");
    return NULL;
  }

  hr_assert(expr[ofs].isNested());
  hr_assert(expr[ofs].leftToken() == kBraceOpen);

  fCurrentGenericTypes.clear();

  //-------- define the items as def const x = y

  Token lastInitToken;
  SrcPos lastInitPos;
  const TokenVector& enumValues = expr[ofs].children();
  for (size_t i = 0; i < enumValues.size(); i++) {
    const Token& enumVal = enumValues[i];

    String sym;
    Ptr<AptNode> initExpr;

    if (enumVal.isSeq()) {
      hr_assert(enumVal.count() >= 3);
      hr_assert(enumVal[0] == kSymbol);

      sym = enumVal[0].idValue();

      hr_assert(enumVal[1] == kAssign);
      initExpr = singletonNodeListOrNull(parseExpr(enumVal[2]));
      lastInitToken = enumVal[2];
      lastInitPos = enumVal[2].srcpos();
    }
    else if (enumVal == kSymbol) {
      sym = enumVal.idValue();
      initExpr = nextEnumInitValue(lastInitPos, enumVal, baseType,
                                   lastInitToken);
    }
    else
      hr_invalid("");

    if (isQualified(sym)) {
      errorf(enumVal.srcpos(), E_QualifiedEnumDefSym,
             "Enum item definitions must not be qualified. "
             "Ignore namespace");
      sym = baseName(sym);
    }

    String fullSymName = ( isLocal
                           ? sym
                           : qualifyId(currentModuleName(), sym) );
    if (fScope->checkForRedefinition(enumVal.srcpos(),
                                     Scope::kNormal, fullSymName))
      return NULL;

    Ptr<AptNode> var = new VardefNode(enumVal.srcpos(),
                                      fullSymName, kEnumVar, isLocal,
                                      baseType, initExpr);
    fScope->registerVar(enumVal.srcpos(), fullSymName, var);

    fScope->attachSymbolForExport(Scope::kNormal, fullEnumName, fullSymName);
  }

  //-------- define the enum type as 'def type X : (Y in ...)'

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullEnumName))
    return NULL;

  // TODO: make the enum type actually a constraint type of the values of the
  // defined items.
  TypeVector dummyGenerics;
  TypeConstVector constraints;
  Type enumType = Type::newTypeRef(baseType.typeName(),
                                   dummyGenerics, constraints, K(isValue));

  fScope->registerType(expr.srcpos(), fullEnumName, enumType);

  // there's no apt node to generate here.
  return NULL;
}


AptNode*
SecondPass::parseMeasureDef(const Token& expr, size_t ofs, bool isLocal)
{
  hr_assert(fCurrentGenericTypes.empty());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == ofs + 5);
  hr_assert(expr[ofs] == Compiler::measureToken);
  hr_assert(expr[ofs + 1] == kSymbol);
  hr_assert(expr[ofs + 2].isNested());
  hr_assert(expr[ofs + 2].count() == 1);
  hr_assert(expr[ofs + 2][0] == kSymbol);
  hr_assert(expr[ofs + 3] == kColon);

  ofs++;

  const TokenVector& seq = expr.children();
  String typeName = seq[ofs].idValue();
  String fullTypeName = qualifyId(currentModuleName(), typeName);
  ofs++;

  String unitName = seq[ofs][0].idValue();
  String fullUnitName = qualifyId(currentModuleName(), unitName);
  ofs++;

  Type isaFrom;
  if (ofs + 1 < seq.size() && seq[ofs] == kColon) {
    isaFrom = parseTypeSpec(seq[ofs + 1]);

    if (!isaFrom.isValueType()) {
      errorf(seq[ofs + 1].srcpos(), E_InheritsRefType,
             "Can't inherit from reference type.  Reference ignored.");
      isaFrom.setIsValueType(true);
    }

    ofs += 2;
  }

  fCurrentGenericTypes.clear();

  Type defMeasureType = Type::newMeasure(fullTypeName, isaFrom, fullUnitName);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullTypeName))
    return NULL;
  fScope->registerType(expr.srcpos(), fullTypeName, defMeasureType);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kUnit, fullUnitName))
    return NULL;
  fScope->registerUnit(expr.srcpos(), fullUnitName, String(),
                       defMeasureType, NULL);

  NodeList dummyApplyParams;
  NodeList dummySlotDefs;
  NodeList dummyOnExprs;

  return new TypeDefNode(expr.srcpos(),
                         fullTypeName,
                         K(isClass),
                         defMeasureType,
                         dummyApplyParams,
                         dummySlotDefs,
                         dummyOnExprs);
}


AptNode*
SecondPass::parseUnitDef(const Token& expr, size_t ofs, bool isLocal)
{
  hr_assert(fCurrentGenericTypes.empty());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() >= ofs + 6);
  hr_assert(expr[ofs] == Compiler::unitToken);
  hr_assert(expr[ofs + 1] == kSymbol);
  hr_assert(expr[ofs + 2] == kMapTo);
  hr_assert(expr[ofs + 4].isNested());
  hr_assert(expr[ofs + 4].count() == 1);

  ofs++;

  const TokenVector& seq = expr.children();
  String unitName = seq[ofs].idValue();
  String fullUnitName = qualifyId(currentModuleName(), unitName);
  ofs++;
  hr_assert(seq[ofs] == kMapTo);
  ofs++;

  TypeUnit baseUnit = fScope->lookupUnit(seq[ofs].idValue(),
                                         K(showAmbiguousSymDef));
  if (!baseUnit.isDef()) {
    error(seq[ofs].srcpos(), E_UndefinedUnit,
          String("Undefined unit: ") + seq[ofs].idValue());
    return NULL;
  }
  ofs++;
  hr_assert(seq[ofs].isNested());

  FundefClauseData data;
  parseFundefClause(seq, ofs, data);

  Ptr<AptNode> funcNode = new FunctionNode(expr.srcpos(), data.fParams,
                                           data.fType, data.fBody);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kUnit, fullUnitName))
    return NULL;
  fScope->registerUnit(expr.srcpos(), fullUnitName, baseUnit.name(),
                       baseUnit.effType(), funcNode);

  return NULL;
}


AptNode*
SecondPass::parseVarDef(const Token& expr, VardefFlags flags, size_t ofs,
                        bool isLocal, const String& linkage)
{
  hr_assert(ofs >= 1);
  hr_assert(ofs < expr.count());
  hr_assert(expr[ofs] == kSymbol);

  const TokenVector& seq = expr.children();
  String sym = seq[ofs].idValue();
  if (isLocal && isQualified(sym)) {
    errorf(expr[ofs].srcpos(), E_QualifiedLocalSym,
           "Local symbol in definition must not be qualified.  "
           "Ignore namespace");
    sym = baseName(sym);
  }
  ofs++;

  Type type;
  if (ofs + 1 < expr.count() && seq[ofs] == kColon) {
    type = parseTypeSpec(seq[ofs + 1]);
    ofs += 2;
  }

  Ptr<AptNode> initExpr;
  if (ofs + 1 < expr.count() && seq[ofs] == kAssign) {
    if (!fCompiler->isParsingInterface() ||
        flags == kConstVar || flags == kConfigVar)
    {
      initExpr = singletonNodeListOrNull(parseExpr(seq[ofs + 1]));
    }
    ofs += 2;
  }

  String fullSymName = ( isLocal
                         ? sym
                         : qualifyId(currentModuleName(), sym) );

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullSymName))
    return NULL;

  Ptr<VardefNode> var = new VardefNode(expr.srcpos(),
                                       fullSymName, flags, isLocal,
                                       type, initExpr);
  var->setLinkage(linkage);
  fScope->registerVar(expr.srcpos(), fullSymName, var);

  return var.release();
}


void
SecondPass::parseFundefClause(const TokenVector& seq, size_t& ofs,
                              FundefClauseData& data)
{
  hr_assert(seq[ofs].isNested());

  ScopeHelper scopeHelper(fScope,
                          !K(doExport), K(isInnerScope),
                          kScopeL_Function);
  TSharedGenericScopeHelper SharedTable(fSharedGenericTable);

  size_t whereOfs = ofs;
  if ((whereOfs = getWhereOfs(seq, ofs)) >= ofs)
    parseWhereClause(seq[whereOfs]);

  parseParameters(&data.fParams, seq[ofs].children());
  ofs++;

  if (ofs < seq.size()) {
    if (seq[ofs] == kColon) {
      data.fType = parseTypeSpec(seq[ofs + 1]);
      ofs += 2;
    }
  }

  if (ofs < seq.size()) {
    if (seq[ofs].isSeq() && seq[ofs].count() > 1 &&
        seq[ofs][0] == kReifyId)
    {
      // TODO: data.fReify =
      ofs++;
    }
  }

  if (ofs == whereOfs)
    ofs++;

  if (ofs < seq.size()) {
    if (seq[ofs] == kEllipsis)
      data.fFlags |= kFuncIsAbstract;
    else if (!fCompiler->isParsingInterface())
      data.fBody = singletonNodeListOrNull(parseExpr(seq[ofs]));
    ofs++;
  }
}


//------------------------------------------------------------------------------

bool
SecondPass::hasSpecParameters(const NodeList& params) const
{
  for (size_t i = 0; i < params.size(); i++) {
    const ParamNode* pParam = dynamic_cast<const ParamNode*>(params[i].obj());
    if (pParam->isSpecArg())
      return true;
  }
  return false;
}


AptNode*
SecondPass::makeGenericFunction(const SrcPos& srcpos,
                                const String& sym,
                                const FundefClauseData& data)
{
  hr_assert((data.fFlags & kFuncIsGeneric) != 0);

  String fullFuncName = qualifyId(currentModuleName(), sym);

  if (fScope->checkForRedefinition(srcpos, Scope::kNormal, fullFuncName))
    return NULL;

  Ptr<FuncDefNode> func = new FuncDefNode(srcpos,
                                          fullFuncName,
                                          // force abstractedness
                                          data.fFlags | kFuncIsAbstract,
                                          data.fParams,
                                          data.fType,
                                          // no body for generic functions
                                          NULL);
  fScope->registerFunction(srcpos, fullFuncName, func);
  return func.release();
}


AptNode*
SecondPass::makeMethod(const SrcPos& srcpos, const String& sym,
                       const FundefClauseData& data)
{
  hr_assert((data.fFlags & kFuncIsGeneric) == 0);
  hr_assert((data.fFlags & kFuncIsAbstract) == 0);

  String fullFuncName = qualifyId(currentModuleName(), sym);

  Ptr<FuncDefNode> func = new FuncDefNode(srcpos,
                                          fullFuncName,
                                          // force abstractedness
                                          data.fFlags | kFuncIsMethod,
                                          data.fParams,
                                          data.fType,
                                          data.fBody);
  return func.release();
}


AptNode*
SecondPass::makeNormalFunction(const SrcPos& srcpos, const String& sym,
                               const FundefClauseData& data,
                               bool isLocal,
                               const String& linkage)
{
  hr_assert((data.fFlags & kFuncIsGeneric) == 0);
  hr_assert((data.fFlags & kFuncIsMethod) == 0);

  String fullFuncName = ( isLocal || linkage == String("C")
                          ? sym
                          : qualifyId(currentModuleName(), sym) );

  if (fScope->checkForRedefinition(srcpos, Scope::kNormal, fullFuncName))
    return NULL;

  Ptr<FuncDefNode> func = new FuncDefNode(srcpos,
                                          fullFuncName,
                                          data.fFlags,
                                          data.fParams,
                                          data.fType,
                                          data.fBody);
  func->setLinkage(linkage);
  fScope->registerFunction(srcpos, fullFuncName, func);

  return func.release();
}


NodeList
SecondPass::parseFunctionDef(const Token& expr, size_t ofs, bool isLocal,
                             const String& linkage)
{
  hr_assert(expr.isSeq());
  hr_assert(expr.count() >= ofs + 2);

  NodeList retval;

  FundefClauseData data;

  if (expr[ofs] == Compiler::genericToken) {
    hr_assert(!isLocal);
    data.fFlags |= kFuncIsGeneric;
    ofs++;
  }

  hr_assert(expr[ofs] == kSymbol);
  String sym = expr[ofs].idValue();
  if ((isLocal || linkage == String("C")) && isQualified(sym)) {
    errorf(expr[ofs].srcpos(), E_QualifiedLocalSym,
           "Local symbol in definition must not be qualified.  "
           "Ignore namespace");
    sym = baseName(sym);
  }

  ofs++;

  const TokenVector& seq = expr.children();
  parseFundefClause(seq, ofs, data);

  bool hasSpecParams = hasSpecParameters(data.fParams);
  if (hasSpecParams) {
    if ((data.fFlags & kFuncIsGeneric) == 0) {
      // a method implementation
      if ((data.fFlags & kFuncIsAbstract) != 0) {
        errorf(expr.srcpos(), E_AbstractMethod,
               "Generic function implementations must not be abstract");
        return NodeList();
      }
      data.fFlags |= kFuncIsMethod;

      retval.push_back(makeMethod(expr.srcpos(), sym, data));
    }
    else {
      // generic functions
      if ((data.fFlags & kFuncIsAbstract) != 0) {
        // generic function definition
        retval.push_back(makeGenericFunction(expr.srcpos(), sym, data));
      }
      else {
        // generic function with default implementation
        retval.push_back(makeGenericFunction(expr.srcpos(), sym, data));

        data.fFlags &= ~kFuncIsGeneric;
        data.fFlags |= kFuncIsMethod;

        retval.push_back(makeMethod(expr.srcpos(), sym, data));
      }
    }
  }
  else {
    if ((data.fFlags & kFuncIsGeneric) != 0) {
      // generic function without specialized parameter?
      errorf(expr.srcpos(), E_GenericNoSpecPrm,
             "Generic function without specialized parameter");
      return NodeList();
    }

    // else: normal function
    retval.push_back(makeNormalFunction(expr.srcpos(), sym, data,
                                        isLocal, linkage));
  }

  return retval;
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::newDefNode(AptNode* node, bool isLet)
{
  if (isLet)
    return new LetNode(node);
  else
    return new DefNode(node);
}


NodeList
SecondPass::rewriteDefNode(AptNode* node, bool isLet)
{
  if (node != NULL)
    return newNodeList(newDefNode(node, isLet));
  return NodeList();
}


NodeList
SecondPass::rewriteDefNodes(const NodeList& nodes, bool isLet)
{
  NodeList retval;
  for (size_t i = 0; i < nodes.size(); i++) {
    AptNode* node = nodes[i];
    if (node != NULL)
      retval.push_back(newDefNode(node, isLet));
  }
  return retval;
}


NodeList
SecondPass::parseDef(const Token& expr, bool isLocal)
{
  hr_assert(expr.count() >= 2);
  hr_assert(expr[0] == kLetId || expr[0] == kDefId);

  String linkage;

  size_t ofs = 1;
  if (expr[1].isSeq() && expr[1].count() == 2 && expr[1][0] == kExternId) {
    const TokenVector& seq = expr[1].children();
    hr_assert(seq[1].isNested());
    hr_assert(seq[1].leftToken() == kParanOpen);
    hr_assert(seq[1].rightToken() == kParanClose);
    hr_assert(seq[1].count() == 1);
    hr_assert(seq[1][0] == kString);

    linkage = seq[1][0].stringValue();

    ofs++;
  }


  if (expr[ofs] == Compiler::typeToken) {
    hr_assert(linkage.isEmpty());
    if (isLocal) {
      errorf(expr.srcpos(), E_LocalTypeDef,
             "Local type definitions are not allowed");
      return NodeList();
    }
    return parseTypeDef(expr, ofs, !K(isClass), isLocal);
  }

  else if (expr[ofs] == Compiler::classToken) {
    hr_assert(linkage.isEmpty());
    if (isLocal) {
      errorf(expr.srcpos(), E_LocalTypeDef,
             "Local type definitions are not allowed");
      return NodeList();
    }
    return parseTypeDef(expr, ofs, K(isType), isLocal);
  }

  else if (expr[ofs] == Compiler::aliasToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseAliasDef(expr, ofs, isLocal), isLocal);
  }

  else if (expr[ofs] == Compiler::slotToken) {
    hr_assert(!isLocal);
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseSlotDef(expr, ofs), isLocal);
  }

  else if (expr[ofs] == Compiler::enumToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseEnumDef(expr, ofs, isLocal), isLocal);
  }
  else if (expr[ofs] == Compiler::measureToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseMeasureDef(expr, ofs, isLocal), isLocal);
  }

  else if (expr[ofs] == Compiler::unitToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseUnitDef(expr, ofs, isLocal), isLocal);
  }

  else if (expr[ofs] == Compiler::constToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseVarDef(expr, kConstVar, ofs + 1, isLocal, String()),
                          isLocal);
  }
  else if (expr[ofs] == Compiler::configToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNode(parseVarDef(expr, kConfigVar, ofs + 1, isLocal, String()),
                          isLocal);
  }

  else if (expr[ofs] == Compiler::genericToken) {
    hr_assert(linkage.isEmpty());
    return rewriteDefNodes(parseFunctionDef(expr, ofs, isLocal, String()),
                           isLocal);
  }

  else if (expr[ofs] == Compiler::charToken) {
    hr_invalid("");
    // should never come here actually
    return NodeList();
  }

  else if (expr[ofs] == Compiler::macroToken) {
    hr_invalid("");
    // should never come here actually
    return NodeList();
  }

  else if (expr[ofs] == kSymbol) {
    if (expr.count() >= ofs + 2) {
      if (expr[ofs + 1].isNested())
        return rewriteDefNodes(parseFunctionDef(expr, ofs, isLocal, linkage),
                               isLocal);

      hr_assert(expr[ofs + 1] == kAssign || expr[ofs + 1] == kColon);
      return rewriteDefNode(parseVarDef(expr, kNormalVar, ofs, isLocal, linkage),
                            isLocal);
    }

    return rewriteDefNode(parseVarDef(expr, kNormalVar, ofs, isLocal, linkage),
                          isLocal);
  }

  errorf(expr[ofs].srcpos(), 0, "Unexpected token: %s\n",
         (const char*)StrHelper(expr[ofs].toString()));
  hr_invalid("");

  return NodeList();
}


AptNode*
SecondPass::parseIf(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.count() >= 3);
  hr_assert(expr[0] == kIfId);
  hr_assert(expr[1].isNested());
  hr_assert(expr[1].count() > 0);

  NodeList test = parseTokenVector(expr[1].children());
  Ptr<AptNode> consequent = singletonNodeListOrNull(parseExpr(expr[2]));
  Ptr<AptNode> alternate;

  if (test.size() != 1) {
    errorf(expr.srcpos(), E_BadParameterList, "broken if-test");
    return NULL;
  }

  if (expr.count() >= 4) {
    hr_assert(expr[3] == kElseId);
    alternate = singletonNodeListOrNull(parseExpr(expr[4]));
  }

  return new IfNode(expr.srcpos(), test[0], consequent, alternate);
}


AptNode*
SecondPass::parseParameter(const Token& expr)
{
  if (expr == kSymbol)
    return new ParamNode(expr.srcpos(),
                         String(), expr.idValue(), kPosArg, Type(), NULL);
  hr_assert(expr.isSeq());
  hr_assert(expr.count() > 0);

  size_t ofs = 0;
  const TokenVector& seq = expr.children();

  String key;
  ParamFlags paramType = kPosArg;
  if (seq[ofs] == kKeyarg) {
    key = seq[ofs].idValue();
    if (isQualified(key)) {
      errorf(seq[ofs].srcpos(), E_QualifiedParamKey,
             "Named Parameter keys must not be qualified.  Ignore namespace");
      key = baseName(key);
    }

    hr_assert(expr.count() >= 2);
    ofs++;

    paramType = kNamedArg;
  }

  hr_assert(seq[ofs] == kSymbol);

  String sym = seq[ofs].idValue();
  if (isQualified(sym)) {
    errorf(seq[ofs].srcpos(), E_QualifiedLocalSym,
           "Parameter names must not be qualified.  Ignore namespace");
    sym = baseName(sym);
  }
  ofs++;

  Type type;
  if (ofs + 1 < expr.count()) {
    if (seq[ofs] == kColon) {
      type = parseTypeSpec(seq[ofs + 1]);
      ofs += 2;
    }
    else if (seq[ofs] == kAt) {
      type = parseTypeSpec(seq[ofs + 1]);
      ofs += 2;

      if (paramType == kNamedArg)
        errorf(expr.srcpos(), E_SpecNamedParam,
               "Specialized named parameters are not allowed");
      else
        paramType = kSpecArg;
    }
  }

  Ptr<AptNode> initExpr;
  if (ofs < expr.count()) {
    if (seq[ofs] == kAssign) {
      hr_assert(ofs + 1 < expr.count());

      if (!fCompiler->isParsingInterface())
        initExpr = singletonNodeListOrNull(parseExpr(seq[ofs + 1]));
      ofs += 2;

      paramType = kNamedArg;
      if (key.isEmpty())
        key = sym;
    }
    else if (seq[ofs] == kEllipsis) {
      hr_assert(key.isEmpty());
      ofs++;

      paramType = kRestArg;
    }
  }

  return new ParamNode(expr.srcpos(), key, sym, paramType, type, initExpr);
}


void
SecondPass::parseParameters(NodeList* parameters, const TokenVector& seq)
{
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    Ptr<AptNode> param = parseParameter(seq[i]);
    if (param)
      parameters->push_back(param);
  }
}


AptNode*
SecondPass::parseOn(const Token& expr)
{
  hr_assert(expr.count() == 4);
  hr_assert(expr[0] == kOnId);
  hr_assert(expr[1] == kSymbol);
  hr_assert(expr[2].isNested());

  NodeList params;
  parseParameters(&params, expr[2].children());

  return new OnNode(expr.srcpos(), expr[1].idValue(), params,
                    singletonNodeListOrNull(parseExpr(expr[3])));
}


AptNode*
SecondPass::parseClosure(const Token& expr)
{
  hr_assert(expr.isSeq());
  hr_assert(expr.count() >= 3);
  hr_assert(expr[0] == kFunctionId);
  hr_assert(expr[1].isNested());

  size_t ofs = 1;
  hr_assert(expr[ofs].isNested());

  NodeList params;
  parseParameters(&params, expr[1].children());
  ofs++;

  Type type;
  if (ofs + 1 < expr.count()) {
    if (expr[ofs] == kColon) {
      type = parseTypeSpec(expr[ofs + 1]);
      ofs += 2;
    }
  }

  hr_assert(ofs < expr.count());
  Ptr<AptNode> body = singletonNodeListOrNull(parseExpr(expr[ofs]));

  return new FunctionNode(expr.srcpos(), params, type, body);
}


AptNode*
SecondPass::parseBinary(const Token& expr)
{
  hr_assert(expr.count() >= 3);

  switch (expr[1].tokenType()) {
  case kAssign:
    {
      Ptr<AptNode> lvalue = singletonNodeListOrNull(parseExpr(expr[0]));
      Ptr<AptNode> rvalue = singletonNodeListOrNull(parseExpr(expr[2]));
      return new AssignNode(expr.srcpos(), lvalue, rvalue);
    }

  case kRange:
    if (expr.count() >= 5) {
      hr_assert(expr[3] == kBy);
      Ptr<AptNode> from = singletonNodeListOrNull(parseExpr(expr[0]));
      Ptr<AptNode> to   = singletonNodeListOrNull(parseExpr(expr[2]));
      Ptr<AptNode> step = singletonNodeListOrNull(parseExpr(expr[4]));
      return new RangeNode(expr.srcpos(), from, to, step);
    }
    else {
      Ptr<AptNode> from = singletonNodeListOrNull(parseExpr(expr[0]));
      Ptr<AptNode> to   = singletonNodeListOrNull(parseExpr(expr[2]));
      return new RangeNode(expr.srcpos(), from, to, NULL);
    }

  case kThenId:
    errorf(expr.srcpos(), E_MisplacedThenWhile,
           "unexpected then/while operator outside of for() expression");
    return NULL;

  case kAs:
    {
      Ptr<AptNode> base = singletonNodeListOrNull(parseExpr(expr[0]));
      Type type = parseTypeSpec(expr[2]);

      return new CastNode(expr.srcpos(), base, type);
    }
    break;

  default:
    ;
  }

  Ptr<AptNode> left  = singletonNodeListOrNull(parseExpr(expr[0]));
  Ptr<AptNode> right = singletonNodeListOrNull(parseExpr(expr[2]));

  if (left == NULL || right == NULL)
    return NULL;
  return new BinaryNode(expr.srcpos(),
                        left,
                        tokenTypeToOperator(expr[1].tokenType()),
                        right);
}


AptNode*
SecondPass::generateArrayAlloc(const Token& expr, AptNode* typeNode)
{
  const ArrayTypeNode* n = dynamic_cast<const ArrayTypeNode*>(typeNode);
  const AptNode* rootType = n->typeNode();
  hr_assert(dynamic_cast<const ArrayTypeNode*>(rootType) == NULL);

  NodeList args = parseFunCallArgs(expr[1].children());

  if (args.empty()) {
    errorf(expr[1].srcpos(), E_BadArgNumber,
           "Bad number of arguments for array allocation");
    return NULL;
  }

  Ptr<AptNode> initValue;
  size_t argc = args.size();
  if (KeyargNode* keyarg = dynamic_cast<KeyargNode*>(args[args.size() - 1].obj())) {
    if (keyarg->key() == Names::kValueKeyargName) {
      initValue = keyarg;
      argc--;
    }
    else {
      error(keyarg->srcpos(), E_BadArgKind,
            String("Unexpected key argument: ") + keyarg->key()
            + " in array allocation");
    }
  }

  if (argc > 1) {
    errorf(expr[1].srcpos(), E_BadArgNumber, "Too many arguments for array allocation");
    return NULL;
  }
  else if (argc < 1) {
    errorf(expr[1].srcpos(), E_BadArgNumber, "Not enough arguments for array allocation");
    return NULL;
  }

  //--------
  Ptr<ApplyNode> newObjAllocExpr = new ApplyNode(expr.srcpos(),
                                                 new SymbolNode(expr.srcpos(),
                                                                Names::kLangAllocateArray));
  newObjAllocExpr->appendNode(rootType->clone());
  if (initValue != NULL)
    newObjAllocExpr->appendNode(initValue);

  //--- columns (depth)
  for (size_t i = 0; i < argc; i++)
    newObjAllocExpr->appendNode(args[i]);

  return newObjAllocExpr.release();
}


AptNode*
SecondPass::generateInitObjectCall(const SrcPos& srcpos,
                                   AptNode* newObjAllocExpr,
                                   const Type& type, const TokenVector& argTokens)
{
  //---
  Ptr<AptNode> funcNode;
  if (type.isOpen()) {
    Ptr<ApplyNode> apply = new ApplyNode(srcpos,
                                         new SymbolNode(srcpos,
                                                        Names::kLangInitFunctor));
    apply->appendNode(new TypeNode(srcpos, type));
    funcNode = apply;
  }
  else {
    String initName = qualifyId(type.typeName(), Names::kInitFuncName);
    funcNode = new SymbolNode(srcpos, initName);
  }

  Ptr<ApplyNode> initExpr = new ApplyNode(srcpos, funcNode);
  initExpr->appendNode(newObjAllocExpr);

  //---
  NodeList args = parseFunCallArgs(argTokens);
  initExpr->appendNodes(args);

  return initExpr.release();
}


AptNode*
SecondPass::generateAlloc(const Token& expr, const Type& type)
{
  Ptr<ApplyNode> newObjAllocExpr = new ApplyNode(expr.srcpos(),
                                                 new SymbolNode(expr.srcpos(),
                                                                Names::kLangAllocate));
  newObjAllocExpr->appendNode(new TypeNode(expr.srcpos(), type));

  return generateInitObjectCall(expr.srcpos(),
                                newObjAllocExpr, type, expr[1].children());
}


NodeList
SecondPass::parseFunCallArgs(const TokenVector& args)
{
  NodeList parsedArgs;
  for (size_t i = 0; i < args.size(); i++) {
    if (args[i] == kComma)
      continue;

    Ptr<AptNode> arg;
    if (args[i] == kKeyarg) {
      hr_assert(i + 1 < args.size());

      Ptr<AptNode> value = singletonNodeListOrNull(parseExpr(args[i + 1]));
      arg = new KeyargNode(args[i].srcpos(), args[i].idValue(), value);
      i++;
    }
    else
      arg = singletonNodeListOrNull(parseExpr(args[i]));

    parsedArgs.push_back(arg);
  }

  return parsedArgs;
}


AptNode*
SecondPass::parseFunCall(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 2);
  hr_assert(expr[1].isNested());
  hr_assert(expr[1].leftToken() == kParanOpen);
  hr_assert(expr[1].rightToken() == kParanClose);

  Ptr<AptNode> first = singletonNodeListOrNull(parseExpr(expr[0]));
  if (first == NULL)
    return NULL;

  if (dynamic_cast<ArrayTypeNode*>(first.obj()) != NULL) {
    return generateArrayAlloc(expr, first);
  }
  else if (dynamic_cast<TypeNode*>(first.obj()) != NULL) {
    return generateAlloc(expr, dynamic_cast<TypeNode*>(first.obj())->type());
  }
  else {
    SymbolNode* symNode = dynamic_cast<SymbolNode*>(first.obj());
    if (symNode != NULL) {
      Type referedType = fScope->lookupType(symNode->name(),
                                            K(showAmbiguousSymDef));
      if (referedType.isDef())
        return generateAlloc(expr, referedType);
    }
  }

  Ptr<ApplyNode> funcall = new ApplyNode(expr.srcpos(), first);
  NodeList args = parseFunCallArgs(expr[1].children());
  funcall->appendNodes(args);

  return funcall.release();
}


//----------------------------------------------------------------------------

static bool
isExplicitForClause(const Token& expr)
{
  return (expr.isSeq() && expr.count() >= 3 &&
          expr[0].isVariableDecl() &&
          expr[1] == kAssign &&
          expr[2].isThenWhileSeq());
}


void
SecondPass::transformExplicitForClause(const Token& token,
                                       NodeList* loopDefines,
                                       NodeList* testExprs,
                                       NodeList* stepExprs)
{
  hr_assert(token.count() == 3);

  SrcPos srcpos = token.srcpos();
  Token thenWhileExpr = token[2];
  hr_assert(thenWhileExpr.count() == 3 || thenWhileExpr.count() == 5);
  hr_assert(thenWhileExpr[1] == kThenId);

  Ptr<AptNode> firstNode = singletonNodeListOrNull(parseExpr(thenWhileExpr[0]));
  Ptr<AptNode> thenNode  = singletonNodeListOrNull(parseExpr(thenWhileExpr[2]));

  Token iteratorVarSym = token[0].isSeq() ? token[0][0] : token[0];

  Ptr<AptNode> vardef = new VardefNode(srcpos,
                                       iteratorVarSym.idValue(), kNormalVar,
                                       K(isLocal), Type(),
                                       firstNode);
  Ptr<AptNode> iteratorDefNode = new LetNode(vardef);
  loopDefines->push_back(iteratorDefNode);

  Ptr<AptNode> nextNode = new AssignNode(srcpos,
                                         new SymbolNode(srcpos,
                                                        iteratorVarSym.idValue()),
                                         thenNode);
  stepExprs->push_back(nextNode);


  if (thenWhileExpr.count() == 5) {
    hr_assert(thenWhileExpr[3] == kWhileId);

    Ptr<AptNode> whileNode = singletonNodeListOrNull(parseExpr(thenWhileExpr[4]));
    testExprs->push_back(whileNode);
  }
}


static bool
isRangeForClause(const Token& expr)
{
  return (expr.isSeq() && expr.count() >= 3 &&
          expr[0].isVariableDecl() &&
          expr[1] == kIn &&
          expr[2].isRange());
}


enum RangeForClauseCountDir
{
  kRangeUpwards,
  kRangeDownwards,
  kRangeUnknown
};

void
SecondPass::transformRangeForClause(const Token& token,
                                    NodeList* loopDefines,
                                    NodeList* testExprs,
                                    NodeList* stepExprs)
{
  hr_assert(token.count() == 3);
  hr_assert(token[2].isRange());

  SrcPos srcpos = token.srcpos();

  Ptr<AptNode> beginRangeNode;
  Ptr<AptNode> endRangeNode;

  // determine loop direction
  Ptr<AptNode> stepValueNode;
  RangeForClauseCountDir direct = kRangeUnknown;
  if (token[2].count() == 3) {
    direct = kRangeUpwards;
    stepValueNode = new IntNode(srcpos, 1, !K(isImg), Type::newInt32());
  }
  else if (token[2].count() == 5) {
    Token byToken = token[2][4];
    if (byToken.isInt() || byToken.isFloat() || byToken.isRational() ||
        byToken.isChar())
    {
      direct = byToken.isNegative() ? kRangeDownwards : kRangeUpwards;
      stepValueNode = singletonNodeListOrNull(parseExpr(byToken));
    }
    else {
      direct = kRangeUnknown;

      Ptr<AptNode> tmpStepNode = singletonNodeListOrNull(parseExpr(byToken));
      // let _step = 2
      Token tmpStepSym = Token::newUniqueSymbolToken(srcpos, "step");
      Ptr<AptNode> vardef = new VardefNode(srcpos,
                                           tmpStepSym.idValue(), kNormalVar,
                                           K(isLocal), Type(),
                                           tmpStepNode);
      Ptr<AptNode> endStepNode = new LetNode(vardef);
      loopDefines->push_back(endStepNode);

      stepValueNode = new SymbolNode(srcpos, tmpStepSym.idValue());
    }
  }


  //-------- determine best end node representation
  Token beginToken = token[2][0];
  if (beginToken.isLit()) {
    beginRangeNode = singletonNodeListOrNull(parseExpr(beginToken));
  }
  else {
    Ptr<AptNode> tmpEndNode = singletonNodeListOrNull(parseExpr(beginToken));

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    Ptr<AptNode> vardef = new VardefNode(srcpos,
                                         tmpEndRangeSym.idValue(), kNormalVar,
                                         K(isLocal), Type(), tmpEndNode);
    Ptr<AptNode> endRangeDefNode = new LetNode(vardef);
    loopDefines->push_back(endRangeDefNode);

    beginRangeNode = new SymbolNode(srcpos, tmpEndRangeSym.idValue());
  }


  //-------- determine best end node representation
  Token endToken = token[2][2];
  if (endToken.isLit()) {
    endRangeNode = singletonNodeListOrNull(parseExpr(endToken));
  }
  else {
    Ptr<AptNode> tmpEndNode = singletonNodeListOrNull(parseExpr(endToken));

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    Ptr<AptNode> vardef = new VardefNode(srcpos,
                                         tmpEndRangeSym.idValue(), kNormalVar,
                                         K(isLocal), Type(), tmpEndNode);
    Ptr<AptNode> endRangeDefNode = new LetNode(vardef);
    loopDefines->push_back(endRangeDefNode);

    endRangeNode = new SymbolNode(srcpos, tmpEndRangeSym.idValue());
  }


  Token iteratorVarSym = token[0].isSeq() ? token[0][0] : token[0];

  //------------------------------ generate known counter variable
  // let i = 0  |  let i = 100
  Type stepVarType;    // TODO
  Ptr<AptNode> vardef = new VardefNode(srcpos,
                                       iteratorVarSym.idValue(), kNormalVar,
                                       K(isLocal), stepVarType,
                                       beginRangeNode);
  Ptr<AptNode> stepDefNode = new LetNode(vardef);
  loopDefines->push_back(stepDefNode);

  Token absMaxEndSym;
  Token absItVarSym;
  Token absStepVarSym;

  if (direct == kRangeUnknown) {
    // for ranges of unknown direction we need some more temporary variables
    absMaxEndSym = Token::newUniqueSymbolToken(srcpos, "abs_end");
    absItVarSym = Token::newUniqueSymbolToken(srcpos, "abs_i");
    absStepVarSym = Token::newUniqueSymbolToken(srcpos, "abs_step");

    // let __i = if (i < _end) i else _end    -- min(i, _end)
    Ptr<AptNode> absVardef =
      new VardefNode(srcpos,
                     absItVarSym.idValue(), kNormalVar, K(isLocal), Type(),
                     new IfNode(srcpos,
                                new BinaryNode(srcpos,
                                               new SymbolNode(srcpos,
                                                              iteratorVarSym.idValue()),
                                               kOpLess,
                                               endRangeNode->clone()),
                                new SymbolNode(srcpos,
                                               iteratorVarSym.idValue()),
                                endRangeNode->clone()));
    Ptr<AptNode> absItVarNode = new LetNode(absVardef);
    loopDefines->push_back(absItVarNode);

    // let _abs_end = if (i < _end) _end else i   -- max(i, _end)
    Ptr<AptNode> absMaxEndVardef =
      new VardefNode(srcpos,
                     absMaxEndSym.idValue(), kNormalVar, K(isLocal), Type(),
                     new IfNode(srcpos,
                                new BinaryNode(srcpos,
                                               new SymbolNode(srcpos,
                                                              iteratorVarSym.idValue()),
                                               kOpLess,
                                               endRangeNode->clone()),
                                endRangeNode->clone(),
                                new SymbolNode(srcpos,
                                               iteratorVarSym.idValue())));
    Ptr<AptNode> absMaxEndNode = new LetNode(absMaxEndVardef);
    loopDefines->push_back(absMaxEndNode);

    // let __abs_step = if (_step < 0) - _step else _step   -- abs(_step)
    Ptr<AptNode> absStepVarSymVardef =
      new VardefNode(srcpos,
                     absStepVarSym.idValue(), kNormalVar, K(isLocal), Type(),
                     new IfNode(srcpos,
                                new BinaryNode(srcpos,
                                               stepValueNode->clone(),
                                               kOpLess,
                                               endRangeNode->clone()),
                                new UnaryNode(srcpos,
                                              kUnaryOpNegate,
                                              stepValueNode->clone()),
                                stepValueNode->clone()));
    Ptr<AptNode> absStepVarNode = new LetNode(absStepVarSymVardef);
    loopDefines->push_back(absStepVarNode);
  }


  //------------------------------ generate test expressions
  switch (direct) {
  case kRangeUpwards:
  case kRangeDownwards:
    {
      OperatorType op = direct == kRangeUpwards ? kOpLessEqual : kOpGreaterEqual;
      // i <= _end  |  i >= _end
      Ptr<AptNode> testExprNode = new BinaryNode(srcpos,
                                                 new SymbolNode(srcpos,
                                                                iteratorVarSym.idValue()),
                                                 op,
                                                 endRangeNode);
      testExprs->push_back(testExprNode);
    }
    break;

  case kRangeUnknown:
    {
      // _abs_i <= _abs_end
      Ptr<AptNode> testExprNode = new BinaryNode(srcpos,
                                                 new SymbolNode(srcpos,
                                                                absItVarSym.idValue()),
                                                 kOpLessEqual,
                                                 new SymbolNode(srcpos,
                                                                absMaxEndSym.idValue()));
      testExprs->push_back(testExprNode);
    }
    break;
  }


  //------------------------------ generate counter step increase
  // i = i + 1
  Ptr<AptNode> stepVarNode = new SymbolNode(srcpos,
                                            iteratorVarSym.idValue());
  Ptr<AptNode> nextValueNode = new BinaryNode(srcpos,
                                              new SymbolNode(srcpos,
                                                             iteratorVarSym.idValue()),
                                              kOpPlus,
                                              stepValueNode);
  Ptr<AptNode> incrStepNode = new AssignNode(srcpos,
                                             stepVarNode, nextValueNode);
  stepExprs->push_back(incrStepNode);

  if (direct == kRangeUnknown) {
    Ptr<AptNode> absStepVarNode = new SymbolNode(srcpos,
                                                 absItVarSym.idValue());
    Ptr<AptNode> absNextValueNode = new BinaryNode(srcpos,
                                                   absStepVarNode->clone(),
                                                   kOpPlus,
                                                   new SymbolNode(srcpos,
                                                                  absStepVarSym.idValue()));
    Ptr<AptNode> absIncrStepNode = new AssignNode(srcpos,
                                                  absStepVarNode, absNextValueNode);
    stepExprs->push_back(absIncrStepNode);
  }
}


static bool
isCollForClause(const Token& expr)
{
  return (expr.isSeq() && expr.count() >= 3 &&
          expr[0].isVariableDecl() &&
          expr[1] == kIn);
}


void
SecondPass::transformCollForClause(const Token& token,
                                   NodeList* loopDefines,
                                   NodeList* testExprs)
{
  hr_assert(token.count() >= 3);

  SrcPos srcpos = token.srcpos();

  Token sym = Token::newUniqueSymbolToken(srcpos, "seq");

  // ------------------------------ let _seq = names
  Type loopType;                // TODO
  Ptr<AptNode> seqInitNode = singletonNodeListOrNull(parseExpr(token[2]));
  Ptr<AptNode> seqInitVardef = new VardefNode(srcpos,
                                              sym.idValue(), kNormalVar,
                                              K(isLocal), loopType,
                                              seqInitNode);
  Ptr<AptNode> loopDefNode = new LetNode(seqInitVardef);
  loopDefines->push_back(loopDefNode);


  // ------------------------------ let name = lang|unspecified
  Token stepSym = token[0].isSeq() ? token[0][0] : token[0];
  hr_assert(stepSym == kSymbol);
  Type stepType;                // TODO
  Ptr<AptNode> stepSymVardef = new VardefNode(srcpos,
                                              stepSym.idValue(), kNormalVar,
                                              K(isLocal), stepType,
                                              new SymbolNode(srcpos,
                                                             Names::kLangUnspecified));
  Ptr<AptNode> stepDefNode = new LetNode(stepSymVardef);
  loopDefines->push_back(stepDefNode);

  // ------------------------------ if (_seq.end?)
  Ptr<ApplyNode> testNode = new ApplyNode(srcpos,
                                          new SymbolNode(srcpos,
                                                         Names::kLangEndp));
  testNode->appendNode(new SymbolNode(srcpos, sym.idValue()));

  // --- then false
  Ptr<AptNode> consNode = new BoolNode(srcpos, false);

  // --- else { name = _seq.next true }
  Ptr<BlockNode> altNode = new BlockNode(srcpos);

  Ptr<AptNode> stepVarNode = new SymbolNode(srcpos, stepSym.idValue());
  Ptr<ApplyNode> nextSeqNode = new ApplyNode(srcpos,
                                             new SymbolNode(srcpos,
                                                            Names::kLangNext));
  nextSeqNode->appendNode(new SymbolNode(srcpos, sym.idValue()));

  Ptr<AptNode> stepNextNode = new AssignNode(srcpos,
                                             stepVarNode, nextSeqNode);
  altNode->appendNode(stepNextNode);
  altNode->appendNode(new BoolNode(srcpos, true));

  Ptr<AptNode> ifNode = new IfNode(srcpos, testNode, consNode, altNode);

  testExprs->push_back(ifNode);
}


AptNode*
SecondPass::constructWhileTestNode(const Token& expr, NodeList& testExprs)
{
  Ptr<AptNode> testNode;

  int nodeCount = 0;
  for (size_t i = 0; i < testExprs.size(); i++) {
    if (nodeCount > 1) {
      Ptr<BinaryNode> prevBin = dynamic_cast<BinaryNode*>(testNode.obj());
      hr_assert(prevBin != NULL);
      Ptr<AptNode> binNode = new BinaryNode(expr.srcpos(),
                                            prevBin->right(),
                                            kOpLogicalAnd, testExprs[i]);
      prevBin->setRight(binNode);
    }
    else if (nodeCount == 1) {
      Ptr<AptNode> binNode = new BinaryNode(expr.srcpos(),
                                            testNode, kOpLogicalAnd, testExprs[i]);
      testNode = binNode;
    }
    else
      testNode = testExprs[i];
    nodeCount++;
  }

  // if we don't have a test node yet all loop clauses are unconditional
  // ones.  Take a simple 'true' therefore.
  if (testNode == NULL) {
    testNode = new BoolNode(expr.srcpos(), true);
  }

  return testNode.release();
}


AptNode*
SecondPass::parseFor(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3 || expr.count() == 5);
  hr_assert(expr[0] == kForId);
  hr_assert(expr[1].isNested());
  hr_assert(implies(expr.count() == 5, expr[3] == kElseId));

  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

  Ptr<AptNode> body = singletonNodeListOrNull(parseExpr(expr[2]));
  Ptr<AptNode> alternate;

  if (expr.count() == 5)
    alternate = singletonNodeListOrNull(parseExpr(expr[4]));

  NodeList loopDefines;
  NodeList testExprs;
  NodeList stepExprs;

  NodeList tests;
  const TokenVector& seq = expr[1].children();
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    if (isExplicitForClause(seq[i])) {
      transformExplicitForClause(seq[i], &loopDefines, &testExprs, &stepExprs);
    }
    else if (isRangeForClause(seq[i])) {
      transformRangeForClause(seq[i], &loopDefines, &testExprs, &stepExprs);
    }
    else if (isCollForClause(seq[i])) {
      transformCollForClause(seq[i], &loopDefines, &testExprs);
    }
    else {
      Ptr<AptNode> exprNode = singletonNodeListOrNull(parseExpr(seq[i]));
      testExprs.push_back(exprNode);
    }
  }


  const bool requiresReturnValue = alternate != NULL || !testExprs.empty();
  const bool hasAlternateBranch = alternate != NULL;

  Ptr<AptNode> testNode = constructWhileTestNode(expr, testExprs);
  Ptr<AptNode> evalNextStepTestNode;

  Token returnSym = Token::newUniqueSymbolToken(expr.srcpos(), "return");
  Token tmpTestSym = Token::newUniqueSymbolToken(expr.srcpos(), "test");

  static int loopId = 0;
  loopId++;

  bool delayTypeSpec = false;
  if (requiresReturnValue) {
    Type retType;

    Ptr<AptNode> defaultRetVal;
    if (alternate == NULL) {
      TypeVector unionTypes = vector_of(Type::newAny())
                                       (Type::newTypeRef(Names::kUnspecifiedTypeName,
                                                         K(isValue)));

      retType = Type::newUnion(unionTypes, K(isValue));
      defaultRetVal = new SymbolNode(expr.srcpos(),
                                     Names::kLangUnspecified);
    }
    else {
      defaultRetVal = new UndefNode();
      delayTypeSpec = true;
    }

    Ptr<VardefNode> returnVardef = new VardefNode(expr.srcpos(),
                                                  returnSym.idValue(), kNormalVar,
                                                  K(isLocal), retType,
                                                  defaultRetVal);
    returnVardef->setTypeSpecDelayed(delayTypeSpec);

    Ptr<LetNode> defReturnNode = new LetNode(returnVardef);
    defReturnNode->setLoopId(loopId);
    loopDefines.push_back(defReturnNode.obj());

    if (hasAlternateBranch) {
      // evaluate the tests once into a temporary variable
      Ptr<AptNode> tmpTestNode = new VardefNode(expr.srcpos(),
                                                tmpTestSym.idValue(), kNormalVar,
                                                K(isLocal), Type::newBool(),
                                                testNode);
      Ptr<AptNode> defTmpTestNode = new LetNode(tmpTestNode);
      loopDefines.push_back(defTmpTestNode);

      // construct the next step evaluation of the test variable
      evalNextStepTestNode = new AssignNode(expr.srcpos(),
                                            new SymbolNode(expr.srcpos(),
                                                           tmpTestSym.idValue()),
                                            testNode->clone());

      // the test is actually to check the temporary test variable
      testNode = new SymbolNode(expr.srcpos(), tmpTestSym.idValue());
    }
  }

  Ptr<BlockNode> block = new BlockNode(expr.srcpos());
  block->appendNodes(loopDefines);

  Ptr<BlockNode> bodyNode = new BlockNode(expr.srcpos());

  if (requiresReturnValue) {
    Ptr<AptNode> retNode = new SymbolNode(expr.srcpos(),
                                          returnSym.idValue());
    Ptr<AssignNode> saveRetNode = new AssignNode(expr.srcpos(),
                                                 retNode, body);
    saveRetNode->setTypeSpecDelayed(delayTypeSpec);
    saveRetNode->setLoopId(loopId);
    bodyNode->appendNode(saveRetNode);
  }
  else
    bodyNode->appendNode(body);

  bodyNode->appendNodes(stepExprs);
  if (evalNextStepTestNode != NULL)
    bodyNode->appendNode(evalNextStepTestNode->clone());

  Ptr<WhileNode> whileNode = new WhileNode(expr.srcpos(), testNode->clone(), bodyNode);

  Ptr<SymbolNode> returnNode = new SymbolNode(expr.srcpos(), returnSym.idValue());
  returnNode->setLoopId(loopId);

  if (hasAlternateBranch) {
    Ptr<BlockNode> consequent = new BlockNode(expr.srcpos());
    consequent->appendNode(whileNode);
    consequent->appendNode(returnNode);

    Ptr<IfNode> ifNode = new IfNode(expr.srcpos(),
                                    testNode->clone(), consequent, alternate);
    block->appendNode(ifNode);
  }
  else {
    block->appendNode(whileNode);

    if (requiresReturnValue)
      block->appendNode(returnNode);
  }

  return block.release();
}


//----------------------------------------------------------------------------

AptNode*
SecondPass::parseSelect(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3);
  hr_assert(expr[0] == kSelectId);
  hr_assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  hr_assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

  const TokenVector& args = expr[1].children();

  if (args.size() > 0) {
    return parseRealSelect(expr);
  }
  else {
    return parseChainSelect(expr);
  }
}


AptNode*
SecondPass::parseRealSelect(const Token& expr)
{
  const TokenVector& args = expr[1].children();
  hr_assert(args.size() > 0);

  Ptr<AptNode> testNode = singletonNodeListOrNull(parseExpr(args[0]));
  Ptr<AptNode> comparatorNode;

  if (args.size() > 2) {
    hr_assert(args[1] == kComma);
    comparatorNode = singletonNodeListOrNull(parseExpr(args[2]));
  }

  Ptr<SelectNode> selectNode = new SelectNode(expr.srcpos(),
                                              testNode,
                                              comparatorNode);

  const TokenVector& testMappings = expr[2].children();
  for (size_t i = 0; i < testMappings.size(); i++) {
    const Token& testToken = testMappings[i];
    hr_assert(testToken.isSeq());
    hr_assert(testToken.count() == 4 || testToken.count() == 3);
    hr_assert(testToken[0] == kPipe);

    if (testToken.count() == 4) {
      hr_assert(testToken[2] == kMapTo);

      NodeList testValueNodes;
      if (testToken[1].isSeq() &&
          !testToken[1].isBinarySeq() &&
          !testToken[1].isTernarySeq())
      {
        const TokenVector& testValues = testToken[1].children();
        for (size_t j = 0; j < testValues.size(); j++) {
          if (testValues[j] == kComma)
            continue;
          Ptr<AptNode> testValueNode = singletonNodeListOrNull(parseExpr(testValues[j]));
          if (testValueNode != NULL)
            testValueNodes.push_back(testValueNode);
        }
      }
      else {
        Ptr<AptNode> testValueNode = singletonNodeListOrNull(parseExpr(testToken[1]));
        if (testValueNode != NULL)
          testValueNodes.push_back(testValueNode);
      }

      Ptr<AptNode> consqExpr = singletonNodeListOrNull(parseExpr(testToken[3]));
      if (consqExpr != NULL)
        selectNode->addMapping(testValueNodes, consqExpr);
    }
    else if (testToken.count() == 3) {
      hr_assert(testToken[1] == kElseId);

      Ptr<AptNode> consqExpr = singletonNodeListOrNull(parseExpr(testToken[2]));
      if (consqExpr != NULL)
        selectNode->addElseMapping(consqExpr);
    }
  }

  return selectNode.release();
}


AptNode*
SecondPass::parseChainSelect(const Token& expr)
{
  hr_assert(expr[1].count() == 0);

  Ptr<AptNode> resultNode;
  Ptr<IfNode> lastNode;

  const TokenVector& testMappings = expr[2].children();
  for (size_t i = 0; i < testMappings.size(); i++) {
    const Token& testToken = testMappings[i];
    hr_assert(testToken.isSeq());
    hr_assert(testToken.count() == 4 || testToken.count() == 3);
    hr_assert(testToken[0] == kPipe);

    if (testToken.count() == 4) {
      hr_assert(testToken[2] == kMapTo);

      Ptr<AptNode> testValueNode = singletonNodeListOrNull(parseExpr(testToken[1]));
      if (testValueNode != NULL) {

        Ptr<AptNode> consqNode = singletonNodeListOrNull(parseExpr(testToken[3]));

        if (consqNode != NULL) {
          Ptr<IfNode> ifNode = new IfNode(testToken[1].srcpos(),
                                          testValueNode, consqNode, NULL);
          if (lastNode != NULL) {
            lastNode->setAlternate(ifNode);
            lastNode = ifNode;
          }
          else
            resultNode = lastNode = ifNode;
        }
      }
    }
    else if (testToken.count() == 3) {
      hr_assert(testToken[1] == kElseId);

      Ptr<AptNode> consqNode = singletonNodeListOrNull(parseExpr(testToken[2]));
      if (consqNode != NULL) {
        if (lastNode != NULL)
          lastNode->setAlternate(consqNode);
        else
          resultNode = consqNode;

        break;
      }
    }
  }

  return resultNode.release();
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseMatch(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3);
  hr_assert(expr[0] == kMatchId);
  hr_assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  hr_assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

  const TokenVector& args = expr[1].children();
  hr_assert(args.size() > 0);

  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

  Ptr<BlockNode> block = new BlockNode(expr.srcpos());

  Ptr<AptNode> exprNode = singletonNodeListOrNull(parseExpr(args[0]));
  Token tmpValueSym = Token::newUniqueSymbolToken(expr.srcpos(), "match");
  Type tempType;                // TODO?
  Ptr<AptNode> tmpVarDef = new VardefNode(expr.srcpos(),
                                          tmpValueSym.idValue(),
                                          kNormalVar,
                                          K(isLocal),
                                          tempType,
                                          exprNode);
  Ptr<AptNode> tmpLetNode = new LetNode(tmpVarDef);
  block->appendNode(tmpLetNode);

  Ptr<AptNode> tmpValueNode = new SymbolNode(expr.srcpos(),
                                             tmpValueSym.idValue());

  Ptr<MatchNode> matchNode = new MatchNode(expr.srcpos(),
                                           tmpValueNode->clone());

  const TokenVector& typeMappings = expr[2].children();
  for (size_t i = 0; i < typeMappings.size(); i++) {
    const Token& typeMapping = typeMappings[i];
    hr_assert(typeMapping.isSeq());
    hr_assert(typeMapping.count() == 4);
    hr_assert(typeMapping[0] == kPipe);
    hr_assert(typeMapping[1].isSeq());
    hr_assert(typeMapping[1].count() >= 2);
    hr_assert(typeMapping[2] == kMapTo);

    {
      ScopeHelper scopeHelper(fScope,
                              !K(doExport), K(isInnerScope),
                              kScopeL_Local);

      Ptr<BlockNode> localBlock = new BlockNode(typeMapping[3].srcpos());

      String  varName;
      Type    varType;
      if (typeMapping[1].count() == 3) {
        hr_assert(typeMapping[1][0] == kSymbol);
        hr_assert(typeMapping[1][1] == kColon);

        SrcPos sympos = typeMapping[1][0].srcpos();

        varName = typeMapping[1][0].idValue();
        varType = parseTypeSpec(typeMapping[1][2]);

        Ptr<AptNode> initVal = new CastNode(typeMapping[0].srcpos(),
                                            tmpValueNode->clone(),
                                            varType);

        Ptr<VardefNode> localVar = new VardefNode(sympos,
                                                  varName, kNormalVar,
                                                  K(isLocal),
                                                  varType, initVal);
        localBlock->appendNode(new LetNode(localVar));
      }
      else {
        hr_assert(typeMapping[1][0] == kColon);
        varType = parseTypeSpec(typeMapping[1][1]);
      }

      Ptr<AptNode> consqNode = singletonNodeListOrNull(parseExpr(typeMapping[3]));
      if (consqNode != NULL) {
        localBlock->appendNode(consqNode);
        matchNode->addMapping(typeMapping[0].srcpos(),
                              varName, varType, localBlock);
      }
    }
  }

  block->appendNode(matchNode);

  return block.release();
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseTypeExpr(const Token& expr, bool inArrayType)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 2);

  if (expr[0] == kSymbol) {
    TypeVector genericArgs;
    String symbol = expr[0].idValue();

    if (expr[1].leftToken() == kGenericOpen) {
      parseTypeVector(&genericArgs, expr[1]);
      Type referedType = fScope->lookupType(symbol, K(showAmbiguousSymDef));
      if (referedType.isDef()) {
        Type ty1 = degeneralizeType(expr.srcpos(), referedType, genericArgs);
        if (ty1.isDef())
          return new TypeNode(expr.srcpos(), ty1);
      }
      return new SymbolNode(expr.srcpos(), symbol, genericArgs);
    }
    else if (expr[1].leftToken() == kBracketOpen) {
      if (inArrayType) {
        errorf(expr.srcpos(), E_MultiDimenArray,
               "Multi-dimensional array types are not supported");
        Type ty = genericTypeRef(symbol, K(isValue));
        return new TypeNode(expr.srcpos(), ty);
      }

      return new ArrayTypeNode(expr.srcpos(),
                               new SymbolNode(expr.srcpos(), symbol));
    }
    else if (expr[1].leftToken() == kParanOpen) {
      return parseFunCall(expr);
    }
    // else {
    //   error(expr[1].srcpos(), E_UnexpectedToken,
    //         String("unexpected token: ") + expr[1].toString());
    //   return NULL;
    // }
  }
  else if (expr[0].isSeq()) {
    TypeVector genericArgs;
    hr_assert(expr[0].count() == 2);
    hr_assert(expr[1].isNested());

    if (expr[0].count() == 2 && expr[0][0] == kQuote && expr[0][1] == kSymbol) {
      errorf(expr.srcpos(), E_BadGenericType,
             "Generic type is not allowed here");
      return NULL;
    }
    else if (expr[1].leftToken() == kBracketOpen) {
      Ptr<AptNode> typeNode;
      if (expr[0][0] == kQuote && expr[0][1] == kSymbol) {
        Type ty = genericTypeRef(expr[0][1].idValue(), K(isValue));
        typeNode = new TypeNode(expr.srcpos(), ty);
      }
      else {
        hr_assert(expr[0][0] == kSymbol || expr[0][0].isSeq());
        typeNode = parseTypeExpr(expr[0], K(inArrayType));
      }

      if (dynamic_cast<SymbolNode*>(typeNode.obj()) ||
          dynamic_cast<TypeNode*>(typeNode.obj()))
      {
        if (inArrayType) {
          errorf(expr.srcpos(), E_MultiDimenArray,
                 "Multi-dimensional array types are not supported");
          return typeNode;
        }
        return new ArrayTypeNode(expr.srcpos(), typeNode);
      }
      else if (typeNode != NULL) {
        error(expr[1].srcpos(), E_BadType,
              String("bad base type in array type: ") + expr.toString());
        return NULL;
      }
      return NULL;
    }
    // else {
    //   error(expr[1].srcpos(), E_UnexpectedToken,
    //         String("unexpected token: ") + expr[1].toString());
    //   return NULL;
    // }
  }
  else if (expr[0] == kQuote && expr[1] == kSymbol) {
    errorf(expr.srcpos(), E_BadGenericType,
           "Generic type is not allowed here");
    // Type ty = genericTypeRef(expr[1].idValue(), K(isValue));
    // return new TypeNode(expr.srcpos(), ty);
    return NULL;
  }

  fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
          (const char*)StrHelper(expr.toString()),
          __FILE__, __LINE__);
  hr_invalid("");
  return NULL;
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseSlotAccess(const Token& expr)
{
  hr_assert(expr.count() == 3);
  hr_assert(expr[1] == kReference);
  hr_assert(expr[2] == kSymbol);

  Ptr<AptNode> baseExpr = singletonNodeListOrNull(parseExpr(expr[0]));
  if (baseExpr == NULL)
    return NULL;

  return new SlotRefNode(expr.srcpos(), baseExpr, expr[2].idValue());
}


//------------------------------------------------------------------------------

NodeList
SecondPass::parseTokenVector(const TokenVector& seq)
{
  if (!seq.empty())
    return parseSeq(Token() << seq);
  return NodeList();
}


AptNode*
SecondPass::parseUnitNumber(const Token& expr)
{
  hr_assert(expr.count() == 3);
  hr_assert(expr[1] == kQuote);
  hr_assert(expr[2] == kSymbol);

  Ptr<AptNode> value = singletonNodeListOrNull(parseExpr(expr[0]));

  TypeUnit unit = fScope->lookupUnit(expr[2].idValue(),
                                     K(showAmbiguousSymDef));
  if (unit.isDef()) {
    return new UnitConstNode(expr.srcpos(), value, unit);
  }
  else {
    error(expr[2].srcpos(), E_UndefinedUnit,
          String("Undefined unit: ") + expr[2].idValue());
    return NULL;
  }
}


NodeList
SecondPass::parseSeq(const Token& expr)
{
  hr_assert(expr.isSeq());
  if (expr.isEmpty())
    return NodeList();

  Token first = expr[0];
  if (first == kModuleId)
    return newNodeList(parseModule(expr));
  else if (first == kExportId)
    return newNodeList(parseExport(expr));
  else if (first == kImportId)
    return newNodeList(parseImport(expr));
  else if (first == kDefId || first == kLetId)
    return parseDef(expr, first == kLetId);
  else if (first == kIfId)
    return newNodeList(parseIf(expr));
  else if (first == kOnId)
    return newNodeList(parseOn(expr));
  else if (first == kFunctionId)
    return newNodeList(parseClosure(expr));
  else if (first == kForId)
    return newNodeList(parseFor(expr));
  else if (first == kSelectId)
    return newNodeList(parseSelect(expr));
  else if (first == kMatchId)
    return newNodeList(parseMatch(expr));
  else if (expr.isBinarySeq() || expr.isTernarySeq())
    return newNodeList(parseBinary(expr));
  else if (first == kMinus) {
    hr_assert(expr.count() == 2);
    Ptr<AptNode> exprNode = singletonNodeListOrNull(parseExpr(expr[1]));
    return newNodeList(new UnaryNode(expr.srcpos(), kUnaryOpNegate, exprNode));
  }
  else if (first == kNotId) {
    hr_assert(expr.count() == 2);
    Ptr<AptNode> exprNode = singletonNodeListOrNull(parseExpr(expr[1]));
    return newNodeList(new UnaryNode(expr.srcpos(), kUnaryOpNot, exprNode));
  }
  else if (expr.count() == 2) {
    if (expr[1].isNested()) {
      if (expr[1].leftToken() == kParanOpen)
        return newNodeList(parseFunCall(expr));
      else if (expr[0] == kSymbol &&
               expr[1].leftToken() == kGenericOpen)
        return newNodeList(parseTypeExpr(expr));
      else if ((expr[0] == kSymbol || expr[0].isSeq()) &&
               expr[1].leftToken() == kBracketOpen)
        return newNodeList(parseTypeExpr(expr));
      else {
        fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
                (const char*)StrHelper(expr.toString()),
                __FILE__, __LINE__);
        hr_invalid("");              // TODO
      }
    }
    else if (expr[0] == kQuote && expr[1] == kSymbol)
      return newNodeList(parseTypeExpr(expr));
  }
  else if (expr.count() == 3) {
    if (expr[0].isNumber() && expr[1] == kColon) {
      switch (expr[0].tokenType()) {
      case kInt:
      case kUInt:
        return newNodeList(parseIntNumber(expr));
      case kRational:
        return newNodeList(parseRationalNumber(expr));
      case kFloat:
        return newNodeList(parseRealNumber(expr));
      default:
        fprintf(stderr, "%d\n", expr.tokenType());
        hr_invalid("");
        return NodeList();
      }
    }
    else if (expr[0].isNumber() && expr[1] == kQuote) {
      return newNodeList(parseUnitNumber(expr));
    }
    else if (expr[1] == kRange) {
      return newNodeList(parseBinary(expr));
    }
    else if (expr[1] == kReference && expr[2] == kSymbol) {
      return newNodeList(parseSlotAccess(expr));
    }
    else {
      fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
              (const char*)StrHelper(expr.toString()),
              __FILE__, __LINE__);
      hr_invalid("");              // TODO
    }
  }
  else if (expr.count() == 4) {
    if (expr[0] == kExtendId)
      return newNodeList(parseExtend(expr));
  }

  return parseExpr(expr[0]);
}


namespace herschel
{
  static bool
  doesNodeNeedBlock(const AptNode* node)
  {
    const OnNode* on = dynamic_cast<const OnNode*>(node);
    if (on != NULL) {
      if (on->key() == Names::kExitKeyword ||
          on->key() == Names::kSignalKeyword)
        return true;
    }
    else if (dynamic_cast<const LetNode*>(node) != NULL ||
             dynamic_cast<const DefNode*>(node) != NULL)
      return true;

    return false;
  }
}


AptNode*
SecondPass::parseBlock(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kBraceOpen);
  hr_assert(expr.rightToken() == kBraceClose);

  ScopeHelper scopeHelper(fScope,
                          !K(doExport), K(isInnerScope),
                          kScopeL_Local);

  const TokenVector& seq = expr.children();
  NodeList nodes;
  for (size_t i = 0; i < seq.size(); i++) {
    NodeList nl = parseExpr(seq[i]);
    appendNodes(nodes, nl);
  }

  if (nodes.size() == 0) {
    return new SymbolNode(expr.srcpos(), Names::kLangUnspecified);
  }
  else if (nodes.size() == 1) {
    if (!doesNodeNeedBlock(nodes[0].obj()))
      return nodes[0].release();
  }

  Ptr<BlockNode> block = new BlockNode(expr.srcpos());
  block->appendNodes(nodes);

  return block.release();
}


AptNode*
SecondPass::parseLiteralVector(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralVectorOpen);
  hr_assert(expr.rightToken() == kParanClose);

  Ptr<VectorNode> vector = new VectorNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    NodeList nl = parseExpr(seq[i]);
    vector->appendNodes(nl);
  }

  return vector.release();
}


AptNode*
SecondPass::parseLiteralArray(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralArrayOpen);
  hr_assert(expr.rightToken() == kBracketClose);

  Ptr<ArrayNode> array = new ArrayNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    NodeList nl = parseExpr(seq[i]);
    array->appendNodes(nl);
  }

  return array.release();
}


AptNode*
SecondPass::parseLiteralDict(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralVectorOpen);
  hr_assert(expr.rightToken() == kParanClose);

  Ptr<DictNode> dict = new DictNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    hr_assert(seq[i].isBinarySeq(kMapTo));

    Ptr<AptNode> key   = singletonNodeListOrNull(parseExpr(seq[i][0]));
    Ptr<AptNode> value = singletonNodeListOrNull(parseExpr(seq[i][2]));

    dict->addPair(key, value);
  }

  return dict.release();
}


NodeList
SecondPass::parseNested(const Token& expr)
{
  hr_assert(!fCompiler->isParsingInterface());
  hr_assert(expr.isNested());

  switch (expr.leftToken()) {
  case kBraceOpen:
    return newNodeList(parseBlock(expr));

  case kLiteralVectorOpen:
    if (expr.count() > 0 && expr[0].isBinarySeq(kMapTo))
      return newNodeList(parseLiteralDict(expr));
    else
      return newNodeList(parseLiteralVector(expr));

  case kLiteralArrayOpen:
    return newNodeList(parseLiteralArray(expr));

  case kParanOpen:
    hr_assert(expr.count() == 1);
    return parseExpr(expr[0]);

  case kBracketOpen:
  case kGenericOpen:

  default:
    // printf("---> %s\n", (const char*)StrHelper(expr.toString()));
    hr_invalid("");                  // you should not be here.
  }

  return NodeList();
}


Type
SecondPass::getIntType(int bitwidth, bool isSigned) const
{
  switch (bitwidth) {
  case 8:  return isSigned ? Type::newInt(8)  : Type::newUInt(8);
  case 16: return isSigned ? Type::newInt(16) : Type::newUInt(16);
  case 32: return isSigned ? Type::newInt(32) : Type::newUInt(32);
  case 64: return isSigned ? Type::newInt(64) : Type::newUInt(64);
  }

  hr_invalid("");
  return Type();
}


AptNode*
SecondPass::parseIntNumber(const Token& expr)
{
  if (expr.tokenType() == kInt || expr.tokenType() == kUInt) {
    Type type = getIntType(expr.bitwidth(), expr.tokenType() == kInt);
    type.setIsImaginary(expr.isImaginary());
    return new IntNode(expr.srcpos(), expr.intValue(), expr.isImaginary(),
                       type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());
    return new IntNode(expr.srcpos(), expr[0].intValue(),
                       expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return NULL;
}


AptNode*
SecondPass::parseRationalNumber(const Token& expr)
{
  if (expr.tokenType() == kRational) {
    Type type = Type::newRational();
    type.setIsImaginary(expr.isImaginary());
    return new RationalNode(expr.srcpos(), expr.rationalValue(),
                            expr.isImaginary(), type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());

    return new RationalNode(expr.srcpos(), expr[0].rationalValue(),
                            expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return NULL;
}


AptNode*
SecondPass::parseRealNumber(const Token& expr)
{
  if (expr.tokenType() == kFloat) {
    Type type = Type::newFloat32();
    type.setIsImaginary(expr.isImaginary());
    return new RealNode(expr.srcpos(), expr.floatValue(),
                        expr.isImaginary(), type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());

    return new RealNode(expr.srcpos(), expr[0].floatValue(),
                        expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return NULL;
}


NodeList
SecondPass::parseExpr(const Token& expr)
{
  switch (expr.type()) {
  case kId:
    {
      const AptNode* var = fScope->lookupVar(expr.idValue(), K(showAmbiguousSymDef));
      const VardefNode* vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef != NULL && vardef->isEnum() && vardef->initExpr() != NULL) {
        return newNodeList(vardef->initExpr()->clone());
      }
    }
    return newNodeList(new SymbolNode(expr.srcpos(), expr.idValue()));

  case kLit:
    switch (expr.tokenType()) {
    case kBool:
      return newNodeList(new BoolNode(expr.srcpos(), expr.boolValue()));
    case kInt:
    case kUInt:
      return newNodeList(parseIntNumber(expr));
    case kRational:
      return newNodeList(parseRationalNumber(expr));
    case kFloat:
      return newNodeList(parseRealNumber(expr));
    case kChar:
      return newNodeList(new CharNode(expr.srcpos(), expr.charValue()));
    case kString:
      return newNodeList(new StringNode(expr.srcpos(), expr.stringValue()));
    case kKeyword:
      return newNodeList(new KeywordNode(expr.srcpos(), expr.stringValue()));

    case kDocString:
      // TODO
      return NodeList();

    default:
      hr_invalid("");
    }
    break;

  case kSeq:
    return parseSeq(expr);

  case kNested:
    return parseNested(expr);

  case kPunct:
    // printf("{1} ---> %s\n", (const char*)StrHelper(expr.toString()));
    errorf(expr.srcpos(), E_UnexpectedToken,
           "Unexpected token");
    return NodeList();
  }

  return NodeList();
}


AptNode*
SecondPass::parse(const Token& exprs)
{
  hr_assert(exprs.isSeq());

  fRootNode = new CompileUnitNode(SrcPos());
  parseTopExprlist(exprs);

  return fRootNode.release();
}
