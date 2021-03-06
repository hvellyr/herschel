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

NodifyPass::NodifyPass(int level, Compiler& compiler,
                       std::shared_ptr<Scope> scope)
  : Token2AptNodeCompilePass(level),
    fScope(std::move(scope)),
    fCompiler(compiler),
    fPass(new SecondPass(fCompiler, fScope))
{ }


std::shared_ptr<AptNode>
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

SecondPass::SecondPass(Compiler& compiler, std::shared_ptr<Scope> scope)
  : AbstractPass(compiler, std::move(scope))
{
}


void
SecondPass::parseTopExprlist(const Token& expr)
{
  hr_assert(fRootNode);

  for (auto& token : expr.children())
    fRootNode->appendNodes(parseExpr(token));
}


std::shared_ptr<AptNode>
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

  return nullptr;
}


std::shared_ptr<AptNode>
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

  return nullptr;
}


std::shared_ptr<AptNode>
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
      fCompiler.importFile(expr.srcpos(), importFile, !K(isPublic), fScope);
    }
    catch (const Exception& e) {
      error(expr.srcpos(), E_UnknownInputFile, e.message());
    }
  }

  return nullptr;
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
    for (auto& c : expr[3].children())
      appendNodes(*functions, parseExpr(c));
  }
}


std::shared_ptr<AptNode>
SecondPass::parseExtend(const Token& expr)
{
  NodeList nodeList;

  parseExtendImpl(&nodeList, expr);

  for (size_t i = 0; i < nodeList.size(); i++) {
    auto n = nodeList[i];
    if (n)
      fRootNode->appendNode(n);
  }

  return nullptr;
}


//------------------------------------------------------------------------------

void
SecondPass::parseTypeVector(TypeVector* generics, const Token& expr,
                            bool forceOpenType)
{
  hr_assert(expr.isNested());

  for (auto& c : expr.children()) {
    if (c == kComma)
      continue;
    Type ty = parseTypeSpec(c, forceOpenType);
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

    constraints.push_back(TypeConstraint::makeType(kConstOp_isa,
                                                  rightType));

    if (isGeneric || forceGeneric)
      return Type::makeTypeRef(expr[0].idValue(), K(isOpen),
                              constraints, isValue);
    else
      return Type::makeTypeRef(expr[0].idValue(),
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

  constraints.push_back(TypeConstraint::makeValue(op, expr[2]));
  if (isGeneric || forceGeneric)
    return Type::makeTypeRef(expr[0].idValue(), K(isOpen),
                            constraints, isValue);

  return Type::makeTypeRef(expr[0].idValue(),
                          dummyGenerics, constraints, isValue);
}


Type
SecondPass::genericTypeRef(const String& id, bool isValue) const
{
  TSharedGenericTable::const_iterator it = fSharedGenericTable.find(id);
  if (it != fSharedGenericTable.end())
    return it->second.clone().setIsValueType(isValue);

  TypeConstVector dummyConstraints;
  return Type::makeTypeRef(id, K(isOpen), dummyConstraints, isValue);
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
    return Type::makeSeq(tyvect, isValue);
  else
    return Type::makeUnion(tyvect, isValue);
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
      return Type::makeTypeRef(expr.idValue(), K(isOpen), TypeConstVector(), isValue);
    else
      return Type::makeTypeRef(expr.idValue(), isValue);
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
                 (zstring)StrHelper(expr[0].idValue()));

        TypeVector generics;
        TypeConstVector dummyConstraints;
        parseTypeVector(&generics, expr[1]);
        return Type::makeTypeRef(expr[0].idValue(), generics, dummyConstraints,
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
        return Type::makeFunction(sign);
      }
      else if (expr.count() == 2 &&
               expr[1].isNested() && expr[1].leftToken() == kBracketOpen)
      {
        // array
        Type baseType = parseTypeSpec(expr[0]);

        int sizeInd = 0;
        if (expr[1].count() > 0) {
          TokenEvalContext ctx(*fCompiler.configVarRegistry());
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

        return Type::makeArray(baseType, sizeInd, isValue);
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
        return Type::makeFunction(sign);
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
    auto pnd = dynamic_cast<ParamNode*>(nl[i].get());
    if (pnd) {
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
        referedType = Type::makeTypeRef(primeName, K(isValue));

      result.fType = referedType;
      result.fPrime = generateInitObjectCall(primeToken.srcpos(),
                                             makeSymbolNode(primeToken.srcpos(),
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
      if (tuple.fPrime)
        primeTuples.push_back(tuple);
    }
  }
  else {
    PrimeTuple tuple = parsePrime(expr[3]);
    if (tuple.fPrime)
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
            if (slotDefs[i]) {
              auto basedef = dynamic_cast<BaseDefNode*>(slotDefs[i].get());
              hr_assert(basedef);

              auto slotDef = dynamic_cast<SlotdefNode*>(basedef->defNode().get());
              hr_assert(slotDef);

              if (slotDef) {
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
            if (auto onNode = dynamic_cast<OnNode*>(nl[0].get())) {
              if (onNode->params().size() != 1) {
                errorf(onNode->srcpos(), E_BadFunctionArity,
                       "wrong number of parameters to 'on init' hook.  Expected 1, found %d",
                       onNode->params().size());
              }
              else if (auto param = dynamic_cast<ParamNode*>(onNode->params()[0].get())) {
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
               (zstring)StrHelper(primeTuples[i].fType.typeId()));
      }
      else {
        primes.push_back(primeTuples[i]);
      }
    }
    else if (inheritsFrom.isDef()) {
      if (inheritsFrom.typeName() != primeTuples[i].fType.typeName()) {
        errorf(primeTuples[i].fPrime->srcpos(), E_BadClassOnAlloc,
               "Super class initialization for unknown type %s",
               (zstring)StrHelper(primeTuples[i].fType.typeId()));
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
                                               Type::makeTypeRef(fullTypeName,
                                                                generics, K(isValue)),
                                               funcParams);

    TypeVector genGenerics;
    for (size_t i = 0; i < generics.size(); i++) {
      hr_assert(generics[i].isRef());
      genGenerics.push_back(genericTypeRef(generics[i].typeName(), K(isValue)));
    }

    defType = Type::makeClass(fullTypeName, generics, inheritsFrom, sign,
                             slotTypes);
  }
  else {
    defType = Type::makeType(fullTypeName, generics, inheritsFrom);
  }


  fCurrentGenericTypes.clear();

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullTypeName))
    return NodeList();

  fScope->registerType(expr.srcpos(), fullTypeName, defType);

  NodeList result;
  result.push_back(newDefNode(makeTypeDefNode(expr.srcpos(),
                                              fullTypeName,
                                              isClass,
                                              defType,
                                              defaultApplyParams,
                                              slotDefs,
                                              onExprs),
                              isLocal));

  if (isClass) {
    auto ctor = generateConstructor(expr, fullTypeName, defType,
                                    defaultApplyParams,
                                    slotDefs,
                                    primes,
                                    onExprs);
    result.push_back(ctor);
  }

  return result;
}


std::shared_ptr<AptNode>
SecondPass::defaultSlotInitValue(const SlotdefNode* slot)
{
  if (slot->initExpr())
    return slot->initExpr()->clone();
  else if (slot->type().isDef()) {
    if (slot->type().isArray()) {
      auto node = makeArrayNode(slot->srcpos());
      node->setType(slot->type());
      return node;
    }
    else {
      if (slot->type().isAnyInt())
        return makeIntNode(slot->srcpos(), 0, slot->type().isImaginary(),
                           slot->type());
      else if (slot->type().isAnyFloat())
        return makeRealNode(slot->srcpos(), 0, slot->type().isImaginary(),
                            slot->type());
      else if (slot->type().isRational())
        return makeRationalNode(slot->srcpos(), Rational(0, 1),
                                slot->type().isImaginary(), slot->type());
      else if (slot->type().isString())
        return makeStringNode(slot->srcpos(), String());
      else if (slot->type().isBool())
        return makeBoolNode(slot->srcpos(), false);
      else if (slot->type().isChar())
        return makeCharNode(slot->srcpos(), Char(0));
      else if (slot->type().isKeyword())
        return makeKeywordNode(slot->srcpos(), String());
    }
  }

  // TODO

  return makeSymbolNode(slot->srcpos(), Names::kLangNil);
}


static String
findAutoParamName(const NodeList& params, const String& slotName)
{
  String resultingSlotName = slotName;

  for (auto& nd : params) {
    auto prm = dynamic_cast<ParamNode*>(nd.get());
    if (prm->name() == resultingSlotName) {
      warningf(prm->srcpos(), E_CtorArgNameConflict,
               "conflict names in class init and auto slot configuration: %s",
               (zstring)StrHelper(slotName));
      resultingSlotName = resultingSlotName + "-1";
    }
  }

  return resultingSlotName;
}


static void
insertKeyedArg(NodeList& params, std::shared_ptr<ParamNode> prm)
{
  for (size_t i = 0; i < params.size(); i++) {
    auto nl = dynamic_cast<ParamNode*>(params[i].get());
    if (nl->isRestArg()) {
      params.insert(params.begin() + i, prm);
      return;
    }
  }

  params.push_back(prm);
}


std::shared_ptr<AptNode>
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
  params.insert(params.begin(), makeParamNode(srcpos, String(), selfParamSym,
                                              kPosArg, defType, nullptr));

  auto body = makeBlockNode(srcpos);

  generatePrimeInits(srcpos, body, defType, primes, selfParamSym);

  // initialize slots
  for (auto& slotDef : slotDefs) {
    auto basedef = dynamic_cast<BaseDefNode*>(slotDef.get());
    hr_assert(basedef);

    auto slot = dynamic_cast<SlotdefNode*>(basedef->defNode().get());
    hr_assert(slot);

    if (slot->isAuto()) {
      auto autoPrmName = findAutoParamName(params, slot->name());

      auto prm = makeParamNode(srcpos, slot->name(), autoPrmName,
                               kNamedArg, slot->type(),
                               defaultSlotInitValue(slot));

      auto slotInit = makeAssignNode(srcpos,
                                     makeSlotRefNode(srcpos,
                                                     makeSymbolNode(srcpos,
                                                                    selfParamSym),
                                                     slot->name()),
                                     makeSymbolNode(srcpos, autoPrmName));
      body->appendNode(slotInit);

      insertKeyedArg(params, prm);
    }
    else {
      auto slotInit = makeAssignNode(srcpos,
                                     makeSlotRefNode(srcpos,
                                                     makeSymbolNode(srcpos,
                                                                    selfParamSym),
                                                     slot->name()),
                                     defaultSlotInitValue(slot));
      body->appendNode(slotInit);
    }
  }

  // inline a possible on init expr.
  for (auto& onExpr : onExprs) {
    auto onNode = dynamic_cast<OnNode*>(onExpr.get());
    if (onNode && onNode->key() == Compiler::initToken.idValue()) {
      NodeList onNodeParams = copyNodes(onNode->params());
      hr_assert(onNodeParams.size() == 1);
      if (!onNodeParams[0]->type().isDef() ||
          onNodeParams[0]->type().isAny())
      {
        onNodeParams[0]->setType(defType);
      }

      auto func = makeFunctionNode(srcpos, onNodeParams,
                                   Type(),
                                   onNode->body()->clone());
      auto initCall = makeApplyNode(srcpos, func);
      initCall->appendNode(makeSymbolNode(srcpos, selfParamSym));

      body->appendNode(initCall);
    }
  }

  body->appendNode(makeSymbolNode(srcpos, selfParamSym));


  // register constructor function
  auto ctorFunc = makeFuncDefNode(srcpos,
                                  ctorFuncName,
                                  0, // flags
                                  params,
                                  defType,
                                  body);
  fScope->registerFunction(typeExpr.srcpos(), ctorFuncName, ctorFunc);

  fScope->attachSymbolForExport(Scope::kNormal, fullTypeName, ctorFuncName);

  return newDefNode(ctorFunc, !K(isLocal));
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
           (zstring)StrHelper(type.typeId()));
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


std::shared_ptr<AptNode>
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

  return nullptr;
}


std::shared_ptr<AptNode>
SecondPass::getPrimeForType(const Type& reqTypeInit,
                            const std::vector<PrimeTuple>& primes,
                            const String& selfParamSym)
{
  auto prime0 = findPrimeForType(reqTypeInit, primes);
  if (prime0) {
    auto prime = prime0->clone();
    auto apply = dynamic_cast<ApplyNode*>(prime.get());
    hr_assert(apply);
    hr_assert(apply->children().size() > 0);

    apply->children()[0] = makeSymbolNode(apply->children()[0]->srcpos(),
                                          selfParamSym);
    return prime;
  }

  return nullptr;
}


void
SecondPass::generatePrimeInits(const SrcPos& srcpos,
                               std::shared_ptr<ListNode> body,
                               const Type& defType,
                               const std::vector<PrimeTuple>& primes,
                               const String& selfParamSym)
{
  std::vector<ReqTypeInitTuple> reqTypeInits = getDirectInheritedTypes(defType, fScope);

  for (size_t i = 0; i < reqTypeInits.size(); i++) {
    // does the super type requires an explicit prime?
    if (reqTypeInits[i].fIsClass) {
      if (reqTypeInits[i].fReqExplicitPrime) {
        auto apply = getPrimeForType(reqTypeInits[i].fType,
                                     primes,
                                     selfParamSym);
        if (!apply) {
          errorf(srcpos, E_BadClassOnAlloc,
                 "No 'on alloc' prime call for super class '%s'",
                 (zstring)StrHelper(reqTypeInits[i].fType.typeId()));
        }
        else
          body->appendNode(apply);
      }
      else {
        auto apply = getPrimeForType(reqTypeInits[i].fType,
                                     primes,
                                     selfParamSym);
        if (!apply)
          apply = generateInitObjectCall(SrcPos(),
                                         makeSymbolNode(SrcPos(),
                                                        selfParamSym),
                                         reqTypeInits[i].fType, TokenVector());
        body->appendNode(apply);
      }
    }
    else if (reqTypeInits[i].fType.isDef()) {
      auto prime = findPrimeForType(reqTypeInits[i].fType, primes);
      if (prime) {
        errorf(srcpos, E_BadClassOnAlloc,
               "Explicit 'on alloc' prime call for non allocable type '%s'",
               (zstring)StrHelper(reqTypeInits[i].fType.typeId()));
      }
    }
  }
}


std::shared_ptr<AptNode>
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
    return nullptr;
  }

  Type aliasType = Type::makeAlias(fullAliasName, generics, referedType);

  fScope->registerType(expr.srcpos(), fullAliasName, aliasType);

  fCurrentGenericTypes.clear();

  return nullptr;
}


std::shared_ptr<AptNode>
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

  std::shared_ptr<AptNode> initExpr;
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
               (zstring)StrHelper(seq[ofs].toString()));
      }
    }
  }

  return makeSlotdefNode(expr.srcpos(), slotName, slotFlags, slotType, initExpr);
}


std::shared_ptr<AptNode>
SecondPass::nextEnumInitValue(const SrcPos& srcpos,
                              const Token& enumItemSym,
                              const Type& baseType, Token& lastInitToken)
{
  std::shared_ptr<AptNode> initExpr;

  auto maker = baseType.makeBaseTypeEnumMaker();
  if (maker) {
    lastInitToken = maker->nextEnumItem(srcpos, enumItemSym, lastInitToken);
    if (lastInitToken.isSet())
      initExpr = singletonNodeListOrNull(parseExpr(lastInitToken));
  }
  else {
    errorf(srcpos, E_EnumNotBaseType, "Enum init value is not a base type");
    tyerror(baseType, "Enum Basetype");
  }

  return initExpr;
}


std::shared_ptr<AptNode>
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
    baseType = Type::makeInt32();

  if (!baseType.isBaseType()) {
    errorf(expr.srcpos(), E_EnumNotBaseType, "Enum base is not a base type.");
    return nullptr;
  }

  hr_assert(expr[ofs].isNested());
  hr_assert(expr[ofs].leftToken() == kBraceOpen);

  fCurrentGenericTypes.clear();

  //-------- define the items as def const x = y

  Token lastInitToken;
  SrcPos lastInitPos;
  for (auto& enumVal : expr[ofs].children()) {
    String sym;
    std::shared_ptr<AptNode> initExpr;

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

    auto fullSymName = ( isLocal
                         ? sym
                         : qualifyId(currentModuleName(), sym) );
    if (fScope->checkForRedefinition(enumVal.srcpos(),
                                     Scope::kNormal, fullSymName))
      return nullptr;

    auto var = makeVardefNode(enumVal.srcpos(),
                              fullSymName, kEnumVar, isLocal,
                              baseType, initExpr);
    fScope->registerVar(enumVal.srcpos(), fullSymName, var);

    fScope->attachSymbolForExport(Scope::kNormal, fullEnumName, fullSymName);
  }

  //-------- define the enum type as 'def type X : (Y in ...)'

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullEnumName))
    return nullptr;

  // TODO: make the enum type actually a constraint type of the values of the
  // defined items.
  TypeVector dummyGenerics;
  TypeConstVector constraints;
  Type enumType = Type::makeTypeRef(baseType.typeName(),
                                   dummyGenerics, constraints, K(isValue));

  fScope->registerType(expr.srcpos(), fullEnumName, enumType);

  // there's no apt node to generate here.
  return nullptr;
}


std::shared_ptr<AptNode>
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

  Type defMeasureType = Type::makeMeasure(fullTypeName, isaFrom, fullUnitName);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullTypeName))
    return nullptr;
  fScope->registerType(expr.srcpos(), fullTypeName, defMeasureType);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kUnit, fullUnitName))
    return nullptr;
  fScope->registerUnit(expr.srcpos(), fullUnitName, String(),
                       defMeasureType, nullptr);

  NodeList dummyApplyParams;
  NodeList dummySlotDefs;
  NodeList dummyOnExprs;

  return makeTypeDefNode(expr.srcpos(), fullTypeName, K(isClass), defMeasureType,
                         dummyApplyParams, dummySlotDefs, dummyOnExprs);
}


std::shared_ptr<AptNode>
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
    return nullptr;
  }
  ofs++;
  hr_assert(seq[ofs].isNested());

  FundefClauseData data;
  parseFundefClause(seq, ofs, data);

  auto funcNode = makeFunctionNode(expr.srcpos(), data.fParams, data.fType,
                                   data.fBody);

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kUnit, fullUnitName))
    return nullptr;
  fScope->registerUnit(expr.srcpos(), fullUnitName, baseUnit.name(),
                       baseUnit.effType(), funcNode);

  return nullptr;
}


std::shared_ptr<AptNode>
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

  std::shared_ptr<AptNode> initExpr;
  if (ofs + 1 < expr.count() && seq[ofs] == kAssign) {
    if (!fCompiler.isParsingInterface() ||
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
    return nullptr;

  auto var = makeVardefNode(expr.srcpos(), fullSymName, flags, isLocal,
                            type, initExpr);
  var->setLinkage(linkage);
  fScope->registerVar(expr.srcpos(), fullSymName, var);

  return var;
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
    else if (!fCompiler.isParsingInterface())
      data.fBody = singletonNodeListOrNull(parseExpr(seq[ofs]));
    ofs++;
  }
}


//------------------------------------------------------------------------------

bool
SecondPass::hasSpecParameters(const NodeList& params) const
{
  for (auto& nd : params) {
    auto param = dynamic_cast<ParamNode*>(nd.get());
    if (param->isSpecArg())
      return true;
  }
  return false;
}


std::shared_ptr<AptNode>
SecondPass::makeGenericFunction(const SrcPos& srcpos,
                                const String& sym,
                                const FundefClauseData& data)
{
  hr_assert((data.fFlags & kFuncIsGeneric) != 0);

  String fullFuncName = qualifyId(currentModuleName(), sym);

  if (fScope->checkForRedefinition(srcpos, Scope::kNormal, fullFuncName))
    return nullptr;

  auto func = makeFuncDefNode(srcpos,
                              fullFuncName,
                              // force abstractedness
                              data.fFlags | kFuncIsAbstract,
                              data.fParams,
                              data.fType,
                              // no body for generic functions
                              nullptr);
  fScope->registerFunction(srcpos, fullFuncName, func);
  return func;
}


std::shared_ptr<AptNode>
SecondPass::makeMethod(const SrcPos& srcpos, const String& sym,
                       const FundefClauseData& data)
{
  hr_assert((data.fFlags & kFuncIsGeneric) == 0);
  hr_assert((data.fFlags & kFuncIsAbstract) == 0);

  String fullFuncName = qualifyId(currentModuleName(), sym);

  return makeFuncDefNode(srcpos,
                         fullFuncName,
                         // force abstractedness
                         data.fFlags | kFuncIsMethod,
                         data.fParams,
                         data.fType,
                         data.fBody);
}


std::shared_ptr<AptNode>
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
    return nullptr;

  auto func = makeFuncDefNode(srcpos,
                              fullFuncName,
                              data.fFlags,
                              data.fParams,
                              data.fType,
                              data.fBody);
  func->setLinkage(linkage);
  fScope->registerFunction(srcpos, fullFuncName, func);

  return func;
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

std::shared_ptr<AptNode>
SecondPass::newDefNode(std::shared_ptr<AptNode> node, bool isLet)
{
  if (isLet)
    return makeLetNode(node);
  else
    return makeDefNode(node);
}


NodeList
SecondPass::rewriteDefNode(std::shared_ptr<AptNode> node, bool isLet)
{
  if (node)
    return makeNodeList(newDefNode(node, isLet));
  return NodeList();
}


NodeList
SecondPass::rewriteDefNodes(const NodeList& nodes, bool isLet)
{
  NodeList retval;
  for (auto& nd : nodes) {
    if (nd)
      retval.push_back(newDefNode(nd, isLet));
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
         (zstring)StrHelper(expr[ofs].toString()));
  hr_invalid("");

  return NodeList();
}


std::shared_ptr<AptNode>
SecondPass::parseIf(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.count() >= 3);
  hr_assert(expr[0] == kIfId);
  hr_assert(expr[1].isNested());
  hr_assert(expr[1].count() > 0);

  NodeList test = parseTokenVector(expr[1].children());
  auto consequent = singletonNodeListOrNull(parseExpr(expr[2]));
  std::shared_ptr<AptNode> alternate;

  if (test.size() != 1) {
    errorf(expr.srcpos(), E_BadParameterList, "broken if-test");
    return nullptr;
  }

  if (expr.count() >= 4) {
    hr_assert(expr[3] == kElseId);
    alternate = singletonNodeListOrNull(parseExpr(expr[4]));
  }

  return makeIfNode(expr.srcpos(), test[0], consequent, alternate);
}


std::shared_ptr<AptNode>
SecondPass::parseParameter(const Token& expr)
{
  if (expr == kSymbol)
    return makeParamNode(expr.srcpos(), String(), expr.idValue(), kPosArg,
                         Type(), nullptr);
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

  std::shared_ptr<AptNode> initExpr;
  if (ofs < expr.count()) {
    if (seq[ofs] == kAssign) {
      hr_assert(ofs + 1 < expr.count());

      if (!fCompiler.isParsingInterface())
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

  return makeParamNode(expr.srcpos(), key, sym, paramType, type, initExpr);
}


void
SecondPass::parseParameters(NodeList* parameters, const TokenVector& seq)
{
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    auto param = parseParameter(seq[i]);
    if (param)
      parameters->push_back(param);
  }
}


std::shared_ptr<AptNode>
SecondPass::parseOn(const Token& expr)
{
  hr_assert(expr.count() == 4);
  hr_assert(expr[0] == kOnId);
  hr_assert(expr[1] == kSymbol);
  hr_assert(expr[2].isNested());

  NodeList params;
  parseParameters(&params, expr[2].children());

  return makeOnNode(expr.srcpos(), expr[1].idValue(), params,
                    singletonNodeListOrNull(parseExpr(expr[3])));
}


std::shared_ptr<AptNode>
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
  auto body = singletonNodeListOrNull(parseExpr(expr[ofs]));

  return makeFunctionNode(expr.srcpos(), params, type, body);
}


std::shared_ptr<AptNode>
SecondPass::parseBinary(const Token& expr)
{
  hr_assert(expr.count() >= 3);

  switch (expr[1].tokenType()) {
  case kAssign:
    {
      auto lvalue = singletonNodeListOrNull(parseExpr(expr[0]));
      auto rvalue = singletonNodeListOrNull(parseExpr(expr[2]));
      return makeAssignNode(expr.srcpos(), lvalue, rvalue);
    }

  case kRange:
    if (expr.count() >= 5) {
      hr_assert(expr[3] == kBy);
      auto from = singletonNodeListOrNull(parseExpr(expr[0]));
      auto to   = singletonNodeListOrNull(parseExpr(expr[2]));
      auto step = singletonNodeListOrNull(parseExpr(expr[4]));
      return makeRangeNode(expr.srcpos(), from, to, step);
    }
    else {
      auto from = singletonNodeListOrNull(parseExpr(expr[0]));
      auto to   = singletonNodeListOrNull(parseExpr(expr[2]));
      return makeRangeNode(expr.srcpos(), from, to, nullptr);
    }

  case kThenId:
    errorf(expr.srcpos(), E_MisplacedThenWhile,
           "unexpected then/while operator outside of for() expression");
    return nullptr;

  case kAs:
    {
      auto base = singletonNodeListOrNull(parseExpr(expr[0]));
      Type type = parseTypeSpec(expr[2]);

      return makeCastNode(expr.srcpos(), base, type);
    }
    break;

  default:
    ;
  }

  auto left  = singletonNodeListOrNull(parseExpr(expr[0]));
  auto right = singletonNodeListOrNull(parseExpr(expr[2]));

  if (!left || !right)
    return nullptr;
  return makeBinaryNode(expr.srcpos(),
                        left,
                        tokenTypeToOperator(expr[1].tokenType()),
                        right);
}


std::shared_ptr<AptNode>
SecondPass::generateArrayAlloc(const Token& expr, std::shared_ptr<AptNode> typeNode)
{
  auto n = dynamic_cast<ArrayTypeNode*>(typeNode.get());
  std::shared_ptr<AptNode> rootType = n->typeNode();
  hr_assert(!dynamic_cast<ArrayTypeNode*>(rootType.get()));

  NodeList args = parseFunCallArgs(expr[1].children());

  if (args.empty()) {
    errorf(expr[1].srcpos(), E_BadArgNumber,
           "Bad number of arguments for array allocation");
    return nullptr;
  }

  std::shared_ptr<AptNode> initValue;
  size_t argc = args.size();
  if (auto keyarg = std::dynamic_pointer_cast<KeyargNode>(args[args.size() - 1])) {
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
    return nullptr;
  }
  else if (argc < 1) {
    errorf(expr[1].srcpos(), E_BadArgNumber, "Not enough arguments for array allocation");
    return nullptr;
  }

  //--------
  auto newObjAllocExpr = makeApplyNode(expr.srcpos(),
                                       makeSymbolNode(expr.srcpos(),
                                                      Names::kLangAllocateArray));
  newObjAllocExpr->appendNode(rootType->clone());
  if (initValue)
    newObjAllocExpr->appendNode(initValue);

  //--- columns (depth)
  for (size_t i = 0; i < argc; i++)
    newObjAllocExpr->appendNode(args[i]);

  return newObjAllocExpr;
}


std::shared_ptr<AptNode>
SecondPass::generateInitObjectCall(const SrcPos& srcpos,
                                   std::shared_ptr<AptNode> newObjAllocExpr,
                                   const Type& type, const TokenVector& argTokens)
{
  //---
  std::shared_ptr<AptNode> funcNode;
  if (type.isOpen()) {
    auto apply = makeApplyNode(srcpos,
                               makeSymbolNode(srcpos,
                                              Names::kLangInitFunctor));
    apply->appendNode(makeTypeNode(srcpos, type));
    funcNode = apply;
  }
  else {
    String initName = qualifyId(type.typeName(), Names::kInitFuncName);
    funcNode = makeSymbolNode(srcpos, initName);
  }

  auto initExpr = makeApplyNode(srcpos, funcNode);
  initExpr->appendNode(newObjAllocExpr);

  //---
  NodeList args = parseFunCallArgs(argTokens);
  initExpr->appendNodes(args);

  return initExpr;
}


std::shared_ptr<AptNode>
SecondPass::generateAlloc(const Token& expr, const Type& type)
{
  auto newObjAllocExpr = makeApplyNode(expr.srcpos(),
                                       makeSymbolNode(expr.srcpos(),
                                                      Names::kLangAllocate));
  newObjAllocExpr->appendNode(makeTypeNode(expr.srcpos(), type));

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

    std::shared_ptr<AptNode> arg;
    if (args[i] == kKeyarg) {
      hr_assert(i + 1 < args.size());

      auto value = singletonNodeListOrNull(parseExpr(args[i + 1]));
      arg = makeKeyargNode(args[i].srcpos(), args[i].idValue(), value);
      i++;
    }
    else
      arg = singletonNodeListOrNull(parseExpr(args[i]));

    parsedArgs.push_back(arg);
  }

  return parsedArgs;
}


std::shared_ptr<AptNode>
SecondPass::parseFunCall(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 2);
  hr_assert(expr[1].isNested());
  hr_assert(expr[1].leftToken() == kParanOpen);
  hr_assert(expr[1].rightToken() == kParanClose);

  auto first = singletonNodeListOrNull(parseExpr(expr[0]));
  if (!first)
    return nullptr;

  if (dynamic_cast<ArrayTypeNode*>(first.get())) {
    return generateArrayAlloc(expr, first);
  }
  else if (dynamic_cast<TypeNode*>(first.get())) {
    return generateAlloc(expr, dynamic_cast<TypeNode*>(first.get())->type());
  }
  else {
    auto symNode = dynamic_cast<SymbolNode*>(first.get());
    if (symNode) {
      Type referedType = fScope->lookupType(symNode->name(),
                                            K(showAmbiguousSymDef));
      if (referedType.isDef())
        return generateAlloc(expr, referedType);
    }
  }

  auto funcall = makeApplyNode(expr.srcpos(), first);
  NodeList args = parseFunCallArgs(expr[1].children());
  funcall->appendNodes(args);

  return funcall;
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

  auto firstNode = singletonNodeListOrNull(parseExpr(thenWhileExpr[0]));
  auto thenNode  = singletonNodeListOrNull(parseExpr(thenWhileExpr[2]));

  Token iteratorVarSym = token[0].isSeq() ? token[0][0] : token[0];

  auto vardef = makeVardefNode(srcpos,
                               iteratorVarSym.idValue(), kNormalVar,
                               K(isLocal), Type(),
                               firstNode);
  auto iteratorDefNode = makeLetNode(vardef);
  loopDefines->push_back(iteratorDefNode);

  auto nextNode = makeAssignNode(srcpos,
                                 makeSymbolNode(srcpos,
                                                iteratorVarSym.idValue()),
                                 thenNode);
  stepExprs->push_back(nextNode);


  if (thenWhileExpr.count() == 5) {
    hr_assert(thenWhileExpr[3] == kWhileId);

    auto whileNode = singletonNodeListOrNull(parseExpr(thenWhileExpr[4]));
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

  std::shared_ptr<AptNode> beginRangeNode;
  std::shared_ptr<AptNode> endRangeNode;

  // determine loop direction
  std::shared_ptr<AptNode> stepValueNode;
  RangeForClauseCountDir direct = kRangeUnknown;
  if (token[2].count() == 3) {
    direct = kRangeUpwards;
    stepValueNode = makeIntNode(srcpos, 1, !K(isImg), Type::makeInt32());
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

      auto tmpStepNode = singletonNodeListOrNull(parseExpr(byToken));
      // let _step = 2
      Token tmpStepSym = Token::newUniqueSymbolToken(srcpos, "step");
      auto vardef = makeVardefNode(srcpos,
                                   tmpStepSym.idValue(), kNormalVar,
                                   K(isLocal), Type(),
                                   tmpStepNode);
      auto endStepNode = makeLetNode(vardef);
      loopDefines->push_back(endStepNode);

      stepValueNode = makeSymbolNode(srcpos, tmpStepSym.idValue());
    }
  }


  //-------- determine best end node representation
  Token beginToken = token[2][0];
  if (beginToken.isLit()) {
    beginRangeNode = singletonNodeListOrNull(parseExpr(beginToken));
  }
  else {
    auto tmpEndNode = singletonNodeListOrNull(parseExpr(beginToken));

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    auto vardef = makeVardefNode(srcpos,
                                 tmpEndRangeSym.idValue(), kNormalVar,
                                 K(isLocal), Type(), tmpEndNode);
    auto endRangeDefNode = makeLetNode(vardef);
    loopDefines->push_back(endRangeDefNode);

    beginRangeNode = makeSymbolNode(srcpos, tmpEndRangeSym.idValue());
  }


  //-------- determine best end node representation
  Token endToken = token[2][2];
  if (endToken.isLit()) {
    endRangeNode = singletonNodeListOrNull(parseExpr(endToken));
  }
  else {
    auto tmpEndNode = singletonNodeListOrNull(parseExpr(endToken));

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    auto vardef = makeVardefNode(srcpos,
                                 tmpEndRangeSym.idValue(), kNormalVar,
                                 K(isLocal), Type(), tmpEndNode);
    auto endRangeDefNode = makeLetNode(vardef);
    loopDefines->push_back(endRangeDefNode);

    endRangeNode = makeSymbolNode(srcpos, tmpEndRangeSym.idValue());
  }


  Token iteratorVarSym = token[0].isSeq() ? token[0][0] : token[0];

  //------------------------------ generate known counter variable
  // let i = 0  |  let i = 100
  Type stepVarType;    // TODO
  auto vardef = makeVardefNode(srcpos,
                               iteratorVarSym.idValue(), kNormalVar,
                               K(isLocal), stepVarType,
                               beginRangeNode);
  auto stepDefNode = makeLetNode(vardef);
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
    auto absVardef =
      makeVardefNode(srcpos,
                     absItVarSym.idValue(), kNormalVar, K(isLocal), Type(),
                     makeIfNode(
                       srcpos,
                       makeBinaryNode(
                         srcpos,
                         makeSymbolNode(srcpos,
                                        iteratorVarSym.idValue()),
                         kOpLess,
                         endRangeNode->clone()),
                       makeSymbolNode(srcpos,
                                      iteratorVarSym.idValue()),
                       endRangeNode->clone()));
    auto absItVarNode = makeLetNode(absVardef);
    loopDefines->push_back(absItVarNode);

    // let _abs_end = if (i < _end) _end else i   -- max(i, _end)
    auto absMaxEndVardef =
      makeVardefNode(srcpos,
                     absMaxEndSym.idValue(), kNormalVar, K(isLocal), Type(),
                     makeIfNode(
                       srcpos,
                       makeBinaryNode(
                         srcpos,
                         makeSymbolNode(srcpos,
                                        iteratorVarSym.idValue()),
                         kOpLess,
                         endRangeNode->clone()),
                       endRangeNode->clone(),
                       makeSymbolNode(
                         srcpos,
                         iteratorVarSym.idValue())));
    auto absMaxEndNode = makeLetNode(absMaxEndVardef);
    loopDefines->push_back(absMaxEndNode);

    // let __abs_step = if (_step < 0) - _step else _step   -- abs(_step)
    auto absStepVarSymVardef =
      makeVardefNode(srcpos,
                     absStepVarSym.idValue(), kNormalVar, K(isLocal), Type(),
                     makeIfNode(
                       srcpos,
                       makeBinaryNode(srcpos,
                                      stepValueNode->clone(),
                                      kOpLess,
                                      endRangeNode->clone()),
                       makeUnaryNode(srcpos,
                                     kUnaryOpNegate,
                                     stepValueNode->clone()),
                       stepValueNode->clone()));
    auto absStepVarNode = makeLetNode(absStepVarSymVardef);
    loopDefines->push_back(absStepVarNode);
  }


  //------------------------------ generate test expressions
  switch (direct) {
  case kRangeUpwards:
  case kRangeDownwards:
    {
      OperatorType op = direct == kRangeUpwards ? kOpLessEqual : kOpGreaterEqual;
      // i <= _end  |  i >= _end
      auto testExprNode = makeBinaryNode(srcpos,
                                         makeSymbolNode(srcpos,
                                                        iteratorVarSym.idValue()),
                                         op,
                                         endRangeNode);
      testExprs->push_back(testExprNode);
    }
    break;

  case kRangeUnknown:
    {
      // _abs_i <= _abs_end
      auto testExprNode = makeBinaryNode(srcpos,
                                         makeSymbolNode(
                                           srcpos,
                                           absItVarSym.idValue()),
                                         kOpLessEqual,
                                         makeSymbolNode(
                                           srcpos,
                                           absMaxEndSym.idValue()));
      testExprs->push_back(testExprNode);
    }
    break;
  }


  //------------------------------ generate counter step increase
  // i = i + 1
  auto stepVarNode = makeSymbolNode(srcpos, iteratorVarSym.idValue());
  auto nextValueNode = makeBinaryNode(srcpos,
                                      makeSymbolNode(
                                        srcpos,
                                        iteratorVarSym.idValue()),
                                      kOpPlus,
                                      stepValueNode);
  auto incrStepNode = makeAssignNode(srcpos, stepVarNode, nextValueNode);
  stepExprs->push_back(incrStepNode);

  if (direct == kRangeUnknown) {
    auto absStepVarNode = makeSymbolNode(srcpos, absItVarSym.idValue());
    auto absNextValueNode = makeBinaryNode(srcpos,
                                           absStepVarNode->clone(),
                                           kOpPlus,
                                           makeSymbolNode(
                                             srcpos,
                                             absStepVarSym.idValue()));
    auto absIncrStepNode = makeAssignNode(srcpos, absStepVarNode,
                                          absNextValueNode);
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
  auto seqInitNode = singletonNodeListOrNull(parseExpr(token[2]));
  auto seqInitVardef = makeVardefNode(srcpos,
                                      sym.idValue(), kNormalVar,
                                      K(isLocal), loopType,
                                      seqInitNode);
  auto loopDefNode = makeLetNode(seqInitVardef);
  loopDefines->push_back(loopDefNode);


  // ------------------------------ let name = lang|unspecified
  Token stepSym = token[0].isSeq() ? token[0][0] : token[0];
  hr_assert(stepSym == kSymbol);
  Type stepType;                // TODO
  auto stepSymVardef = makeVardefNode(srcpos,
                                      stepSym.idValue(), kNormalVar,
                                      K(isLocal), stepType,
                                      makeSymbolNode(srcpos,
                                                     Names::kLangUnspecified));
  auto stepDefNode = makeLetNode(stepSymVardef);
  loopDefines->push_back(stepDefNode);

  // ------------------------------ if (_seq.end?)
  auto testNode = makeApplyNode(srcpos,
                                makeSymbolNode(srcpos, Names::kLangEndp));
  testNode->appendNode(makeSymbolNode(srcpos, sym.idValue()));

  // --- then false
  auto consNode = makeBoolNode(srcpos, false);

  // --- else { name = _seq.next true }
  auto altNode = makeBlockNode(srcpos);

  auto stepVarNode = makeSymbolNode(srcpos, stepSym.idValue());
  auto nextSeqNode = makeApplyNode(srcpos,
                                   makeSymbolNode(srcpos, Names::kLangNext));
  nextSeqNode->appendNode(makeSymbolNode(srcpos, sym.idValue()));

  auto stepNextNode = makeAssignNode(srcpos, stepVarNode, nextSeqNode);
  altNode->appendNode(stepNextNode);
  altNode->appendNode(makeBoolNode(srcpos, true));

  auto ifNode = makeIfNode(srcpos, testNode, consNode, altNode);

  testExprs->push_back(ifNode);
}


std::shared_ptr<AptNode>
SecondPass::constructWhileTestNode(const Token& expr, NodeList& testExprs)
{
  std::shared_ptr<AptNode> testNode;

  int nodeCount = 0;
  for (auto& tstExpr : testExprs) {
    if (nodeCount > 1) {
      auto prevBin = dynamic_cast<BinaryNode*>(testNode.get());
      hr_assert(prevBin);
      auto binNode = makeBinaryNode(expr.srcpos(),
                                    prevBin->right(),
                                    kOpLogicalAnd, tstExpr);
      prevBin->setRight(binNode);
    }
    else if (nodeCount == 1) {
      auto binNode = makeBinaryNode(expr.srcpos(),
                                    testNode, kOpLogicalAnd,
                                    tstExpr);
      testNode = binNode;
    }
    else
      testNode = tstExpr;
    nodeCount++;
  }

  // if we don't have a test node yet all loop clauses are unconditional
  // ones.  Take a simple 'true' therefore.
  if (!testNode) {
    testNode = makeBoolNode(expr.srcpos(), true);
  }

  return testNode;
}


std::shared_ptr<AptNode>
SecondPass::parseFor(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3 || expr.count() == 5);
  hr_assert(expr[0] == kForId);
  hr_assert(expr[1].isNested());
  hr_assert(implies(expr.count() == 5, expr[3] == kElseId));

  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

  auto body = singletonNodeListOrNull(parseExpr(expr[2]));
  std::shared_ptr<AptNode> alternate;

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
      auto exprNode = singletonNodeListOrNull(parseExpr(seq[i]));
      testExprs.push_back(exprNode);
    }
  }


  const bool requiresReturnValue = alternate || !testExprs.empty();
  const bool hasAlternateBranch = alternate != nullptr;

  auto testNode = constructWhileTestNode(expr, testExprs);
  std::shared_ptr<AptNode> evalNextStepTestNode;

  Token returnSym = Token::newUniqueSymbolToken(expr.srcpos(), "return");
  Token tmpTestSym = Token::newUniqueSymbolToken(expr.srcpos(), "test");

  static int loopId = 0;
  loopId++;

  bool delayTypeSpec = false;
  if (requiresReturnValue) {
    Type retType;

    std::shared_ptr<AptNode> defaultRetVal;
    if (!alternate) {
      TypeVector unionTypes = makeVector(Type::makeAny(),
                                         Type::makeTypeRef(Names::kUnspecifiedTypeName,
                                                          K(isValue)));

      retType = Type::makeUnion(unionTypes, K(isValue));
      defaultRetVal = makeSymbolNode(expr.srcpos(), Names::kLangUnspecified);
    }
    else {
      defaultRetVal = makeUndefNode();
      delayTypeSpec = true;
    }

    auto returnVardef = makeVardefNode(expr.srcpos(),
                                       returnSym.idValue(), kNormalVar,
                                       K(isLocal), retType,
                                       defaultRetVal);
    returnVardef->setTypeSpecDelayed(delayTypeSpec);

    auto defReturnNode = makeLetNode(returnVardef);
    defReturnNode->setLoopId(loopId);
    loopDefines.push_back(defReturnNode);

    if (hasAlternateBranch) {
      // evaluate the tests once into a temporary variable
      auto tmpTestNode = makeVardefNode(expr.srcpos(),
                                        tmpTestSym.idValue(), kNormalVar,
                                        K(isLocal), Type::makeBool(),
                                        testNode);
      auto defTmpTestNode = makeLetNode(tmpTestNode);
      loopDefines.push_back(defTmpTestNode);

      // construct the next step evaluation of the test variable
      evalNextStepTestNode = makeAssignNode(expr.srcpos(),
                                            makeSymbolNode(
                                              expr.srcpos(),
                                              tmpTestSym.idValue()),
                                            testNode->clone());

      // the test is actually to check the temporary test variable
      testNode = makeSymbolNode(expr.srcpos(), tmpTestSym.idValue());
    }
  }

  auto block = makeBlockNode(expr.srcpos());
  block->appendNodes(loopDefines);

  auto bodyNode = makeBlockNode(expr.srcpos());

  if (requiresReturnValue) {
    auto retNode = makeSymbolNode(expr.srcpos(), returnSym.idValue());
    auto saveRetNode = makeAssignNode(expr.srcpos(), retNode, body);
    saveRetNode->setTypeSpecDelayed(delayTypeSpec);
    saveRetNode->setLoopId(loopId);
    bodyNode->appendNode(saveRetNode);
  }
  else
    bodyNode->appendNode(body);

  bodyNode->appendNodes(stepExprs);
  if (evalNextStepTestNode)
    bodyNode->appendNode(evalNextStepTestNode->clone());

  auto whileNode = makeWhileNode(expr.srcpos(), testNode->clone(), bodyNode);

  auto returnNode = makeSymbolNode(expr.srcpos(), returnSym.idValue());
  returnNode->setLoopId(loopId);

  if (hasAlternateBranch) {
    auto consequent = makeBlockNode(expr.srcpos());
    consequent->appendNode(whileNode);
    consequent->appendNode(returnNode);

    auto ifNode = makeIfNode(expr.srcpos(),
                             testNode->clone(), consequent,
                             alternate);
    block->appendNode(ifNode);
  }
  else {
    block->appendNode(whileNode);

    if (requiresReturnValue)
      block->appendNode(returnNode);
  }

  return block;
}


//----------------------------------------------------------------------------

std::shared_ptr<AptNode>
SecondPass::parseSelect(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3);
  hr_assert(expr[0] == kSelectId);
  hr_assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  hr_assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

  if (expr[1].children().size() > 0) {
    return parseRealSelect(expr);
  }
  else {
    return parseChainSelect(expr);
  }
}


std::shared_ptr<AptNode>
SecondPass::parseRealSelect(const Token& expr)
{
  const TokenVector& args = expr[1].children();
  hr_assert(args.size() > 0);

  auto testNode = singletonNodeListOrNull(parseExpr(args[0]));
  std::shared_ptr<AptNode> comparatorNode;

  if (args.size() > 2) {
    hr_assert(args[1] == kComma);
    comparatorNode = singletonNodeListOrNull(parseExpr(args[2]));
  }

  auto selectNode = makeSelectNode(expr.srcpos(), testNode, comparatorNode);

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
          if (auto testValueNode = singletonNodeListOrNull(parseExpr(testValues[j])))
            testValueNodes.push_back(testValueNode);
        }
      }
      else {
        if (auto testValueNode = singletonNodeListOrNull(parseExpr(testToken[1])))
          testValueNodes.push_back(testValueNode);
      }

      if (auto consqExpr = singletonNodeListOrNull(parseExpr(testToken[3])))
        selectNode->addMapping(testValueNodes, consqExpr);
    }
    else if (testToken.count() == 3) {
      hr_assert(testToken[1] == kElseId);

      if (auto consqExpr = singletonNodeListOrNull(parseExpr(testToken[2])))
        selectNode->addElseMapping(consqExpr);
    }
  }

  return selectNode;
}


std::shared_ptr<AptNode>
SecondPass::parseChainSelect(const Token& expr)
{
  hr_assert(expr[1].count() == 0);

  std::shared_ptr<AptNode> resultNode;
  std::shared_ptr<IfNode> lastNode;

  const TokenVector& testMappings = expr[2].children();
  for (size_t i = 0; i < testMappings.size(); i++) {
    const Token& testToken = testMappings[i];
    hr_assert(testToken.isSeq());
    hr_assert(testToken.count() == 4 || testToken.count() == 3);
    hr_assert(testToken[0] == kPipe);

    if (testToken.count() == 4) {
      hr_assert(testToken[2] == kMapTo);

      auto testValueNode = singletonNodeListOrNull(parseExpr(testToken[1]));
      if (testValueNode) {

        auto consqNode = singletonNodeListOrNull(parseExpr(testToken[3]));

        if (consqNode) {
          auto ifNode = makeIfNode(testToken[1].srcpos(),
                                   testValueNode, consqNode, nullptr);
          if (lastNode) {
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

      auto consqNode = singletonNodeListOrNull(parseExpr(testToken[2]));
      if (consqNode) {
        if (lastNode)
          lastNode->setAlternate(consqNode);
        else
          resultNode = consqNode;

        break;
      }
    }
  }

  return resultNode;
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
SecondPass::parseMatch(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());

  hr_assert(expr.isSeq());
  hr_assert(expr.count() == 3);
  hr_assert(expr[0] == kMatchId);
  hr_assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  hr_assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

  const TokenVector& args = expr[1].children();
  hr_assert(args.size() > 0);

  ScopeHelper scopeHelper(fScope, !K(doExport), K(isInnerScope), kScopeL_Local);

  auto block = makeBlockNode(expr.srcpos());

  auto exprNode = singletonNodeListOrNull(parseExpr(args[0]));
  Token tmpValueSym = Token::newUniqueSymbolToken(expr.srcpos(), "match");
  Type tempType;                // TODO?
  auto tmpVarDef = makeVardefNode(expr.srcpos(),
                                  tmpValueSym.idValue(),
                                  kNormalVar,
                                  K(isLocal),
                                  tempType,
                                  exprNode);
  auto tmpLetNode = makeLetNode(tmpVarDef);
  block->appendNode(tmpLetNode);

  auto tmpValueNode = makeSymbolNode(expr.srcpos(), tmpValueSym.idValue());

  auto matchNode = makeMatchNode(expr.srcpos(), tmpValueNode->clone());

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

      auto localBlock = makeBlockNode(typeMapping[3].srcpos());

      String  varName;
      Type    varType;
      if (typeMapping[1].count() == 3) {
        hr_assert(typeMapping[1][0] == kSymbol);
        hr_assert(typeMapping[1][1] == kColon);

        SrcPos sympos = typeMapping[1][0].srcpos();

        varName = typeMapping[1][0].idValue();
        varType = parseTypeSpec(typeMapping[1][2]);

        auto initVal = makeCastNode(typeMapping[0].srcpos(),
                                    tmpValueNode->clone(),
                                    varType);
        auto localVar = makeVardefNode(sympos,
                                       varName, kNormalVar,
                                       K(isLocal),
                                       varType, initVal);
        localBlock->appendNode(makeLetNode(localVar));
      }
      else {
        hr_assert(typeMapping[1][0] == kColon);
        varType = parseTypeSpec(typeMapping[1][1]);
      }

      if (auto consqNode = singletonNodeListOrNull(parseExpr(typeMapping[3]))) {
        localBlock->appendNode(consqNode);
        matchNode->addMapping(typeMapping[0].srcpos(),
                              varName, varType, localBlock);
      }
    }
  }

  block->appendNode(matchNode);

  return block;
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
SecondPass::parseTypeExpr(const Token& expr, bool inArrayType)
{
  hr_assert(!fCompiler.isParsingInterface());
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
          return makeTypeNode(expr.srcpos(), ty1);
      }
      return makeSymbolNode(expr.srcpos(), symbol, genericArgs);
    }
    else if (expr[1].leftToken() == kBracketOpen) {
      if (inArrayType) {
        errorf(expr.srcpos(), E_MultiDimenArray,
               "Multi-dimensional array types are not supported");
        Type ty = genericTypeRef(symbol, K(isValue));
        return makeTypeNode(expr.srcpos(), ty);
      }

      return makeArrayTypeNode(expr.srcpos(),
                               makeSymbolNode(expr.srcpos(), symbol));
    }
    else if (expr[1].leftToken() == kParanOpen) {
      return parseFunCall(expr);
    }
    // else {
    //   error(expr[1].srcpos(), E_UnexpectedToken,
    //         String("unexpected token: ") + expr[1].toString());
    //   return nullptr;
    // }
  }
  else if (expr[0].isSeq()) {
    TypeVector genericArgs;
    hr_assert(expr[0].count() == 2);
    hr_assert(expr[1].isNested());

    if (expr[0].count() == 2 && expr[0][0] == kQuote && expr[0][1] == kSymbol) {
      errorf(expr.srcpos(), E_BadGenericType,
             "Generic type is not allowed here");
      return nullptr;
    }
    else if (expr[1].leftToken() == kBracketOpen) {
      std::shared_ptr<AptNode> typeNode;
      if (expr[0][0] == kQuote && expr[0][1] == kSymbol) {
        Type ty = genericTypeRef(expr[0][1].idValue(), K(isValue));
        typeNode = makeTypeNode(expr.srcpos(), ty);
      }
      else {
        hr_assert(expr[0][0] == kSymbol || expr[0][0].isSeq());
        typeNode = parseTypeExpr(expr[0], K(inArrayType));
      }

      if (dynamic_cast<SymbolNode*>(typeNode.get()) ||
          dynamic_cast<TypeNode*>(typeNode.get()))
      {
        if (inArrayType) {
          errorf(expr.srcpos(), E_MultiDimenArray,
                 "Multi-dimensional array types are not supported");
          return typeNode;
        }
        return makeArrayTypeNode(expr.srcpos(), typeNode);
      }
      else if (typeNode) {
        error(expr[1].srcpos(), E_BadType,
              String("bad base type in array type: ") + expr.toString());
        return nullptr;
      }
      return nullptr;
    }
    // else {
    //   error(expr[1].srcpos(), E_UnexpectedToken,
    //         String("unexpected token: ") + expr[1].toString());
    //   return nullptr;
    // }
  }
  else if (expr[0] == kQuote && expr[1] == kSymbol) {
    errorf(expr.srcpos(), E_BadGenericType,
           "Generic type is not allowed here");
    // Type ty = genericTypeRef(expr[1].idValue(), K(isValue));
    // return makeTypeNode(expr.srcpos(), ty);
    return nullptr;
  }

  fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
          (zstring)StrHelper(expr.toString()),
          __FILE__, __LINE__);
  hr_invalid("");
  return nullptr;
}


//------------------------------------------------------------------------------

std::shared_ptr<AptNode>
SecondPass::parseSlotAccess(const Token& expr)
{
  hr_assert(expr.count() == 3);
  hr_assert(expr[1] == kReference);
  hr_assert(expr[2] == kSymbol);

  auto baseExpr = singletonNodeListOrNull(parseExpr(expr[0]));
  if (!baseExpr)
    return nullptr;

  return makeSlotRefNode(expr.srcpos(), baseExpr, expr[2].idValue());
}


//------------------------------------------------------------------------------

NodeList
SecondPass::parseTokenVector(const TokenVector& seq)
{
  if (!seq.empty())
    return parseSeq(Token() << seq);
  return NodeList();
}


std::shared_ptr<AptNode>
SecondPass::parseUnitNumber(const Token& expr)
{
  hr_assert(expr.count() == 3);
  hr_assert(expr[1] == kQuote);
  hr_assert(expr[2] == kSymbol);

  auto value = singletonNodeListOrNull(parseExpr(expr[0]));

  auto unit = fScope->lookupUnit(expr[2].idValue(),
                                 K(showAmbiguousSymDef));
  if (unit.isDef()) {
    return makeUnitConstNode(expr.srcpos(), value, unit);
  }
  else {
    error(expr[2].srcpos(), E_UndefinedUnit,
          String("Undefined unit: ") + expr[2].idValue());
    return nullptr;
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
    return makeNodeList(parseModule(expr));
  else if (first == kExportId)
    return makeNodeList(parseExport(expr));
  else if (first == kImportId)
    return makeNodeList(parseImport(expr));
  else if (first == kDefId || first == kLetId)
    return parseDef(expr, first == kLetId);
  else if (first == kIfId)
    return makeNodeList(parseIf(expr));
  else if (first == kOnId)
    return makeNodeList(parseOn(expr));
  else if (first == kFunctionId)
    return makeNodeList(parseClosure(expr));
  else if (first == kForId)
    return makeNodeList(parseFor(expr));
  else if (first == kSelectId)
    return makeNodeList(parseSelect(expr));
  else if (first == kMatchId)
    return makeNodeList(parseMatch(expr));
  else if (expr.isBinarySeq() || expr.isTernarySeq())
    return makeNodeList(parseBinary(expr));
  else if (first == kMinus) {
    hr_assert(expr.count() == 2);
    auto exprNode = singletonNodeListOrNull(parseExpr(expr[1]));
    return makeNodeList(makeUnaryNode(expr.srcpos(),
                                      kUnaryOpNegate,
                                      exprNode));
  }
  else if (first == kNotId) {
    hr_assert(expr.count() == 2);
    auto exprNode = singletonNodeListOrNull(parseExpr(expr[1]));
    return makeNodeList(makeUnaryNode(expr.srcpos(),
                                      kUnaryOpNot,
                                      exprNode));
  }
  else if (expr.count() == 2) {
    if (expr[1].isNested()) {
      if (expr[1].leftToken() == kParanOpen)
        return makeNodeList(parseFunCall(expr));
      else if (expr[0] == kSymbol &&
               expr[1].leftToken() == kGenericOpen)
        return makeNodeList(parseTypeExpr(expr));
      else if ((expr[0] == kSymbol || expr[0].isSeq()) &&
               expr[1].leftToken() == kBracketOpen)
        return makeNodeList(parseTypeExpr(expr));
      else {
        fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
                (zstring)StrHelper(expr.toString()),
                __FILE__, __LINE__);
        hr_invalid("");              // TODO
      }
    }
    else if (expr[0] == kQuote && expr[1] == kSymbol)
      return makeNodeList(parseTypeExpr(expr));
  }
  else if (expr.count() == 3) {
    if (expr[0].isNumber() && expr[1] == kColon) {
      switch (expr[0].tokenType()) {
      case kInt:
      case kUInt:
        return makeNodeList(parseIntNumber(expr));
      case kRational:
        return makeNodeList(parseRationalNumber(expr));
      case kFloat:
        return makeNodeList(parseRealNumber(expr));
      default:
        fprintf(stderr, "%d\n", expr.tokenType());
        hr_invalid("");
        return NodeList();
      }
    }
    else if (expr[0].isNumber() && expr[1] == kQuote) {
      return makeNodeList(parseUnitNumber(expr));
    }
    else if (expr[1] == kRange) {
      return makeNodeList(parseBinary(expr));
    }
    else if (expr[1] == kReference && expr[2] == kSymbol) {
      return makeNodeList(parseSlotAccess(expr));
    }
    else {
      fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
              (zstring)StrHelper(expr.toString()),
              __FILE__, __LINE__);
      hr_invalid("");              // TODO
    }
  }
  else if (expr.count() == 4) {
    if (expr[0] == kExtendId)
      return makeNodeList(parseExtend(expr));
  }

  return parseExpr(expr[0]);
}


namespace herschel
{
  static bool
  doesNodeNeedBlock(const AptNode* node)
  {
    auto on = dynamic_cast<const OnNode*>(node);
    if (on) {
      if (on->key() == Names::kExitKeyword ||
          on->key() == Names::kSignalKeyword)
        return true;
    }
    else if (dynamic_cast<const LetNode*>(node) ||
             dynamic_cast<const DefNode*>(node))
      return true;

    return false;
  }
}


std::shared_ptr<AptNode>
SecondPass::parseBlock(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
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
    return makeSymbolNode(expr.srcpos(), Names::kLangUnspecified);
  }
  else if (nodes.size() == 1) {
    if (!doesNodeNeedBlock(nodes[0].get()))
      return nodes[0];
  }

  auto block = makeBlockNode(expr.srcpos());
  block->appendNodes(nodes);

  return block;
}


std::shared_ptr<AptNode>
SecondPass::parseLiteralVector(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralVectorOpen);
  hr_assert(expr.rightToken() == kParanClose);

  auto vector = makeVectorNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] != kComma) {
      NodeList nl = parseExpr(seq[i]);
      vector->appendNodes(nl);
    }
  }

  return vector;
}


std::shared_ptr<AptNode>
SecondPass::parseLiteralArray(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralArrayOpen);
  hr_assert(expr.rightToken() == kBracketClose);

  auto array = makeArrayNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] != kComma) {
      NodeList nl = parseExpr(seq[i]);
      array->appendNodes(nl);
    }
  }

  return array;
}


std::shared_ptr<AptNode>
SecondPass::parseLiteralDict(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.isNested());
  hr_assert(expr.leftToken() == kLiteralVectorOpen);
  hr_assert(expr.rightToken() == kParanClose);

  auto dict = makeDictNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] != kComma) {
      hr_assert(seq[i].isBinarySeq(kMapTo));

      dict->addPair(
        singletonNodeListOrNull(parseExpr(seq[i][0])),
        singletonNodeListOrNull(parseExpr(seq[i][2])));
    }
  }

  return dict;
}


NodeList
SecondPass::parseNested(const Token& expr)
{
  hr_assert(!fCompiler.isParsingInterface());
  hr_assert(expr.isNested());

  switch (expr.leftToken()) {
  case kBraceOpen:
    return makeNodeList(parseBlock(expr));

  case kLiteralVectorOpen:
    if (expr.count() > 0 && expr[0].isBinarySeq(kMapTo))
      return makeNodeList(parseLiteralDict(expr));
    else
      return makeNodeList(parseLiteralVector(expr));

  case kLiteralArrayOpen:
    return makeNodeList(parseLiteralArray(expr));

  case kParanOpen:
    hr_assert(expr.count() == 1);
    return parseExpr(expr[0]);

  case kBracketOpen:
  case kGenericOpen:

  default:
    // printf("---> %s\n", (zstring)StrHelper(expr.toString()));
    hr_invalid("");                  // you should not be here.
  }

  return NodeList();
}


Type
SecondPass::getIntType(int bitwidth, bool isSigned) const
{
  switch (bitwidth) {
  case 8:  return isSigned ? Type::makeInt(8)  : Type::makeUInt(8);
  case 16: return isSigned ? Type::makeInt(16) : Type::makeUInt(16);
  case 32: return isSigned ? Type::makeInt(32) : Type::makeUInt(32);
  case 64: return isSigned ? Type::makeInt(64) : Type::makeUInt(64);
  }

  hr_invalid("");
  return Type();
}


std::shared_ptr<AptNode>
SecondPass::parseIntNumber(const Token& expr)
{
  if (expr.tokenType() == kInt || expr.tokenType() == kUInt) {
    Type type = getIntType(expr.bitwidth(), expr.tokenType() == kInt);
    type.setIsImaginary(expr.isImaginary());
    return makeIntNode(expr.srcpos(), expr.intValue(), expr.isImaginary(),
                       type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());
    return makeIntNode(expr.srcpos(), expr[0].intValue(),
                       expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return nullptr;
}


std::shared_ptr<AptNode>
SecondPass::parseRationalNumber(const Token& expr)
{
  if (expr.tokenType() == kRational) {
    Type type = Type::makeRational();
    type.setIsImaginary(expr.isImaginary());
    return makeRationalNode(expr.srcpos(), expr.rationalValue(),
                            expr.isImaginary(), type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());

    return makeRationalNode(expr.srcpos(), expr[0].rationalValue(),
                            expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return nullptr;
}


std::shared_ptr<AptNode>
SecondPass::parseRealNumber(const Token& expr)
{
  if (expr.tokenType() == kFloat) {
    Type type = Type::makeFloat32();
    type.setIsImaginary(expr.isImaginary());
    return makeRealNode(expr.srcpos(), expr.floatValue(),
                        expr.isImaginary(), type);
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);
    if (type.isDef())
      type.setIsImaginary(expr[0].isImaginary());

    return makeRealNode(expr.srcpos(), expr[0].floatValue(),
                        expr[0].isImaginary(), type);
  }

  hr_invalid("");
  return nullptr;
}


NodeList
SecondPass::parseExpr(const Token& expr)
{
  switch (expr.type()) {
  case kId:
    {
      auto var = fScope->lookupVar(expr.idValue(), K(showAmbiguousSymDef));
      auto vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef && vardef->isEnum() && vardef->initExpr()) {
        return makeNodeList(vardef->initExpr()->clone());
      }
    }
    return makeNodeList(makeSymbolNode(expr.srcpos(), expr.idValue()));

  case kLit:
    switch (expr.tokenType()) {
    case kBool:
      return makeNodeList(makeBoolNode(expr.srcpos(), expr.boolValue()));
    case kInt:
    case kUInt:
      return makeNodeList(parseIntNumber(expr));
    case kRational:
      return makeNodeList(parseRationalNumber(expr));
    case kFloat:
      return makeNodeList(parseRealNumber(expr));
    case kChar:
      return makeNodeList(makeCharNode(expr.srcpos(), expr.charValue()));
    case kString:
      return makeNodeList(makeStringNode(expr.srcpos(), expr.stringValue()));
    case kKeyword:
      return makeNodeList(makeKeywordNode(expr.srcpos(), expr.stringValue()));

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
    // printf("{1} ---> %s\n", (zstring)StrHelper(expr.toString()));
    errorf(expr.srcpos(), E_UnexpectedToken,
           "Unexpected token");
    return NodeList();
  }

  return NodeList();
}


std::shared_ptr<AptNode>
SecondPass::parse(const Token& exprs)
{
  hr_assert(exprs.isSeq());

  fRootNode = makeCompileUnitNode(SrcPos());
  parseTopExprlist(exprs);

  return fRootNode;
}
