/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
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


using namespace heather;

//----------------------------------------------------------------------------

NodifyPass::NodifyPass(int level, Compiler* compiler, Scope* scope)
  : Token2AptNodeCompilePass(level),
    fScope(scope),
    fCompiler(compiler),
    fPass(new SecondPass(fCompiler, fScope))
{ }


AptNode*
NodifyPass::doApply(const Token& src)
{
  return fPass->parse(src);
}


Scope*
NodifyPass::currentScope()
{
  return fPass->scope();
}


//----------------------------------------------------------------------------

SecondPass::SecondPass(Compiler* compiler, Scope* scope)
  : AbstractPass(compiler, scope)
{
}


void
SecondPass::parseTopExprlist(const Token& expr)
{
  assert(fRootNode != NULL);

  for (TokenVector::const_iterator it = expr.children().begin();
       it != expr.children().end();
       it++)
  {
    Ptr<AptNode> n = parseExpr(*it);
    if (n != NULL)
      fRootNode->appendNode(n);
  }
}


AptNode*
SecondPass::parseModule(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kModuleId);
  assert(expr[1].isSymbol());

  String modName = expr[1].idValue();
  String publicId;

  if (expr.count() > 2) {
    assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);
    assert(expr[2].count() == 1);
    assert(expr[2][0].isString());

    publicId = expr[2][0].stringValue();
  }


  if (expr.count() > 3) {
    assert(expr[3].isNested() && expr[3].leftToken() == kBraceOpen);

    {
      ScopeHelper scopeHelper(fScope, true, true, kScopeL_Module);

      ModuleHelper moduleHelper(this, modName);
      parseTopExprlist(expr[3]);
    }
  }
  else {
    fScope = new Scope(kScopeL_Module, fScope);
    fCurrentModuleName = qualifyId(fCurrentModuleName, modName);
  }

  return NULL;
}


AptNode*
SecondPass::parseExport(const Token& expr)
{
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kExportId);

  size_t symbolOfs = 1;
  VizType vizType = kPrivate;
  if (expr[1].isSymbol()) {
    if (expr[1] == Compiler::publicToken)
      vizType = kPublic;
    else if (expr[1] == Compiler::innerToken)
      vizType = kInner;
    else if (expr[1] == Compiler::outerToken)
      vizType = kOuter;
    else {
      error(expr[1].srcpos(), E_UnknownVisibility,
            String("unknown visibility level: ") + expr[1]);
    }

    symbolOfs = 2;
  }

  assert(expr.count() > symbolOfs);
  std::vector<Scope::ScopeName> symbols;

  if (expr[symbolOfs].isNested()) {
    Token symbolExprs = expr[symbolOfs];

    for (size_t j = 0; j < symbolExprs.count(); j++) {
      if (symbolExprs[j].isSymbol()) {
        symbols.push_back(Scope::ScopeName(Scope::kNormal, symbolExprs[j].idValue()));
      }
      else if (symbolExprs[j].isSeq()) {
        assert(symbolExprs[j].count() == 3);
        assert(symbolExprs[j][1] == kColon);
        assert(symbolExprs[j][2] == kSymbol);

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
    assert(expr[symbolOfs + 1] == kAs);
    assert(expr[symbolOfs + 2] == Compiler::finalToken);

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
  assert(expr.isSeq() && expr.count() >= 2);
  assert(expr[0] == kImportId);
  assert(expr[1].isString());

  String importFile = expr[1].stringValue();
  // StringStringMap renames;

  // if (expr.count() >= 3) {
  //   assert(expr[2].isNested() && expr[2].leftToken() == kParanOpen);

  //   Token renExprs = expr[2];
  //   for (int i = 0; i < renExprs.count(); i++) {
  //     Token renExpr = renExprs[i];
  //     if (renExpr.isBinarySeq(kMapTo)) {
  //       assert(renExpr[0].isSymbol());
  //       assert(renExpr[2].isSymbol());

  //       renames.insert(std::make_pair(
  //                        renExpr[0].idValue(), renExpr[2].idValue()));
  //     }
  //   }
  // }

  // return new ImportNode(expr[1].srcpos(), importFile, renames);

  bool canImport = true;
#if defined(UNITTESTS)
  canImport = !Properties::test_dontImport();
#endif

  if (canImport) {
    try
    {
      fCompiler->importFile(expr.srcpos(), importFile, false, fScope);
    }
    catch (const Exception& e) {
      error(expr.srcpos(), E_UnknownInputFile, e.message());
    }
  }

  return NULL;
}


//------------------------------------------------------------------------------

void
SecondPass::parseExtendImpl(NodeList* reqProtocol, const Token& expr)
{
  assert(expr.isSeq() && expr.count() == 4);
  assert(expr[0] == kExtendId);
  assert(expr[1] == kModuleId);
  assert(expr[2] == kSymbol);
  assert(expr[3].isNested());

  String moduleName = expr[2].idValue();

  {
    // temporarily change the current module name
    ModuleHelper modHelper(this, moduleName, true);

    const TokenVector& children = expr[3].children();
    for (size_t i = 0; i < children.size(); i++) {
      Ptr<AptNode> node = parseExpr(children[i]);
      if (node != NULL)
        reqProtocol->push_back(node);
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
SecondPass::parseTypeVector(TypeVector* generics, const Token& expr)
{
  assert(expr.isNested());

  for (size_t i = 0; i < expr.children().size(); i++) {
    if (expr[i] == kComma)
      continue;
    Type ty = parseTypeSpec(expr[i]);
    generics->push_back(ty);
  }
}


Type
SecondPass::parseTypeSpec(const Token& expr)
{
  Type ty = parseTypeSpecImpl(expr);

  if (ty.isRef()) {
    Type referedType = fScope->lookupType(ty.typeName(), true);
    if (referedType.isDef()) {
      if (referedType.isAlias())
        return fScope->normalizeType(referedType, ty);

      // we normally don't want to have full types here (these would lead to
      // unnecessary data expansion and possible issues with recursive types).
      // Rewrite the typeref to have the fully qualified type name
      return Type::newTypeRef(referedType.typeName(), ty);
    }
  }

  return ty;
}


Type
SecondPass::parseBinaryTypeSpec(const Token& expr, bool forceGeneric,
                                bool isValue)
{
  assert(expr.count() == 3);
  assert(expr[0] == kSymbol);

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
      return Type::newTypeRef(expr[0].idValue(), true, constraints, isValue);
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
    assert(0);

  constraints.push_back(TypeConstraint::newValue(op, expr[2]));
  if (isGeneric || forceGeneric)
    return Type::newTypeRef(expr[0].idValue(), true, constraints, isValue);

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
  return Type::newTypeRef(id, true, dummyConstraints, isValue);
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
  assert(expr.isNested());
  assert(expr.leftToken() == kParanOpen || expr.leftToken() == kUnionOpen);

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
SecondPass::parseTypeSpecImpl(const Token& expr)
{
  if (expr.isSeq() && expr[0] == kReference) {
    assert(expr.count() == 2);

    return parseTypeSpecImpl2(expr[1], false);
  }

  return parseTypeSpecImpl2(expr, true);
}


Type
SecondPass::parseTypeSpecImpl2(const Token& expr, bool isValue)
{
  if (expr == kSymbol) {
    if (fCurrentGenericTypes.find(expr.idValue()) != fCurrentGenericTypes.end())
      return genericTypeRef(expr.idValue(), isValue);
    else
      return Type::newTypeRef(expr.idValue(), isValue);
  }
  else if (expr.isSeq()) {
    if (expr.count() == 2) {
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

        FunctionSignature sign(false, String(), Type(), funcParams);
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
            errorf(expr[1][0].srcpos(), E_InvaliArraySize,
                   "array size expression did not evaluate to integer. Treat it as 0");
          }
        }

        return Type::newArray(baseType, sizeInd, isValue);
      }
      else if (expr[0] == kQuote) {
        assert(expr[1] == kSymbol);
        return genericTypeRef(expr[1].idValue(), isValue);
      }
      else
        assert(0);
    }
    else if (expr.count() == 3) {
      return parseBinaryTypeSpec(expr, false, isValue);
    }
    else if (expr.count() == 4) {
      if (expr[0] == kFUNCTIONId &&
          expr[1].isNested() && expr[1].leftToken() == kParanOpen)
      {
        assert(expr[2] == kColon);

        if (!isValue)
          warning(expr.srcpos(), E_RefToFunc,
                  String("References to function types have no effect.  Ignored"));

        NodeList defaultApplyParams;
        parseParameters(&defaultApplyParams, expr[1].children());

        FunctionParamVector funcParams;
        paramsNodeListToType(&funcParams, defaultApplyParams);

        Type retType = parseTypeSpec(expr[3]);

        FunctionSignature sign(false, String(), retType, funcParams);
        return Type::newFunction(sign);
      }
    }
    else
      assert(0);
  }
  else if (expr.isNested()) {
    return parseGroupType(expr, isValue);
  }
  else
    assert(0);

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
                            false, String(), pnd->type()));
        break;
      case kSpecArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamPos,
                            true, String(), pnd->type()));
        break;
      case kNamedArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamNamed,
                            false, pnd->key(), pnd->type()));
        break;
      case kRestArg:
        funcParams->push_back(
          FunctionParameter(FunctionParameter::kParamRest,
                            false, String(), pnd->type()));
        break;
      }
    }
  }
}


FunctionSignature
SecondPass::nodeToFunSignature(const FuncDefNode* node) const
{
  FunctionParamVector funcParams;
  paramsNodeListToType(&funcParams, node->params());

  return FunctionSignature(node->isGeneric(),
                           node->name(),
                           node->retType(),
                           funcParams);
}


void
SecondPass::protocolNodeListToType(FunctionSignatureVector* protoSignatures,
                                   const NodeList& nl) const
{
  for (size_t i = 0; i < nl.size(); i++) {
    const BaseDefNode* defnd = dynamic_cast<const BaseDefNode*>(nl[i].obj());
    if (defnd != NULL) {
      const FuncDefNode* fnd = dynamic_cast<const FuncDefNode*>(defnd->defNode());
      if (fnd != NULL)
        protoSignatures->push_back(nodeToFunSignature(fnd));
    }
  }
}


Type
SecondPass::parseWhereConstraint(const Token& whereConstrSeq)
{
  assert(whereConstrSeq.isSeq());
  assert(whereConstrSeq.count() == 3);
  assert(whereConstrSeq[0] == kSymbol);

  return parseBinaryTypeSpec(whereConstrSeq, true, true);
}


void
SecondPass::parseWhereClause(const Token& whereSeq)
{
  const TokenVector& whereClause = whereSeq.children();
  assert(whereClause[0] == kWhereId);
  assert(whereClause.size() > 1);

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
      assert(i > ofs);
      return i;
    }
  }

  return 0;
}


size_t
SecondPass::getWhereOfs(const Token& expr) const
{
  return getWhereOfs(expr.children(), 0);
}


AptNode*
SecondPass::parseTypeDef(const Token& expr, size_t ofs, bool isClass)
{
  assert(fCurrentGenericTypes.empty());
  TSharedGenericScopeHelper SharedTable(fSharedGenericTable);

  assert(expr.isSeq());
  assert(expr.count() >= ofs + 2);
  assert(expr[ofs] == Compiler::typeToken || expr[ofs] == Compiler::classToken);
  assert(expr[ofs + 1] == kSymbol);

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
    parseTypeVector(&generics, expr[ofs]);

    for (size_t i = 0; i < generics.size(); i++) {
      assert(generics[i].isRef());
      fCurrentGenericTypes.insert(generics[i].typeName());
    }
    ofs++;
  }

  NodeList defaultApplyParams;
  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kParanOpen)
  {
    // default apply signature
    assert(isClass);

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

  NodeList slotDefs;
  NodeList reqProtocol;
  NodeList onExprs;

  if (ofs < seq.size() &&
      seq[ofs].isNested() && seq[ofs].leftToken() == kBraceOpen)
  {
    const TokenVector& defs = seq[ofs].children();

    for (size_t i = 0; i < defs.size(); i++) {
      assert(defs[i].isSeq() && defs[i].count() > 1);
      assert(defs[i][0] == kDefId ||
             defs[i][0] == kOnId ||
             defs[i][0] == kExtendId);

      if (defs[i][0] == kDefId) {
        if (defs[i][1] == Compiler::slotToken) {
          assert(isClass);

          Ptr<AptNode> def = parseExpr(defs[i]);
          if (def != NULL)
            slotDefs.push_back(def);
        }
        else if (defs[i][1] == Compiler::genericToken) {
          Ptr<AptNode> def = parseExpr(defs[i]);
          if (def != NULL)
            reqProtocol.push_back(def);
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
        else {
          Ptr<AptNode> on = parseExpr(defs[i]);
          onExprs.push_back(on);
        }
      }
      else if (defs[i][0] == kExtendId) {
        parseExtendImpl(&reqProtocol, defs[i]);
      }
      else {
        errorf(defs[i].srcpos(), E_UnexpectedDefExpr,
               "Unexpected expression in type body");
      }
    }

    ofs++;
  }


  FunctionSignatureVector protoSignatures;
  protocolNodeListToType(&protoSignatures, reqProtocol);

  Type defType;
  if (isClass) {
    FunctionParamVector funcParams;
    paramsNodeListToType(&funcParams, defaultApplyParams);

    TypeVector genGenerics;
    for (size_t i = 0; i < generics.size(); i++) {
      assert(generics[i].isRef());
      genGenerics.push_back(genericTypeRef(generics[i].typeName(), true));
    }

    TypeConstVector dummyConstraints;
    FunctionSignature defApplySign(true,
                                   String("apply"),
                                   Type::newTypeRef(fullTypeName, genGenerics,
                                                    dummyConstraints, true),
                                   funcParams);

    defType = Type::newClass(fullTypeName, generics,
                             inheritsFrom,
                             defApplySign,
                             protoSignatures);
  }
  else {
    defType = Type::newType(fullTypeName, generics,
                            inheritsFrom,
                            protoSignatures);
  }


  fCurrentGenericTypes.clear();

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullTypeName))
    return NULL;

  fScope->registerType(expr.srcpos(), fullTypeName, defType);

  return new TypeDefNode(expr.srcpos(),
                         fullTypeName,
                         isClass,
                         defType,
                         defaultApplyParams,
                         slotDefs,
                         reqProtocol,
                         onExprs);
}


AptNode*
SecondPass::parseAliasDef(const Token& expr, size_t ofs, bool isLocal)
{
  assert(fCurrentGenericTypes.empty());
  TSharedGenericScopeHelper SharedTable(fSharedGenericTable);

  assert(expr.isSeq());
  assert(expr.count() > ofs + 3);
  assert(expr[ofs] == Compiler::aliasToken);
  assert(expr[ofs + 1] == kSymbol);

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
      assert(generics[i].isRef());
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
  assert(expr.isSeq());
  assert(expr.count() >= ofs + 2);
  assert(expr[ofs] == Compiler::slotToken);

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
    initExpr = parseExpr(seq[ofs + 1]);
    ofs += 2;
  }

  unsigned int slotFlags = kSimpleSlot;
  if (ofs < seq.size() && seq[ofs] == kSemicolon) {
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
      else if (seq[ofs] == Compiler::observableToken) {
        slotFlags |= kObservableSlot;
      }
      else if (seq[ofs] == Compiler::autoToken) {
        slotFlags |= kAutoSlot;
      }
      else {
        assert(seq[ofs] == kSymbol);
        errorf(seq[ofs].srcpos(), E_UnknownSlotFlag,
               "Unknown slot flag '%s' ignored", (const char*)StrHelper(seq[ofs].toString()));
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
      initExpr = parseExpr(lastInitToken);
  }
  else {
    errorf(srcpos, E_EnumNotBaseType, "Enum init value is not of base type");
  }

  return initExpr.release();
}


AptNode*
SecondPass::parseEnumDef(const Token& expr, size_t ofs, bool isLocal)
{
  assert(fCurrentGenericTypes.empty());

  assert(expr.isSeq());
  assert(expr.count() == ofs + 3 || expr.count() == ofs + 5);
  assert(expr[ofs] == Compiler::enumToken);
  assert(expr[ofs + 1] == kSymbol);

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
    baseType = Type::newTypeRef(Names::kIntTypeName, true);

  if (!baseType.isBaseType()) {
    errorf(expr.srcpos(), E_EnumNotBaseType, "Enum base is not a base type.");
    return NULL;
  }

  assert(expr[ofs].isNested());
  assert(expr[ofs].leftToken() == kBraceOpen);

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
      assert(enumVal.count() >= 3);
      assert(enumVal[0] == kSymbol);

      sym = enumVal[0].idValue();

      assert(enumVal[1] == kAssign);
      initExpr = parseExpr(enumVal[2]);
      lastInitToken = enumVal[2];
      lastInitPos = enumVal[2].srcpos();
    }
    else if (enumVal == kSymbol) {
      sym = enumVal.idValue();
      initExpr = nextEnumInitValue(lastInitPos, enumVal, baseType,
                                   lastInitToken);
    }
    else
      assert(0);

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
                                   dummyGenerics, constraints, true);

  fScope->registerType(expr.srcpos(), fullEnumName, enumType);

  // there's no apt node to generate here.
  return NULL;
}


AptNode*
SecondPass::parseMeasureDef(const Token& expr, size_t ofs, bool isLocal)
{
  assert(fCurrentGenericTypes.empty());

  assert(expr.isSeq());
  assert(expr.count() == ofs + 5);
  assert(expr[ofs] == Compiler::measureToken);
  assert(expr[ofs + 1] == kSymbol);
  assert(expr[ofs + 2].isNested());
  assert(expr[ofs + 2].count() == 1);
  assert(expr[ofs + 2][0] == kSymbol);
  assert(expr[ofs + 3] == kColon);

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
  NodeList dummyReqProtocol;
  NodeList dummyOnExprs;

  return new TypeDefNode(expr.srcpos(),
                         fullTypeName,
                         true,     // is instantiatable
                         defMeasureType,
                         dummyApplyParams,
                         dummySlotDefs,
                         dummyReqProtocol,
                         dummyOnExprs);
}


AptNode*
SecondPass::parseUnitDef(const Token& expr, size_t ofs, bool isLocal)
{
  assert(fCurrentGenericTypes.empty());

  assert(expr.isSeq());
  assert(expr.count() >= ofs + 6);
  assert(expr[ofs] == Compiler::unitToken);
  assert(expr[ofs + 1] == kSymbol);
  assert(expr[ofs + 2] == kMapTo);
  assert(expr[ofs + 4].isNested());
  assert(expr[ofs + 4].count() == 1);

  ofs++;

  const TokenVector& seq = expr.children();
  String unitName = seq[ofs].idValue();
  String fullUnitName = qualifyId(currentModuleName(), unitName);
  ofs++;
  assert(seq[ofs] == kMapTo);
  ofs++;

  TypeUnit baseUnit = fScope->lookupUnit(seq[ofs].idValue(), true);
  if (!baseUnit.isDef()) {
    error(seq[ofs].srcpos(), E_UndefinedUnit,
          String("Undefined unit: ") + seq[ofs].idValue());
    return NULL;
  }
  ofs++;
  assert(seq[ofs].isNested());

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
  assert(ofs >= 1);
  assert(ofs < expr.count());
  assert(expr[ofs] == kSymbol);

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
      initExpr = parseExpr(seq[ofs + 1]);
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
  assert(seq[ofs].isNested());

  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Function);
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
      data.fBody = parseExpr(seq[ofs]);
    ofs++;
  }
}


AptNode*
SecondPass::parseFunctionDef(const Token& expr, size_t ofs, bool isLocal,
                             const String& linkage)
{
  assert(expr.isSeq());
  assert(expr.count() >= ofs + 2);

  FundefClauseData data;

  if (expr[ofs] == Compiler::genericToken) {
    data.fFlags |= kFuncIsGeneric;
    ofs++;
  }

  assert(expr[ofs] == kSymbol);
  String sym = expr[ofs].idValue();
  if (isLocal && isQualified(sym)) {
    errorf(expr[ofs].srcpos(), E_QualifiedLocalSym,
           "Local symbol in definition must not be qualified.  "
           "Ignore namespace");
    sym = baseName(sym);
  }

  ofs++;

  const TokenVector& seq = expr.children();
  parseFundefClause(seq, ofs, data);

  String fullFuncName = ( isLocal
                          ? sym
                          : qualifyId(currentModuleName(), sym) );

  if (fScope->checkForRedefinition(expr.srcpos(),
                                   Scope::kNormal, fullFuncName))
    return NULL;

  Ptr<FuncDefNode> func = new FuncDefNode(expr.srcpos(),
                                          fullFuncName,
                                          data.fFlags,
                                          data.fParams,
                                          data.fType,
                                          data.fBody);
  func->setLinkage(linkage);
  fScope->registerFunction(expr.srcpos(), fullFuncName, func);

  return func.release();
}


AptNode*
SecondPass::newDefNode(AptNode* node, bool isLet)
{
  if (isLet)
    return new LetNode(node);
  else
    return new DefNode(node);
}


AptNode*
SecondPass::parseDef(const Token& expr, bool isLocal)
{
  assert(expr.count() >= 2);
  assert(expr[0] == kLetId || expr[0] == kDefId);

  String linkage;

  size_t ofs = 1;
  if (expr[1].isSeq() && expr[1].count() == 2 && expr[1][0] == kExternId) {
    const TokenVector& seq = expr[1].children();
    assert(seq[1].isNested());
    assert(seq[1].leftToken() == kParanOpen);
    assert(seq[1].rightToken() == kParanClose);
    assert(seq[1].count() == 1);
    assert(seq[1][0] == kString);

    linkage = seq[1][0].stringValue();

    ofs++;
  }


  if (expr[ofs] == Compiler::typeToken) {
    assert(linkage.isEmpty());
    if (isLocal) {
      errorf(expr.srcpos(), E_LocalTypeDef,
             "Local type definitions are not allowed");
      return NULL;
    }
    return parseTypeDef(expr, ofs, false);
  }

  else if (expr[ofs] == Compiler::classToken) {
    assert(linkage.isEmpty());
    if (isLocal) {
      errorf(expr.srcpos(), E_LocalTypeDef,
             "Local type definitions are not allowed");
      return NULL;
    }
    return parseTypeDef(expr, ofs, true);
  }

  else if (expr[ofs] == Compiler::aliasToken) {
    assert(linkage.isEmpty());
    return parseAliasDef(expr, ofs, isLocal);
  }

  else if (expr[ofs] == Compiler::slotToken) {
    assert(!isLocal);
    assert(linkage.isEmpty());
    return parseSlotDef(expr, ofs);
  }

  else if (expr[ofs] == Compiler::enumToken) {
    assert(linkage.isEmpty());
    return parseEnumDef(expr, ofs, isLocal);
  }
  else if (expr[ofs] == Compiler::measureToken) {
    assert(linkage.isEmpty());
    return parseMeasureDef(expr, ofs, isLocal);
  }

  else if (expr[ofs] == Compiler::unitToken) {
    assert(linkage.isEmpty());
    return parseUnitDef(expr, ofs, isLocal);
  }

  else if (expr[ofs] == Compiler::constToken) {
    assert(linkage.isEmpty());
    return parseVarDef(expr, kConstVar, ofs + 1, isLocal, String());
  }
  else if (expr[ofs] == Compiler::fluidToken) {
    assert(linkage.isEmpty());
    return parseVarDef(expr, kFluidVar, ofs + 1, isLocal, String());
  }
  else if (expr[ofs] == Compiler::configToken) {
    assert(linkage.isEmpty());
    return parseVarDef(expr, kConfigVar, ofs + 1, isLocal, String());
  }

  else if (expr[ofs] == Compiler::genericToken) {
    assert(linkage.isEmpty());
    return parseFunctionDef(expr, ofs, isLocal, String());
  }

  else if (expr[ofs] == Compiler::charToken) {
    assert(0);
    // should never come here actually
    return NULL;
  }

  else if (expr[ofs] == Compiler::macroToken) {
    assert(0);
    // should never come here actually
    return NULL;
  }

  else if (expr[ofs] == kSymbol) {
    if (expr.count() >= ofs + 2) {
      if (expr[ofs + 1].isNested())
        return parseFunctionDef(expr, ofs, isLocal, linkage);

      assert(expr[ofs + 1] == kAssign || expr[ofs + 1] == kColon);
      return parseVarDef(expr, kNormalVar, ofs, isLocal, linkage);
    }

    return parseVarDef(expr, kNormalVar, ofs, isLocal, linkage);
  }

  errorf(expr[ofs].srcpos(), 0, "Unexpected token: %s\n",
         (const char*)StrHelper(expr[ofs].toString()));
  assert(0);

  return NULL;
}


AptNode*
SecondPass::parseIf(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.count() >= 3);
  assert(expr[0] == kIfId);
  assert(expr[1].isNested());
  assert(expr[1].count() > 0);

  Ptr<AptNode> test = parseTokenVector(expr[1].children());
  Ptr<AptNode> consequent = parseExpr(expr[2]);
  Ptr<AptNode> alternate;

  if (expr.count() >= 4) {
    assert(expr[3] == kElseId);
    alternate = parseExpr(expr[4]);
  }

  return new IfNode(expr.srcpos(), test, consequent, alternate);
}


AptNode*
SecondPass::parseParameter(const Token& expr)
{
  if (expr == kSymbol)
    return new ParamNode(expr.srcpos(),
                         String(), expr.idValue(), kPosArg, Type(), NULL);
  assert(expr.isSeq());
  assert(expr.count() > 0);

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

    assert(expr.count() >= 2);
    ofs++;

    paramType = kNamedArg;
  }

  assert(seq[ofs] == kSymbol);

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
      assert(ofs + 1 < expr.count());

      if (!fCompiler->isParsingInterface())
        initExpr = parseExpr(seq[ofs + 1]);
      ofs += 2;

      paramType = kNamedArg;
      if (key.isEmpty())
        key = sym;
    }
    else if (seq[ofs] == kEllipsis) {
      assert(key.isEmpty());
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
  assert(expr.count() == 4);
  assert(expr[0] == kOnId);
  assert(expr[1] == kSymbol);
  assert(expr[2].isNested());

  NodeList params;
  parseParameters(&params, expr[2].children());

  return new OnNode(expr.srcpos(), expr[1].idValue(), params,
                    parseExpr(expr[3]));
}


AptNode*
SecondPass::parseClosure(const Token& expr)
{
  assert(expr.isSeq());
  assert(expr.count() >= 3);
  assert(expr[0] == kFunctionId);
  assert(expr[1].isNested());

  size_t ofs = 1;
  assert(expr[ofs].isNested());

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

  assert(ofs < expr.count());
  Ptr<AptNode> body = parseExpr(expr[ofs]);;

  return new FunctionNode(expr.srcpos(), params, type, body);
}


AptNode*
SecondPass::parseBinary(const Token& expr)
{
  assert(expr.count() >= 3);

  switch (expr[1].tokenType()) {
  case kAssign:
    {
      Ptr<AptNode> lvalue = parseExpr(expr[0]);
      Ptr<AptNode> rvalue = parseExpr(expr[2]);
      return new AssignNode(expr.srcpos(), lvalue, rvalue);
    }

  case kRange:
    if (expr.count() >= 5) {
      assert(expr[3] == kBy);
      Ptr<AptNode> from = parseExpr(expr[0]);
      Ptr<AptNode> to   = parseExpr(expr[2]);
      Ptr<AptNode> step = parseExpr(expr[4]);
      return new RangeNode(expr.srcpos(), from, to, step);
    }
    else {
      Ptr<AptNode> from = parseExpr(expr[0]);
      Ptr<AptNode> to   = parseExpr(expr[2]);
      return new RangeNode(expr.srcpos(), from, to, NULL);
    }

  case kThenId:
    if (expr.count() >= 5) {
      assert(expr[3] == kWhileId);
      Ptr<AptNode> first = parseExpr(expr[0]);
      Ptr<AptNode> step = parseExpr(expr[2]);
      Ptr<AptNode> test = parseExpr(expr[4]);
      return new ThenWhileNode(expr.srcpos(), first, step, test);
    }
    else {
      Ptr<AptNode> first = parseExpr(expr[0]);
      Ptr<AptNode> step = parseExpr(expr[2]);
      return new ThenWhileNode(expr.srcpos(), first, step, NULL);
    }

  case kAs:
    {
      Ptr<AptNode> base = parseExpr(expr[0]);
      Type type = parseTypeSpec(expr[2]);

      return new CastNode(expr.srcpos(), base, type);
    }
    break;

  default:
    ;
  }

  Ptr<AptNode> left = parseExpr(expr[0]);
  Ptr<AptNode> right = parseExpr(expr[2]);
  return new BinaryNode(expr.srcpos(),
                        left,
                        tokenTypeToOperator(expr[1].tokenType()),
                        right);
}


AptNode*
SecondPass::parseFunCall(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.isSeq());
  assert(expr.count() == 2);
  assert(expr[1].isNested());
  assert(expr[1].leftToken() == kParanOpen);
  assert(expr[1].rightToken() == kParanClose);

  Ptr<AptNode> first = parseExpr(expr[0]);
  Ptr<ApplyNode> funcall = new ApplyNode(expr.srcpos(), first);

  const TokenVector& seq = expr[1].children();
  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    Ptr<AptNode> arg;
    if (seq[i] == kKeyarg) {
      assert(i + 1 < seq.size());

      Ptr<AptNode> value = parseExpr(seq[i + 1]);
      arg = new KeyargNode(seq[i].srcpos(), seq[i].idValue(), value);
      i++;
    }
    else
      arg = parseExpr(seq[i]);

    funcall->appendNode(arg);
  }

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
  assert(token.count() == 3);

  SrcPos srcpos = token.srcpos();
  Token thenWhileExpr = token[2];
  assert(thenWhileExpr.count() == 3 || thenWhileExpr.count() == 5);
  assert(thenWhileExpr[1] == kThenId);

  Ptr<AptNode> firstNode = parseExpr(thenWhileExpr[0]);
  Ptr<AptNode> thenNode = parseExpr(thenWhileExpr[2]);

  Token iteratorVarSym = token[0].isSeq() ? token[0][0] : token[0];

  Ptr<AptNode> vardef = new VardefNode(srcpos,
                                       iteratorVarSym.idValue(), kNormalVar, true, Type(),
                                       firstNode);
  Ptr<AptNode> iteratorDefNode = new LetNode(vardef);
  loopDefines->push_back(iteratorDefNode);

  Ptr<AptNode> nextNode = new AssignNode(srcpos,
                                         new SymbolNode(srcpos,
                                                        iteratorVarSym.idValue()),
                                         thenNode);
  stepExprs->push_back(nextNode);


  if (thenWhileExpr.count() == 5) {
    assert(thenWhileExpr[3] == kWhileId);

    Ptr<AptNode> whileNode = parseExpr(thenWhileExpr[4]);
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
  assert(token.count() == 3);
  assert(token[2].isRange());

  SrcPos srcpos = token.srcpos();

  Ptr<AptNode> beginRangeNode;
  Ptr<AptNode> endRangeNode;

  // determine loop direction
  Ptr<AptNode> stepValueNode;
  RangeForClauseCountDir direct = kRangeUnknown;
  if (token[2].count() == 3) {
    direct = kRangeUpwards;
    stepValueNode = new IntNode(srcpos, 1, false, Type::newInt());
  }
  else if (token[2].count() == 5) {
    Token byToken = token[2][4];
    if (byToken.isInt() || byToken.isReal() || byToken.isRational() ||
        byToken.isChar())
    {
      direct = byToken.isNegative() ? kRangeDownwards : kRangeUpwards;
      stepValueNode = parseExpr(byToken);
    }
    else {
      direct = kRangeUnknown;

      Ptr<AptNode> tmpStepNode = parseExpr(byToken);
      // let _step = 2
      Token tmpStepSym = Token::newUniqueSymbolToken(srcpos, "step");
      Ptr<AptNode> vardef = new VardefNode(srcpos,
                                           tmpStepSym.idValue(), kNormalVar, true, Type(),
                                           tmpStepNode);
      Ptr<AptNode> endStepNode = new LetNode(vardef);
      loopDefines->push_back(endStepNode);

      stepValueNode = new SymbolNode(srcpos, tmpStepSym.idValue());
    }
  }


  //-------- determine best end node representation
  Token beginToken = token[2][0];
  if (beginToken.isLit()) {
    beginRangeNode = parseExpr(beginToken);
  }
  else {
    Ptr<AptNode> tmpEndNode = parseExpr(beginToken);

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    Ptr<AptNode> vardef = new VardefNode(srcpos,
                                         tmpEndRangeSym.idValue(), kNormalVar,
                                         true, Type(), tmpEndNode);
    Ptr<AptNode> endRangeDefNode = new LetNode(vardef);
    loopDefines->push_back(endRangeDefNode);

    beginRangeNode = new SymbolNode(srcpos, tmpEndRangeSym.idValue());
  }


  //-------- determine best end node representation
  Token endToken = token[2][2];
  if (endToken.isLit()) {
    endRangeNode = parseExpr(endToken);
  }
  else {
    Ptr<AptNode> tmpEndNode = parseExpr(endToken);

    // let _end = 100
    Token tmpEndRangeSym = Token::newUniqueSymbolToken(srcpos, "end");
    Ptr<AptNode> vardef = new VardefNode(srcpos,
                                         tmpEndRangeSym.idValue(), kNormalVar,
                                         true, Type(), tmpEndNode);
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
                                       true, stepVarType,
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
                     absItVarSym.idValue(), kNormalVar, true, Type(),
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
                     absMaxEndSym.idValue(), kNormalVar, true, Type(),
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
                     absStepVarSym.idValue(), kNormalVar, true, Type(),
                     new IfNode(srcpos,
                                new BinaryNode(srcpos,
                                               stepValueNode->clone(),
                                               kOpLess,
                                               endRangeNode->clone()),
                                new NegateNode(srcpos,
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
  assert(token.count() >= 3);

  SrcPos srcpos = token.srcpos();

  Token sym = Token::newUniqueSymbolToken(srcpos, "seq");

  // ------------------------------ let _seq = names
  Type loopType;                // TODO
  Ptr<AptNode> seqInitNode = parseExpr(token[2]);
  Ptr<AptNode> seqInitVardef = new VardefNode(srcpos,
                                              sym.idValue(), kNormalVar, true, loopType,
                                              seqInitNode);
  Ptr<AptNode> loopDefNode = new LetNode(seqInitVardef);
  loopDefines->push_back(loopDefNode);


  // ------------------------------ let name = lang|unspecified
  Token stepSym = token[0].isSeq() ? token[0][0] : token[0];
  assert(stepSym == kSymbol);
  Type stepType;                // TODO
  Ptr<AptNode> stepSymVardef = new VardefNode(srcpos,
                                              stepSym.idValue(), kNormalVar,
                                              true, stepType,
                                              new SymbolNode(srcpos,
                                                             String("lang|unspecified")));
  Ptr<AptNode> stepDefNode = new LetNode(stepSymVardef);
  loopDefines->push_back(stepDefNode);

  // ------------------------------ if (_seq.end?)
  Ptr<ApplyNode> testNode = new ApplyNode(srcpos,
                                          new SymbolNode(srcpos,
                                                         String("end?")));
  testNode->appendNode(new SymbolNode(srcpos, sym.idValue()));

  // --- then false
  Ptr<AptNode> consNode = new BoolNode(srcpos, false);

  // --- else { name = _seq.next true }
  Ptr<BlockNode> altNode = new BlockNode(srcpos);

  Ptr<AptNode> stepVarNode = new SymbolNode(srcpos, stepSym.idValue());
  Ptr<ApplyNode> nextSeqNode = new ApplyNode(srcpos,
                                             new SymbolNode(srcpos,
                                                            String("next")));
  nextSeqNode->appendNode(new SymbolNode(srcpos, sym.idValue()));

  Ptr<AptNode> stepNextNode = new AssignNode(srcpos,
                                             stepVarNode, nextSeqNode);
  altNode->appendNode(stepNextNode);
  altNode->appendNode(new BoolNode(srcpos, true));

  Ptr<AptNode> ifNode = new IfNode(srcpos, testNode, consNode, altNode);

  testExprs->push_back(ifNode);
}


AptNode*
SecondPass::parseFor(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());

  assert(expr.isSeq());
  assert(expr.count() == 3 || expr.count() == 5);
  assert(expr[0] == kForId);
  assert(expr[1].isNested());
  assert(heaImplies(expr.count() == 5, expr[3] == kElseId));

  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);

  Ptr<AptNode> body = parseExpr(expr[2]);
  Ptr<AptNode> alternate;

  if (expr.count() == 5)
    alternate = parseExpr(expr[4]);

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
      Ptr<AptNode> exprNode = parseExpr(seq[i]);
      testExprs.push_back(exprNode);
    }
  }

  const bool requiresReturnValue = alternate != NULL || !testExprs.empty();

  Token returnSym = Token::newUniqueSymbolToken(expr.srcpos(), "return");

  if (requiresReturnValue) {
    if (alternate == NULL)
      alternate = new SymbolNode(expr.srcpos(), String("lang|unspecified"));

    Type retType;               // TODO?
    Ptr<AptNode> returnVardef = new VardefNode(expr.srcpos(),
                                               returnSym.idValue(), kNormalVar,
                                               true, retType, alternate);
    Ptr<AptNode> defReturnNode = new LetNode(returnVardef);
    loopDefines.push_back(defReturnNode);
  }

  Ptr<BlockNode> block = new BlockNode(expr.srcpos());
  block->appendNodes(loopDefines);

  Ptr<AptNode> testNode;
  int nodeCount = 0;
  for (size_t i = 0; i < testExprs.size(); i++) {
    if (nodeCount > 1) {
      Ptr<BinaryNode> prevBin = dynamic_cast<BinaryNode*>(testNode.obj());
      assert(prevBin != NULL);
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

  Ptr<BlockNode> bodyNode = new BlockNode(expr.srcpos());

  if (requiresReturnValue) {
    Ptr<AptNode> retNode = new SymbolNode(expr.srcpos(),
                                          returnSym.idValue());
    Ptr<AptNode> saveRetNode = new AssignNode(expr.srcpos(),
                                              retNode, body);
    bodyNode->appendNode(saveRetNode);
  }
  else
    bodyNode->appendNode(body);
  bodyNode->appendNodes(stepExprs);

  Ptr<AptNode> returnNode;
  if (requiresReturnValue)
    returnNode = new SymbolNode(expr.srcpos(), returnSym.idValue());

  Ptr<WhileNode> whileNode = new WhileNode(expr.srcpos(), testNode, bodyNode);
  block->appendNode(whileNode);

  if (returnNode != NULL)
    block->appendNode(returnNode);

  return block.release();
}


//----------------------------------------------------------------------------

AptNode*
SecondPass::parseSelect(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());

  assert(expr.isSeq());
  assert(expr.count() == 3);
  assert(expr[0] == kSelectId);
  assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

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
  assert(args.size() > 0);

  Ptr<AptNode> testNode = parseExpr(args[0]);
  Ptr<AptNode> comparatorNode;

  if (args.size() > 2) {
    assert(args[1] == kComma);
    comparatorNode = parseExpr(args[2]);
  }

  Ptr<SelectNode> selectNode = new SelectNode(expr.srcpos(),
                                              testNode,
                                              comparatorNode);

  const TokenVector& testMappings = expr[2].children();
  for (size_t i = 0; i < testMappings.size(); i++) {
    const Token& testToken = testMappings[i];
    assert(testToken.isSeq());
    assert(testToken.count() == 4 || testToken.count() == 3);
    assert(testToken[0] == kPipe);

    if (testToken.count() == 4) {
      assert(testToken[2] == kMapTo);

      NodeList testValueNodes;
      if (testToken[1].isSeq() &&
          !testToken[1].isBinarySeq() &&
          !testToken[1].isTernarySeq())
      {
        const TokenVector& testValues = testToken[1].children();
        for (size_t j = 0; j < testValues.size(); j++) {
          if (testValues[j] == kComma)
            continue;
          Ptr<AptNode> testValueNode = parseExpr(testValues[j]);
          if (testValueNode != NULL)
            testValueNodes.push_back(testValueNode);
        }
      }
      else {
        Ptr<AptNode> testValueNode = parseExpr(testToken[1]);
        if (testValueNode != NULL)
          testValueNodes.push_back(testValueNode);
      }

      Ptr<AptNode> consqExpr = parseExpr(testToken[3]);
      if (consqExpr != NULL)
        selectNode->addMapping(testValueNodes, consqExpr);
    }
    else if (testToken.count() == 3) {
      assert(testToken[1] == kElseId);

      Ptr<AptNode> consqExpr = parseExpr(testToken[2]);
      if (consqExpr != NULL)
        selectNode->addElseMapping(consqExpr);
    }
  }

  return selectNode.release();
}


AptNode*
SecondPass::parseChainSelect(const Token& expr)
{
  assert(expr[1].count() == 0);

  Ptr<AptNode> resultNode;
  Ptr<IfNode> lastNode;

  const TokenVector& testMappings = expr[2].children();
  for (size_t i = 0; i < testMappings.size(); i++) {
    const Token& testToken = testMappings[i];
    assert(testToken.isSeq());
    assert(testToken.count() == 4 || testToken.count() == 3);
    assert(testToken[0] == kPipe);

    if (testToken.count() == 4) {
      assert(testToken[2] == kMapTo);

      Ptr<AptNode> testValueNode = parseExpr(testToken[1]);
      if (testValueNode != NULL) {

        Ptr<AptNode> consqNode = parseExpr(testToken[3]);

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
      assert(testToken[1] == kElseId);

      Ptr<AptNode> consqNode = parseExpr(testToken[2]);
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
  assert(!fCompiler->isParsingInterface());

  assert(expr.isSeq());
  assert(expr.count() == 3);
  assert(expr[0] == kMatchId);
  assert(expr[1].isNested() && expr[1].leftToken() == kParanOpen);
  assert(expr[2].isNested() && expr[2].leftToken() == kBraceOpen);

  const TokenVector& args = expr[1].children();
  assert(args.size() > 0);

  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);

  Ptr<BlockNode> block = new BlockNode(expr.srcpos());

  Ptr<AptNode> exprNode = parseExpr(args[0]);
  Token tmpValueSym = Token::newUniqueSymbolToken(expr.srcpos(), "match");
  Type tempType;                // TODO?
  Ptr<AptNode> tmpVarDef = new VardefNode(expr.srcpos(),
                                          tmpValueSym.idValue(),
                                          kNormalVar,
                                          true,
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
    assert(typeMapping.isSeq());
    assert(typeMapping.count() == 4);
    assert(typeMapping[0] == kPipe);
    assert(typeMapping[1].isSeq());
    assert(typeMapping[1].count() >= 2);
    assert(typeMapping[2] == kMapTo);

    {
      ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);

      Ptr<BlockNode> localBlock = new BlockNode(typeMapping[3].srcpos());

      String  varName;
      Type    varType;
      if (typeMapping[1].count() == 3) {
        assert(typeMapping[1][0] == kSymbol);
        assert(typeMapping[1][1] == kColon);

        SrcPos sympos = typeMapping[1][0].srcpos();

        varName = typeMapping[1][0].idValue();
        varType = parseTypeSpec(typeMapping[1][2]);

        Ptr<AptNode> initVal = new CastNode(typeMapping[0].srcpos(),
                                            tmpValueNode->clone(),
                                            varType);

        Ptr<VardefNode> localVar = new VardefNode(sympos,
                                                  varName, kNormalVar, true,
                                                  varType, initVal);
        localBlock->appendNode(new LetNode(localVar));
      }
      else {
        assert(typeMapping[1][0] == kColon);
        varType = parseTypeSpec(typeMapping[1][1]);
      }

      Ptr<AptNode> consqNode = parseExpr(typeMapping[3]);
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
SecondPass::parseTypeExpr(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.isSeq());
  assert(expr.count() == 2);

  if (expr[0] == kSymbol) {
    TypeVector genericArgs;
    String symbol = expr[0].idValue();

    if (expr[1].leftToken() == kGenericOpen) {
      parseTypeVector(&genericArgs, expr[1]);
      return new SymbolNode(expr.srcpos(), symbol, genericArgs);
    }
    else if (expr[1].leftToken() == kBracketOpen) {
      return new ArrayTypeNode(expr.srcpos(),
                               new SymbolNode(expr.srcpos(), symbol));
    }
  }
  else if (expr[0].isSeq()) {
    assert(expr[0].count() == 2);
    assert(expr[0][0] == kSymbol || expr[0][0].isSeq());
    assert(expr[1].isNested());
    assert(expr[1].leftToken() == kBracketOpen);

    Ptr<AptNode> typeNode = parseTypeExpr(expr[0]);
    if (typeNode != NULL) {
      return new ArrayTypeNode(expr.srcpos(), typeNode);
    }
  }

  fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
          (const char*)StrHelper(expr.toString()),
          __FILE__, __LINE__);
  assert(0);
  return NULL;
}


//------------------------------------------------------------------------------

AptNode*
SecondPass::parseTokenVector(const TokenVector& seq)
{
  if (!seq.empty())
    return parseSeq(Token() << seq);
  return NULL;
}


AptNode*
SecondPass::parseUnitNumber(const Token& expr)
{
  assert(expr.count() == 3);
  assert(expr[1] == kQuote);
  assert(expr[2] == kSymbol);

  Ptr<AptNode> value = parseExpr(expr[0]);

  TypeUnit unit = fScope->lookupUnit(expr[2].idValue(), true);
  if (unit.isDef()) {
    return new UnitConstNode(expr.srcpos(), value, unit);
  }
  else {
    error(expr[2].srcpos(), E_UndefinedUnit,
          String("Undefined unit: ") + expr[2].idValue());
    return NULL;
  }
}


AptNode*
SecondPass::parseSeq(const Token& expr)
{
  assert(expr.isSeq());
  if (expr.isEmpty())
    return NULL;

  Token first = expr[0];
  if (first == kModuleId)
    return parseModule(expr);
  else if (first == kExportId)
    return parseExport(expr);
  else if (first == kImportId)
    return parseImport(expr);
  else if (first == kDefId || first == kLetId) {
    Ptr<AptNode> node = parseDef(expr, first == kLetId);
    if (node != NULL)
      return newDefNode(node, first == kLetId);
    return NULL;
  }
  else if (first == kIfId)
    return parseIf(expr);
  else if (first == kOnId)
    return parseOn(expr);
  else if (first == kFunctionId)
    return parseClosure(expr);
  else if (first == kForId)
    return parseFor(expr);
  else if (first == kSelectId)
    return parseSelect(expr);
  else if (first == kMatchId)
    return parseMatch(expr);
  else if (expr.isBinarySeq() || expr.isTernarySeq())
    return parseBinary(expr);
  else if (first == kMinus) {
    assert(expr.count() == 2);
    Ptr<AptNode> exprNode = parseExpr(expr[1]);
    return new NegateNode(expr.srcpos(), exprNode);
  }
  else if (expr.count() == 2) {
    if (expr[1].isNested()) {
      if (expr[1].leftToken() == kParanOpen)
        return parseFunCall(expr);
      else if (expr[0] == kSymbol &&
               expr[1].leftToken() == kGenericOpen)
        return parseTypeExpr(expr);
      else if ((expr[0] == kSymbol || expr[0].isSeq()) &&
               expr[1].leftToken() == kBracketOpen)
        return parseTypeExpr(expr);
      else {
        fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
                (const char*)StrHelper(expr.toString()),
                __FILE__, __LINE__);
        assert(0);              // TODO
      }
    }
  }
  else if (expr.count() == 3) {
    if (expr[0].isNumber() && expr[1] == kColon) {
      switch (expr[0].tokenType()) {
      case kInt:
        return parseIntNumber(expr);
      case kRational:
        return parseRationalNumber(expr);
      case kReal:
        return parseRealNumber(expr);
      default:
        printf("%d\n", expr.tokenType());
        assert(0);
        return NULL;
      }
    }
    else if (expr[0].isNumber() && expr[1] == kQuote) {
      return parseUnitNumber(expr);
    }
    else if (expr[1] == kRange) {
      return parseBinary(expr);
    }
    else {
      fprintf(stderr, "UNEXPECTED DEXPR: %s (%s %d)\n",
              (const char*)StrHelper(expr.toString()),
              __FILE__, __LINE__);
      assert(0);              // TODO
    }
  }
  else if (expr.count() == 4) {
    if (expr[0] == kExtendId)
      return parseExtend(expr);
  }

  return parseExpr(expr[0]);
}


namespace heather
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
  assert(!fCompiler->isParsingInterface());
  assert(expr.isNested());
  assert(expr.leftToken() == kBraceOpen);
  assert(expr.rightToken() == kBraceClose);

  ScopeHelper scopeHelper(fScope, false, true, kScopeL_Local);

  const TokenVector& seq = expr.children();
  NodeList nodes;
  for (size_t i = 0; i < seq.size(); i++) {
    Ptr<AptNode> item = parseExpr(seq[i]);
    if (item != NULL)
      nodes.push_back(item);
  }

  if (nodes.size() == 0) {
    return new SymbolNode(expr.srcpos(), String("lang|unspecified"));
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
  assert(!fCompiler->isParsingInterface());
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralVectorOpen);
  assert(expr.rightToken() == kParanClose);

  Ptr<VectorNode> vector = new VectorNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    Ptr<AptNode> item = parseExpr(seq[i]);
    vector->appendNode(item);
  }

  return vector.release();
}


AptNode*
SecondPass::parseLiteralArray(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralArrayOpen);
  assert(expr.rightToken() == kBracketClose);

  Ptr<ArrayNode> array = new ArrayNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;
    Ptr<AptNode> item = parseExpr(seq[i]);
    array->appendNode(item);
  }

  return array.release();
}


AptNode*
SecondPass::parseLiteralDict(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.isNested());
  assert(expr.leftToken() == kLiteralVectorOpen);
  assert(expr.rightToken() == kParanClose);

  Ptr<DictNode> dict = new DictNode(expr.srcpos());
  const TokenVector& seq = expr.children();

  for (size_t i = 0; i < seq.size(); i++) {
    if (seq[i] == kComma)
      continue;

    assert(seq[i].isBinarySeq(kMapTo));

    Ptr<AptNode> key = parseExpr(seq[i][0]);
    Ptr<AptNode> value = parseExpr(seq[i][2]);

    dict->addPair(key, value);
  }

  return dict.release();
}


AptNode*
SecondPass::parseNested(const Token& expr)
{
  assert(!fCompiler->isParsingInterface());
  assert(expr.isNested());

  switch (expr.leftToken()) {
  case kBraceOpen:
    return parseBlock(expr);

  case kLiteralVectorOpen:
    if (expr.count() > 0 && expr[0].isBinarySeq(kMapTo))
      return parseLiteralDict(expr);
    else
      return parseLiteralVector(expr);

  case kLiteralArrayOpen:
    return parseLiteralArray(expr);

  case kParanOpen:
    assert(expr.count() == 1);
    return parseExpr(expr[0]);

  case kBracketOpen:
  case kGenericOpen:

  default:
    // printf("---> %s\n", (const char*)StrHelper(expr.toString()));
    assert(0);                  // you should not be here.
  }

  return NULL;
}


AptNode*
SecondPass::parseIntNumber(const Token& expr)
{
  if (expr.tokenType() == kInt) {
    return new IntNode(expr.srcpos(), expr.intValue(), expr.isImaginary(),
                       Type::newInt());
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);

    return new IntNode(expr.srcpos(), expr[0].intValue(),
                       expr[0].isImaginary(), type);
  }

  assert(0);
  return NULL;
}


AptNode*
SecondPass::parseRationalNumber(const Token& expr)
{
  if (expr.tokenType() == kRational) {
    return new RationalNode(expr.srcpos(), expr.rationalValue(),
                            expr.isImaginary(), Type::newRational());
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);

    return new RationalNode(expr.srcpos(), expr[0].rationalValue(),
                            expr[0].isImaginary(), type);
  }

  assert(0);
  return NULL;
}


AptNode*
SecondPass::parseRealNumber(const Token& expr)
{
  if (expr.tokenType() == kReal) {
    return new RealNode(expr.srcpos(), expr.realValue(),
                        expr.isImaginary(), Type::newReal());
  }
  else if (expr.isSeq() && expr.count() == 3 &&
           expr[0].isNumber() &&
           expr[1] == kColon)
  {
    Type type = parseTypeSpec(expr[2]);

    return new RealNode(expr.srcpos(), expr[0].realValue(),
                        expr[0].isImaginary(), type);
  }

  assert(0);
  return NULL;
}


AptNode*
SecondPass::parseExpr(const Token& expr)
{
  switch (expr.type()) {
  case kId:
    {
      const AptNode* var = fScope->lookupVar(expr.idValue(), true);
      const VardefNode* vardef = dynamic_cast<const VardefNode*>(var);
      if (vardef != NULL && vardef->isEnum() && vardef->initExpr() != NULL) {
        return vardef->initExpr()->clone();
      }
    }
    return new SymbolNode(expr.srcpos(), expr.idValue());

  case kLit:
    switch (expr.tokenType()) {
    case kBool:
      return new BoolNode(expr.srcpos(), expr.boolValue());
    case kInt:
      return parseIntNumber(expr);
    case kRational:
      return parseRationalNumber(expr);
    case kReal:
      return parseRealNumber(expr);
    case kChar:
      return new CharNode(expr.srcpos(), expr.charValue());
    case kString:
      return new StringNode(expr.srcpos(), expr.stringValue());
    case kKeyword:
      return new KeywordNode(expr.srcpos(), expr.stringValue());

    case kDocString:
      // TODO
      return NULL;

    default:
      assert(0);
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
    return NULL;
  }

  return NULL;
}


AptNode*
SecondPass::parse(const Token& exprs)
{
  assert(exprs.isSeq());

  fRootNode = new CompileUnitNode(SrcPos());
  parseTopExprlist(exprs);

  return fRootNode.release();
}
