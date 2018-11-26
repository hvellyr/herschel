/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "abstrpass.hpp"
#include "ast.hpp"
#include "compilepass.hpp"
#include "compiler.hpp"
#include "port.hpp"
#include "scope.hpp"
#include "token.hpp"
#include "tokenport.hpp"
#include "type.hpp"
#include "typectx.hpp"

#include <list>
#include <map>
#include <memory>
#include <set>


namespace herschel {

//! The second pass in a compile run take the token expressions as returned
//! from the \c FirstPass and translate it into a tree of \c AstNodes.
//! Complicated high level expressions (e.g. \c for()) are transformed into
//! simpler base forms, already.  Some types are assigned (if available).
//! Constructor and access functions for classes and types are generated.
//!
//! Note that on this level no macros exist anymore.
class SecondPass : public AbstractPass {
public:
  SecondPass(Compiler& compiler, std::shared_ptr<Scope> scope);

  //! Transforms the token expressions as returned by the \c FirstPass into
  //! a tree of \c AstNode nodes.
  std::shared_ptr<AstNode> parse(const Token& exprs);

private:
  struct PrimeTuple {
    std::shared_ptr<AstNode> fPrime;
    Type fType;
  };

  NodeList parseExpr(const Token& expr);
  NodeList parseSeq(const Token& expr);

  std::shared_ptr<AstNode> parseModule(const Token& expr);
  std::shared_ptr<AstNode> parseExport(const Token& expr);
  std::shared_ptr<AstNode> parseImport(const Token& expr);


  void parseTopExprlist(const Token& expr);

  NodeList rewriteDefNode(std::shared_ptr<AstNode> node, bool isLet);
  NodeList rewriteDefNodes(const NodeList& nodes, bool isLet);

  NodeList parseDef(const Token& expr, bool isLocal);
  std::shared_ptr<AstNode> parseIf(const Token& expr);
  std::shared_ptr<AstNode> parseFor(const Token& expr);
  std::shared_ptr<AstNode> parseSelect(const Token& expr);
  std::shared_ptr<AstNode> parseMatch(const Token& expr);
  std::shared_ptr<AstNode> parseClosure(const Token& expr);
  std::shared_ptr<AstNode> parseBinary(const Token& expr);
  std::shared_ptr<AstNode> parseFunCall(const Token& expr);
  NodeList parseFunCallArgs(const TokenVector& args);
  std::shared_ptr<AstNode> parseTypeExpr(const Token& expr, bool inArrayType = false);

  NodeList parseTokenVector(const TokenVector& seq);
  void parseParameters(NodeList* parameters, const TokenVector& seq);

  std::shared_ptr<AstNode> parseParameter(const Token& expr);

  NodeList parseTypeDef(const Token& expr, size_t ofs, bool isType, bool isLocal);
  std::shared_ptr<AstNode>
  generateConstructor(const Token& typeExpr, const String& fullTypeName,
                      const Type& defType, const NodeList& defaultApplyParams,
                      const NodeList& slotDefs, const std::vector<PrimeTuple>& primes,
                      const NodeList& onExprs);
  void generatePrimeInits(const SrcPos& srcpos, std::shared_ptr<ListNode> body,
                          const Type& defType, const std::vector<PrimeTuple>& primes,
                          const String& selfParamSym);
  std::shared_ptr<AstNode> findPrimeForType(const Type& reqTypeInit,
                                            const std::vector<PrimeTuple>& primes);
  std::shared_ptr<AstNode> getPrimeForType(const Type& reqTypeInit,
                                           const std::vector<PrimeTuple>& primes,
                                           const String& selfParamSym);

  std::shared_ptr<AstNode> defaultSlotInitValue(const SlotdefNode* slot);
  PrimeTuple parsePrime(const Token& primeToken);

  std::shared_ptr<AstNode> parseAliasDef(const Token& expr, size_t ofs, bool isLocal);
  std::shared_ptr<AstNode> parseSlotDef(const Token& expr, size_t ofs);
  std::shared_ptr<AstNode> parseEnumDef(const Token& expr, size_t ofs, bool isLocal);
  std::shared_ptr<AstNode> nextEnumInitValue(const SrcPos& srcpos,
                                             const Token& enumItemSym,
                                             const Type& baseType, Token& lastInitToken);
  std::shared_ptr<AstNode> parseMeasureDef(const Token& expr, size_t ofs, bool isLocal);
  std::shared_ptr<AstNode> parseUnitDef(const Token& expr, size_t ofs, bool isLocal);
  std::shared_ptr<AstNode> parseVarDef(const Token& expr, VardefFlags flags, size_t ofs,
                                       bool isLocal, const String& linkage);
  NodeList parseFunctionDef(const Token& expr, size_t ofs, bool isLocal,
                            const String& linkage);

  std::shared_ptr<AstNode> parseLiteralArray(const Token& expr);
  std::shared_ptr<AstNode> parseLiteralVector(const Token& expr);
  std::shared_ptr<AstNode> parseLiteralDict(const Token& expr);

  std::shared_ptr<AstNode> newDefNode(std::shared_ptr<AstNode> node, bool isLet);

  std::shared_ptr<AstNode> parseBlock(const Token& expr);
  NodeList parseNested(const Token& expr);

  std::shared_ptr<AstNode> parseExtend(const Token& expr);

  Type parseTypeSpec(const Token& expr, bool forceOpenType = false);
  Type parseTypeSpecImpl(const Token& expr, bool forceOpenType);
  Type parseTypeSpecImpl2(const Token& expr, bool isValue, bool forceOpenType);
  Type parseGroupType(const Token& expr, bool isValue);
  Type rephraseRefType(const SrcPos& srcpos, const Type& inType, bool isValue);

  void parseExtendImpl(NodeList* functions, const Token& expr);


  void transformCollForClause(const Token& token, NodeList* loopDefines,
                              NodeList* testExprs);
  void transformRangeForClause(const Token& token, NodeList* loopDefines,
                               NodeList* testExprs, NodeList* stepExprs);
  void transformExplicitForClause(const Token& token, NodeList* loopDefines,
                                  NodeList* testExprs, NodeList* stepExprs);

  void parseTypeVector(TypeVector* generics, const Token& expr,
                       bool forceOpenType = false);
  void paramsNodeListToType(FunctionParamVector* funcParams, const NodeList& nl) const;

  std::shared_ptr<AstNode> parseIntNumber(const Token& expr);
  std::shared_ptr<AstNode> parseRationalNumber(const Token& expr);
  std::shared_ptr<AstNode> parseRealNumber(const Token& expr);

  std::shared_ptr<AstNode> parseChainSelect(const Token& expr);
  std::shared_ptr<AstNode> parseRealSelect(const Token& expr);

  std::shared_ptr<AstNode> parseUnitNumber(const Token& expr);

  std::shared_ptr<AstNode> generateArrayAlloc(const Token& expr,
                                              std::shared_ptr<AstNode> typeNode);
  std::shared_ptr<AstNode> generateAlloc(const Token& expr, const Type& type);
  std::shared_ptr<AstNode>
  generateInitObjectCall(const SrcPos& srcpos, std::shared_ptr<AstNode> newObjAllocExpr,
                         const Type& type, const TokenVector& argTokens);


  //@{ Parsing functions

  struct FundefClauseData {
    FundefClauseData()
        : fFlags(0)
    {
    }

    NodeList fParams;
    Type fType;
    std::shared_ptr<AstNode> fWhere;
    unsigned int fFlags;
    std::shared_ptr<AstNode> fBody;
  };
  void parseFundefClause(const TokenVector& seq, size_t& ofs, FundefClauseData& data);

  //! indicates whether the node list \p params (a list of ParamNodes)
  //! contains a parameter which is specialized (isSpecArg()).
  bool hasSpecParameters(const NodeList& params) const;

  //! make and register a generic function declaration for name \p sym and
  //! parsed function data \p data.  Since generic functions are never local
  //! or may have a special linkage this can be passed here.
  std::shared_ptr<AstNode> makeGenericFunction(const SrcPos& srcpos, const String& sym,
                                               const FundefClauseData& data);

  //! make a method (i.e. a generic function implementation) for the generic
  //! function called \p sym with parsed function data \p data.
  std::shared_ptr<AstNode> makeMethod(const SrcPos& srcpos, const String& sym,
                                      const FundefClauseData& data);

  //! make a normal function named \p sym with parsed function data \p
  //! data.  The function data must not contain specialized parameters, nor
  //! must the function be flagged as generic.
  std::shared_ptr<AstNode> makeNormalFunction(const SrcPos& srcpos, const String& sym,
                                              const FundefClauseData& data, bool isLocal,
                                              const String& linkage);
  //@}

  Type parseBinaryTypeSpec(const Token& expr, bool forceGeneric, bool isValue);
  Type parseWhereConstraint(const Token& whereConstrSeq);
  void parseWhereClause(const Token& whereSeq);

  Type genericTypeRef(const String& id, bool isValue) const;
  size_t getWhereOfs(const Token& expr) const;
  size_t getWhereOfs(const TokenVector& seq, size_t ofs) const;

  std::shared_ptr<AstNode> constructWhileTestNode(const Token& expr, NodeList& testExprs);

  std::shared_ptr<AstNode> parseSlotAccess(const Token& expr);

  Type getIntType(int bitwidth, bool isSigned) const;


  using TSharedGenericTable = std::map<String, Type>;
  class TSharedGenericScopeHelper {
  public:
    TSharedGenericScopeHelper(TSharedGenericTable& table)
        : fOldTable(table)
        , fOldLoc(table)
    {
      // don't reset the old table.  We keep it as is and simply overwrite
      // it with new values to get a simple read-through mechanism to deeper
      // layers
    }

    ~TSharedGenericScopeHelper() { fOldLoc = fOldTable; }

    TSharedGenericTable fOldTable;
    TSharedGenericTable& fOldLoc;
  };

  std::set<String> fCurrentGenericTypes;
  TSharedGenericTable fSharedGenericTable;
  std::shared_ptr<ListNode> fRootNode;
};


//! \c TokenCompilePass wrapper for the \c SecondPass pass to be used in the
//! process pipeline as second pass.
class NodifyPass : public Token2AstNodeCompilePass {
public:
  NodifyPass(int level, Compiler& compiler, std::shared_ptr<Scope> scope);
  std::shared_ptr<AstNode> doApply(const Token& src) override;
  std::shared_ptr<Scope> currentScope();

private:
  std::shared_ptr<Scope> fScope;
  Compiler& fCompiler;
  std::shared_ptr<SecondPass> fPass;
};

}  // namespace herschel
