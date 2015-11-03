/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "apt.h"
#include "compilepass.h"
#include "compiler.h"
#include "pass.h"
#include "port.h"
#include "refcountable.h"
#include "scope.h"
#include "token.h"
#include "tokenport.h"
#include "type.h"
#include "typectx.h"

#include <set>
#include <list>
#include <map>
#include <memory>


namespace herschel
{
  //--------------------------------------------------------------------------

  //! The second pass in a compile run take the token expressions as returned
  //! from the \c FirstPass and translate it into a tree of \c AptNodes.
  //! Complicated high level expressions (e.g. \c for()) are transformed into
  //! simpler base forms, already.  Some types are assigned (if available).
  //! Constructor and access functions for classes and types are generated.
  //!
  //! Note that on this level no macros exist anymore.

  class SecondPass : public AbstractPass
  {
  public:
    SecondPass(Compiler& compiler, std::shared_ptr<Scope> scope);

    //! Transforms the token expressions as returned by the \c FirstPass into
    //! a tree of \c AptNode nodes.
    std::shared_ptr<AptNode> parse(const Token& exprs);

  private:
    struct PrimeTuple
    {
      std::shared_ptr<AptNode> fPrime;
      Type fType;
    };

    NodeList parseExpr(const Token& expr);
    NodeList parseSeq(const Token& expr);

    std::shared_ptr<AptNode> parseModule(const Token& expr);
    std::shared_ptr<AptNode> parseExport(const Token& expr);
    std::shared_ptr<AptNode> parseImport(const Token& expr);


    void parseTopExprlist(const Token& expr);

    NodeList rewriteDefNode(std::shared_ptr<AptNode> node, bool isLet);
    NodeList rewriteDefNodes(const NodeList& nodes, bool isLet);

    NodeList parseDef(const Token& expr, bool isLocal);
    std::shared_ptr<AptNode> parseIf(const Token& expr);
    std::shared_ptr<AptNode> parseOn(const Token& expr);
    std::shared_ptr<AptNode> parseFor(const Token& expr);
    std::shared_ptr<AptNode> parseSelect(const Token& expr);
    std::shared_ptr<AptNode> parseMatch(const Token& expr);
    std::shared_ptr<AptNode> parseClosure(const Token& expr);
    std::shared_ptr<AptNode> parseBinary(const Token& expr);
    std::shared_ptr<AptNode> parseFunCall(const Token& expr);
    NodeList parseFunCallArgs(const TokenVector& args);
    std::shared_ptr<AptNode> parseTypeExpr(const Token& expr, bool inArrayType = false);

    NodeList parseTokenVector(const TokenVector& seq);
    void parseParameters(NodeList* parameters, const TokenVector& seq);

    std::shared_ptr<AptNode> parseParameter(const Token& expr);

    NodeList parseTypeDef(const Token& expr, size_t ofs, bool isType,
                          bool isLocal);
    std::shared_ptr<AptNode>
    generateConstructor(const Token& typeExpr,
                        const String& fullTypeName,
                        const Type& defType,
                        const NodeList& defaultApplyParams,
                        const NodeList& slotDefs,
                        const std::vector<PrimeTuple>& primes,
                        const NodeList& onExprs);
    void generatePrimeInits(const SrcPos& srcpos,
                            std::shared_ptr<ListNode> body,
                            const Type& defType,
                            const std::vector<PrimeTuple>& primes,
                            const String& selfParamSym);
    std::shared_ptr<AptNode> findPrimeForType(const Type& reqTypeInit,
                                              const std::vector<PrimeTuple>& primes);
    std::shared_ptr<AptNode> getPrimeForType(const Type& reqTypeInit,
                                             const std::vector<PrimeTuple>& primes,
                                             const String& selfParamSym);

    std::shared_ptr<AptNode> defaultSlotInitValue(const SlotdefNode* slot);
    PrimeTuple parsePrime(const Token& primeToken);
    std::vector<SecondPass::PrimeTuple> parseOnAllocExpr(const Token& expr);

    std::shared_ptr<AptNode> parseAliasDef(const Token& expr, size_t ofs, bool isLocal);
    std::shared_ptr<AptNode> parseSlotDef(const Token& expr, size_t ofs);
    std::shared_ptr<AptNode> parseEnumDef(const Token& expr, size_t ofs, bool isLocal);
    std::shared_ptr<AptNode> nextEnumInitValue(const SrcPos& srcpos,
                               const Token& enumItemSym,
                               const Type& baseType, Token& lastInitToken);
    std::shared_ptr<AptNode> parseMeasureDef(const Token& expr, size_t ofs, bool isLocal);
    std::shared_ptr<AptNode> parseUnitDef(const Token& expr, size_t ofs, bool isLocal);
    std::shared_ptr<AptNode> parseVarDef(const Token& expr, VardefFlags flags, size_t ofs,
                         bool isLocal, const String& linkage);
    NodeList parseFunctionDef(const Token& expr, size_t ofs, bool isLocal,
                              const String& linkage);

    std::shared_ptr<AptNode> parseLiteralArray(const Token& expr);
    std::shared_ptr<AptNode> parseLiteralVector(const Token& expr);
    std::shared_ptr<AptNode> parseLiteralDict(const Token& expr);

    std::shared_ptr<AptNode> newDefNode(std::shared_ptr<AptNode> node, bool isLet);

    std::shared_ptr<AptNode> parseBlock(const Token& expr);
    NodeList parseNested(const Token& expr);

    std::shared_ptr<AptNode> parseExtend(const Token& expr);

    Type parseTypeSpec(const Token& expr, bool forceOpenType = false);
    Type parseTypeSpecImpl(const Token& expr, bool forceOpenType);
    Type parseTypeSpecImpl2(const Token& expr, bool isValue, bool forceOpenType);
    Type parseGroupType(const Token& expr, bool isValue);
    Type rephraseRefType(const SrcPos& srcpos, const Type& inType, bool isValue);

    void parseExtendImpl(NodeList* functions, const Token& expr);


    void transformCollForClause(const Token& token,
                                NodeList* loopDefines,
                                NodeList* testExprs);
    void transformRangeForClause(const Token& token,
                                 NodeList* loopDefines,
                                 NodeList* testExprs,
                                 NodeList* stepExprs);
    void transformExplicitForClause(const Token& token,
                                    NodeList* loopDefines,
                                    NodeList* testExprs,
                                    NodeList* stepExprs);

    void parseTypeVector(TypeVector* generics, const Token& expr,
                         bool forceOpenType = false);
    void paramsNodeListToType(FunctionParamVector* funcParams,
                              const NodeList& nl) const;

    std::shared_ptr<AptNode> parseIntNumber(const Token& expr);
    std::shared_ptr<AptNode> parseRationalNumber(const Token& expr);
    std::shared_ptr<AptNode> parseRealNumber(const Token& expr);

    std::shared_ptr<AptNode> parseChainSelect(const Token& expr);
    std::shared_ptr<AptNode> parseRealSelect(const Token& expr);

    std::shared_ptr<AptNode> parseUnitNumber(const Token& expr);

    std::shared_ptr<AptNode> generateArrayAlloc(const Token& expr, std::shared_ptr<AptNode> typeNode);
    std::shared_ptr<AptNode> generateAlloc(const Token& expr, const Type& type);
    std::shared_ptr<AptNode> generateInitObjectCall(const SrcPos& srcpos,
                                    std::shared_ptr<AptNode> newObjAllocExpr,
                                    const Type& type, const TokenVector& argTokens);


    //@{ Parsing functions

    struct FundefClauseData
    {
      FundefClauseData()
        : fFlags(0)
      { }

      NodeList fParams;
      Type fType;
      std::shared_ptr<AptNode> fReify;
      std::shared_ptr<AptNode> fWhere;
      unsigned int fFlags;
      std::shared_ptr<AptNode> fBody;
    };
    void parseFundefClause(const TokenVector& seq, size_t& ofs,
                           FundefClauseData& data);

    //! indicates whether the node list \p params (a list of ParamNodes)
    //! contains a parameter which is specialized (isSpecArg()).
    bool hasSpecParameters(const NodeList& params) const;

    //! make and register a generic function declaration for name \p sym and
    //! parsed function data \p data.  Since generic functions are never local
    //! or may have a special linkage this can be passed here.
    std::shared_ptr<AptNode> makeGenericFunction(const SrcPos& srcpos,
                                 const String& sym,
                                 const FundefClauseData& data);

    //! make a method (i.e. a generic function implementation) for the generic
    //! function called \p sym with parsed function data \p data.
    std::shared_ptr<AptNode> makeMethod(const SrcPos& srcpos, const String& sym,
                        const FundefClauseData& data);

    //! make a normal function named \p sym with parsed function data \p
    //! data.  The function data must not contain specialized parameters, nor
    //! must the function be flagged as generic.
    std::shared_ptr<AptNode> makeNormalFunction(const SrcPos& srcpos, const String& sym,
                                const FundefClauseData& data,
                                bool isLocal,
                                const String& linkage);
    //@}

    Type parseBinaryTypeSpec(const Token& expr, bool forceGeneric,
                             bool isValue);
    Type parseWhereConstraint(const Token& whereConstrSeq);
    void parseWhereClause(const Token& whereSeq);

    Type genericTypeRef(const String& id, bool isValue) const;
    size_t getWhereOfs(const Token& expr) const;
    size_t getWhereOfs(const TokenVector& seq, size_t ofs) const;

    std::shared_ptr<AptNode> constructWhileTestNode(const Token& expr, NodeList& testExprs);

    std::shared_ptr<AptNode> parseSlotAccess(const Token& expr);

    Type getIntType(int bitwidth, bool isSigned) const;


    using TSharedGenericTable = std::map<String, Type>;
    class TSharedGenericScopeHelper
    {
    public:
      TSharedGenericScopeHelper(TSharedGenericTable& table)
        : fOldTable(table),
          fOldLoc(table)
      {
        // don't reset the old table.  We keep it as is and simply overwrite
        // it with new values to get a simple read-through mechanism to deeper
        // layers
      }

      ~TSharedGenericScopeHelper()
      {
        fOldLoc = fOldTable;
      }

      TSharedGenericTable fOldTable;
      TSharedGenericTable& fOldLoc;
    };

    //-------- data member

    std::set<String> fCurrentGenericTypes;
    TSharedGenericTable fSharedGenericTable;
    std::shared_ptr<ListNode> fRootNode;
  };


  //--------------------------------------------------------------------------

  //! \c TokenCompilePass wrapper for the \c SecondPass pass to be used in the
  //! process pipeline as second pass.

  class NodifyPass : public Token2AptNodeCompilePass
  {
  public:
    NodifyPass(int level, Compiler& compiler, std::shared_ptr<Scope> scope);
    virtual std::shared_ptr<AptNode> doApply(const Token& src);
    std::shared_ptr<Scope> currentScope();

  private:
    std::shared_ptr<Scope> fScope;
    Compiler& fCompiler;
    std::shared_ptr<SecondPass> fPass;
  };

} // namespace
