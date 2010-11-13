/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass2_h
#define bootstrap_pass2_h

#include <set>
#include <list>
#include <map>

#include "apt.h"
#include "compilepass.h"
#include "parser.h"
#include "pass.h"
#include "port.h"
#include "refcountable.h"
#include "scope.h"
#include "token.h"
#include "tokenport.h"
#include "type.h"
#include "typectx.h"

namespace heather
{
  //--------------------------------------------------------------------------

  class SecondPass : public AbstractPass
  {
  public:
    SecondPass(Parser* parser, Scope* scope);

    AptNode* parse(const Token& exprs);

  private:
    AptNode* parseExpr(const Token& expr);
    AptNode* parseSeq(const Token& expr);

    AptNode* parseModule(const Token& expr);
    AptNode* parseExport(const Token& expr);
    AptNode* parseImport(const Token& expr);


    void parseTopExprlist(const Token& expr);

    AptNode* parseDef(const Token& expr, bool isLocal);
    AptNode* parseIf(const Token& expr);
    AptNode* parseOn(const Token& expr);
    AptNode* parseFor(const Token& expr);
    AptNode* parseSelect(const Token& expr);
    AptNode* parseMatch(const Token& expr);
    AptNode* parseClosure(const Token& expr);
    AptNode* parseBinary(const Token& expr);
    AptNode* parseFunCall(const Token& expr);
    AptNode* parseTypeExpr(const Token& expr);

    AptNode* parseTokenVector(const TokenVector& seq);
    void parseParameters(NodeList* parameters, const TokenVector& seq);

    AptNode* parseParameter(const Token& expr);

    AptNode* parseTypeDef(const Token& expr, size_t ofs, bool isType);
    AptNode* parseAliasDef(const Token& expr, size_t ofs, bool isLocal);
    AptNode* parseSlotDef(const Token& expr, size_t ofs);
    AptNode* parseEnumDef(const Token& expr, size_t ofs, bool isLocal);
    AptNode* nextEnumInitValue(const SrcPos& srcpos,
                               const Token& enumItemSym,
                               const Type& baseType, Token& lastInitToken);
    AptNode* parseMeasureDef(const Token& expr, size_t ofs, bool isLocal);
    AptNode* parseUnitDef(const Token& expr, size_t ofs, bool isLocal);
    AptNode* parseVarDef(const Token& expr, VardefFlags flags, size_t ofs,
                         bool isLocal, const String& linkage);
    AptNode* parseFunctionDef(const Token& expr, size_t ofs, bool isLocal,
                              const String& linkage);

    AptNode* parseLiteralArray(const Token& expr);
    AptNode* parseLiteralVector(const Token& expr);
    AptNode* parseLiteralDict(const Token& expr);

    AptNode* newDefNode(AptNode* node, bool isLet);

    AptNode* parseBlock(const Token& expr);
    AptNode* parseNested(const Token& expr);

    AptNode* parseExtend(const Token& expr);

    Type parseTypeSpec(const Token& expr);
    Type parseTypeSpecImpl(const Token& expr);
    Type parseTypeSpecImpl2(const Token& expr, bool isValue);
    Type parseGroupType(const Token& expr, bool isValue);
    Type rephraseRefType(const SrcPos& srcpos, const Type& inType, bool isValue);

    void parseExtendImpl(NodeList* reqProtocol, const Token& expr);


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

    void parseTypeVector(TypeVector* generics, const Token& expr);
    void paramsNodeListToType(FunctionParamVector* funcParams,
                              const NodeList& nl) const;
    void protocolNodeListToType(FunctionSignatureVector* protoSignatures,
                                const NodeList& nl) const;
    FunctionSignature nodeToFunSignature(const FuncDefNode* node) const;

    AptNode* parseIntNumber(const Token& expr);
    AptNode* parseRationalNumber(const Token& expr);
    AptNode* parseRealNumber(const Token& expr);

    AptNode* parseChainSelect(const Token& expr);
    AptNode* parseRealSelect(const Token& expr);

    AptNode* parseUnitNumber(const Token& expr);

    struct FundefClauseData
    {
      FundefClauseData()
        : fFlags(0)
      { }

      NodeList     fParams;
      Type         fType;
      Ptr<AptNode> fReify;
      Ptr<AptNode> fWhere;
      unsigned int fFlags;
      Ptr<AptNode> fBody;
    };
    void parseFundefClause(const TokenVector& seq, size_t& ofs,
                           FundefClauseData& data);


    Type parseBinaryTypeSpec(const Token& expr, bool forceGeneric,
                             bool isValue);
    Type parseWhereConstraint(const Token& whereConstrSeq);
    void parseWhereClause(const Token& whereSeq);

    Type genericTypeRef(const String& id, bool isValue) const;
    size_t getWhereOfs(const Token& expr) const;
    size_t getWhereOfs(const TokenVector& seq, size_t ofs) const;


    typedef std::map<String, Type> TSharedGenericTable;
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

    std::set<String>    fCurrentGenericTypes;
    TSharedGenericTable fSharedGenericTable;
    Ptr<ListNode>       fRootNode;
  };


  //--------------------------------------------------------------------------

  class NodifyPass : public Token2AptNodeCompilePass
  {
  public:
    NodifyPass(int level, Parser* parser, Scope* scope);
    virtual AptNode* doApply(const Token& src);
    Scope* currentScope();

  private:
    Ptr<Scope>      fScope;
    Ptr<Parser>     fParser;
    Ptr<SecondPass> fPass;
  };
};

#endif  // bootstrap_pass2_h
