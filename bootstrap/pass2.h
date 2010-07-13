/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass2_h
#define bootstrap_pass2_h

#include <set>
#include <list>

#include "apt.h"
#include "parser.h"
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

  class SecondPass
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


    void parseTopExprlist(AptNode* rootNode, const Token& expr);

    AptNode* parseDef(const Token& expr);
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

    AptNode* parseTypeDef(const Token& expr, bool isType);
    AptNode* parseAliasDef(const Token& expr);
    AptNode* parseSlotDef(const Token& expr);
    AptNode* parseEnumDef(const Token& expr);
    AptNode* parseMeasureDef(const Token& expr);
    AptNode* parseUnitDef(const Token& expr);
    AptNode* parseVarDef(const Token& expr, VardefFlags flags, int ofs);
    AptNode* parseFunctionDef(const Token& expr);

    AptNode* parseLiteralArray(const Token& expr);
    AptNode* parseLiteralVector(const Token& expr);
    AptNode* parseLiteralDict(const Token& expr);

    AptNode* newDefNode(AptNode* node, bool isLet);

    AptNode* parseBlock(const Token& expr);
    AptNode* parseNested(const Token& expr);


    Type parseTypeSpec(const Token& expr);
    Type parseTypeSpecImpl(const Token& expr);



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


    //-------- data member

    Ptr<Parser> fParser;
    std::list<Ptr<AptNode> > fLastModules;
    std::set<String>         fCurrentGenericTypes;

    Ptr<Scope>               fScope;
  };
};

#endif  // bootstrap_pass2_h
