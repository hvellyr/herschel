/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_pass2_h
#define bootstrap_pass2_h

#include "refcountable.h"
#include "apt.h"
#include "port.h"
#include "tokenport.h"
#include "token.h"
#include "parser.h"


namespace heather
{
  //--------------------------------------------------------------------------

  class SecondPass
  {
  public:
    SecondPass(Parser* parser);

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

    AptNode* newDefNode(AptNode* node, bool isLet);

    AptNode* parseBlock(const Token& expr);
    AptNode* parseNested(const Token& expr);

    //-------- data member

    Ptr<Parser> fParser;
    std::list<Ptr<AptNode> > fLastModules;
  };
};

#endif  // bootstrap_pass2_h
