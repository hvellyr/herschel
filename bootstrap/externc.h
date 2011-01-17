/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_externc_h
#define bootstrap_externc_h

#include "token.h"
#include "pass1.h"
#include "refcountable.h"
#include "ptr.h"
#include "token.h"


namespace herschel
{
  class ExternCParser : public RefCountable
  {
  public:
    ExternCParser(FirstPass* pass1);

    TokenVector parseBlock();

  private:
    Token parseTypeSpec();
    Token scanUntilBrace();

    Token nextToken();

    Token parseCFunction(const Token& sym, const Token& retType);

    TokenVector makeExternDefHead(const SrcPos& srcpos);

    Token fToken;
    Ptr<FirstPass> fPass;
  };
};                              // namespace

#endif                          // bootstrap_externc_h
