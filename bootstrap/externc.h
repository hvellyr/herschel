/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
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
  //! Simple parser for the C language subset, which is allowed in "extern
  //! (C)" statements.
  //!
  //! The class is only functional as component to a \c FirstPass instance,
  //! and has to be called on the stream when an "extern C" is detected.  If
  //! this class returns vom \c parseBlock() the final closing brace of the
  //! extern block has been digested and the next token in the \c FirstPass
  //! instance is the next token in the herschel language.

  class ExternCParser : public RefCountable
  {
  public:
    ExternCParser(FirstPass* pass1);

    //! Parse the "extern (C)" block.  On return the final closing brace has
    //! been digested.
    TokenVector parseBlock();

  private:
    Token parseTypeSpec();
    Token scanUntilBrace();

    Token nextToken();

    Token parseCFunction(const Token& sym, const Token& retType);

    TokenVector makeExternDefHead(const SrcPos& srcpos);

    //-------- data members

    Token fToken;
    Ptr<FirstPass> fPass;
  };
};                              // namespace

#endif                          // bootstrap_externc_h
