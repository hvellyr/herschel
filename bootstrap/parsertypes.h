/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_parsertypes_h
#define bootstrap_parsertypes_h

#include "token.h"
#include "registry.h"


namespace herschel
{
  class Token;

  enum VizType {
    kUnset,
    kPrivate,
    kInner,
    kOuter,
    kPublic,
  };


  //--------------------------------------------------------------------------

  class ConfigVarRegistry : public Registry<Token>
  {
  public:
    ConfigVarRegistry();
    ConfigVarRegistry(ConfigVarRegistry* parent);

    virtual bool lookup(const String& name, Token* out) const;

  private:
    Ptr<ConfigVarRegistry> fParent;
  };


  OperatorType tokenTypeToOperator(TokenType type);
  TokenType operatorToTokenType(OperatorType op);

};                              // namespace


#endif  // bootstrap_parsertypes_h
