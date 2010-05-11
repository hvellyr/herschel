/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_parsertypes_h
#define bootstrap_parsertypes_h

#include "pexpr.h"
#include "registry.h"


namespace heather
{
  class Pexpr;


  //--------------------------------------------------------------------------

  class ConfigVarRegistry : public Registry<Pexpr>
  {
  public:
    ConfigVarRegistry();
    ConfigVarRegistry(ConfigVarRegistry* parent);

    virtual bool lookup(const String& name, Pexpr* out) const;

  private:
    Ptr<ConfigVarRegistry> fParent;
  };


  OperatorType tokenTypeToOperator(TokenType type);
  TokenType operatorToTokenType(OperatorType op);

};                              // namespace


#endif  // bootstrap_parsertypes_h
