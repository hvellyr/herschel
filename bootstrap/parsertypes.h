/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "token.h"
#include "registry.h"

#include <memory>


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
    ConfigVarRegistry(std::shared_ptr<ConfigVarRegistry> parent);

    bool lookup(const String& name, Token* out) const override;

  private:
    std::shared_ptr<ConfigVarRegistry> fParent;
  };


  OperatorType tokenTypeToOperator(TokenType type);
  TokenType operatorToTokenType(OperatorType op);

} // namespace
