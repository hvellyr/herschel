/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "registry.hpp"
#include "token.hpp"

#include <memory>
#include <ostream>


namespace herschel {
class Token;
class String;

enum VizType {
  kUnset,
  kPrivate,
  kIntern,
  kPublic,
};

std::ostream& operator<<(std::ostream& os, VizType v);

class ConfigVarRegistry : public Registry<Token> {
public:
  ConfigVarRegistry();
  ConfigVarRegistry(std::shared_ptr<ConfigVarRegistry> parent);

  bool lookup(const String& name, Token* out) const override;

private:
  std::shared_ptr<ConfigVarRegistry> fParent;
};


OperatorType tokenTypeToOperator(TokenType type);
TokenType operatorToTokenType(OperatorType op);

}  // namespace herschel
