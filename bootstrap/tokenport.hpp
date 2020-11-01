/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"

#include "port.hpp"
#include "tokenizer.hpp"

#include <list>
#include <memory>
#include <vector>
#include <list>


namespace herschel {

class CheckpointRecorder {
public:
  CheckpointRecorder() = default;

  std::vector<Token> fParked;
};


class TokenPort : public Port<Token> {
public:
  size_t write(const Token* data, size_t items) override;
  int write(Token item) override;

  void flush() override;

  bool canSetCursor() const override;

  void pushCheckpoint(std::shared_ptr<CheckpointRecorder> cpr);
  std::shared_ptr<CheckpointRecorder> popCheckpoint();
  CheckpointRecorder* currentCheckpoint();

private:
  std::list<std::shared_ptr<CheckpointRecorder>> fCheckpoints;
};


class FileTokenPort : public TokenPort {
public:
  FileTokenPort(std::shared_ptr<Port<Octet>> port, const String& srcName,
                std::shared_ptr<CharRegistry> charRegistry = nullptr);
  FileTokenPort(std::shared_ptr<Port<Char>> port, const String& srcName,
                std::shared_ptr<CharRegistry> charRegistry = nullptr);

  bool isOpen() const override;
  bool isEof() const override;

  Token read() override;

private:
  void setTokenizer(std::shared_ptr<Tokenizer> tokenizer);
  Token recordToCheckpoint(const Token& token);

  String fSrcName;
  std::shared_ptr<Tokenizer> fTokenizer;
};


class InternalTokenPort : public TokenPort {
public:
  InternalTokenPort(const std::list<Token>& tokens);
  InternalTokenPort(const TokenVector& tokens);

  bool isOpen() const override;
  bool isEof() const override;

  Token read() override;

private:
  std::list<Token> fTokens;
};

}  // namespace herschel
