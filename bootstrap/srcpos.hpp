/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#pragma once

#include "common.hpp"
#include "str.hpp"


namespace herschel {

//! Represents a position in a source file.  A position is formed by a
//! source file name and a line number.  Column numbers are not tracked
//! currently.
class SrcPos {
public:
  //! Constructs a default source position, pointing nowhere.
  SrcPos() = default;

  //! Constructs a source position refering to \p file and line number \p
  //! lineno.
  SrcPos(const String& file, int lineno, int colno);

  //! Constructs a source position refering to \p file and line number \p
  //! lineno.
  SrcPos(zstring file, int lineno, int colno)
    : SrcPos(String(file), lineno, colno)
  {
  }

  //! Indicates whether *this is a valid source position, i.e. whether it
  //! refers to a file and lineno.
  bool isValid() const { return fFileIdx >= 0 && fLineNo != 0; }

  //! Compare operator
  bool operator==(const SrcPos& other) const
  {
    return fFileIdx == other.fFileIdx && fLineNo == other.fLineNo && fColNo == other.fColNo;
  }

  //! Compare operator
  bool operator!=(const SrcPos& other) const { return !(*this == other); }

  //! Returns the file name portion of *this.
  const String& file() const;

  //! Returns the line number portion of *this.
  int lineNumber() const { return fLineNo; }

  //! Returns the column number portion of *this.
  int columnNumber() const { return fColNo; }

  //! Returns a string representation of *this which follows the convention
  //! of a other compilers and tools.  It is especially understand by emacs.
  String toString() const;

private:
  int fFileIdx = -1;
  int fLineNo = 0;
  int fColNo = 0;
};

}  // namespace herschel
