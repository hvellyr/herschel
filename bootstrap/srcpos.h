/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_srcpos_h
#define bootstrap_srcpos_h

#include "str.h"


namespace herschel
{
  //! Represents a position in a source file.  A position is formed by a
  //! source file name and a line number.  Column numbers are not tracked
  //! currently.
  class SrcPos
  {
  public:
    //! Constructs a default source position, pointing nowhere.
    SrcPos()
      : fLineNo(0)
    { }

    //! Constructs a source position refering to \p file and line number \p
    //! lineno.
    SrcPos(const String& file, int lineno)
      : fFile(file),
        fLineNo(lineno)
    { }

    //! Constructs a source position refering to \p file and line number \p
    //! lineno.
    SrcPos(zstring file, int lineno)
      : fFile(String(file)),
        fLineNo(lineno)
    { }

    //! Copy constructor
    SrcPos(const SrcPos& other)
    {
      *this = other;
    }

    //! Assign operator
    SrcPos& operator=(const SrcPos& other)
    {
      fFile = other.fFile;
      fLineNo = other.fLineNo;
      return *this;
    }

    //! Indicates whether *this is a valid source position, i.e. whether it
    //! refers to a file and lineno.
    bool isValid() const
    {
      return !fFile.isEmpty() && fLineNo != 0;
    }

    //! Compare operator
    bool operator==(const SrcPos& other) const
    {
      return (fFile == other.fFile &&
              fLineNo == other.fLineNo);
    }

    //! Compare operator
    bool operator!=(const SrcPos& other) const
    {
      return !(operator==(other));
    }

    //! Returns the file name portion of *this.
    const String& file() const
    {
      return fFile;
    }

    //! Returns the line number portion of *this.
    int lineNumber() const
    {
      return fLineNo;
    }

    //! Returns a string representation of *this which follows the convention
    //! of a other compilers and tools.  It is especially understand by emacs.
    String toString() const;

  private:
    String fFile;
    int fLineNo;
  };
};


#endif  // bootstrap_srcpos_h
