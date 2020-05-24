/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#include "srcpos.hpp"

#include "str.hpp"
#include "strbuf.hpp"

#include <unordered_map>


namespace herschel {

namespace {
  std::unordered_map<String, int>& path2IdxCache()
  {
    static auto cache = std::unordered_map<String, int>{};
    return cache;
  }

  std::unordered_map<int, String>& idx2PathCache()
  {
    static auto cache = std::unordered_map<int, String>{};
    return cache;
  }

  int pathToIndex(const String& file)
  {
    static auto nextFileIndex = 0;

    auto iFile = path2IdxCache().find(file);
    if (iFile != end(path2IdxCache())) {
      return iFile->second;
    }

    auto idx = ++nextFileIndex;
    path2IdxCache()[file] = idx;
    idx2PathCache()[idx] = file;

    return idx;
  }

  const String& indexToPath(int idx)
  {
    static auto nullValue = String{};
    auto iIdx = idx2PathCache().find(idx);
    return iIdx != end(idx2PathCache()) ? iIdx->second : nullValue;
  }
}  // namespace


SrcPos::SrcPos(const String& file, int lineno, int colno)
    : fFileIdx(pathToIndex(file))
    , fLineNo(lineno)
    , fColNo(colno)
{
}


const String& SrcPos::file() const
{
  return indexToPath(fFileIdx);
}

String SrcPos::toString() const
{
  return (StringBuffer(file()) << ':' << fromInt(fLineNo) << ':' << fromInt(fColNo))
      .toString();
}

}  // namespace herschel
