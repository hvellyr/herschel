/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "common.h"

#include <unistd.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "file.h"
#include "log.h"
#include "str.h"
#include "strbuf.h"


using namespace herschel;

String
herschel::file::makeDir(const String& path)
{
  if (path.length() > 0) {
    if (path[path.length() - 1] == '/')
      return path;
  }

  return path + '/';
}


bool
herschel::file::isFilePath(const String& path)
{
  return (!path.isEmpty() && path[path.length() - 1] != '/');
}


bool
herschel::file::isFile(const String& path)
{
  struct stat fstat;

  if (::stat(StrHelper(path), &fstat) == 0) {
    if (S_ISREG(fstat.st_mode))
      return true;
  }

  return false;
}


bool
herschel::file::isDir(const String& path)
{
  struct stat fstat;

  if (::stat(StrHelper(path), &fstat) == 0) {
    if (S_ISDIR(fstat.st_mode))
      return true;
  }

  return false;
}


bool
herschel::file::isAbsolutePath(const String& path)
{
  // TODO: support drive letters and UNC notation for windows.
  return (!path.isEmpty() && (path[0] == '/' ||
                              path[0] == '~'));
}


bool
herschel::file::hasExtension(const String& path)
{
  String name = file::namePart(path);
  int idx = name.lastIndexOf('.');
  return idx >= 0;
}


String
herschel::file::namePart(const String& path)
{
  int idx = path.lastIndexOf('/');
  if (idx >= 0)
    return path.part(idx + 1, path.length());

  return path;
}


String
herschel::file::dirPart(const String& path)
{
  int idx = path.lastIndexOf('/');
  if (idx >= 0)
    return path.part(0, idx + 1);

  return String();
}


String
herschel::file::baseName(const String& path)
{
  String name = file::namePart(path);
  int idx = name.lastIndexOf('.');
  if (idx >= 0)
    return name.part(0, idx);

  return path;
}


String
herschel::file::appendDir(const String& path, const String& dirName)
{
  hr_assert(!isFilePath(path));
  return makeDir(path + dirName);
}


String
herschel::file::appendDir(const String& path, const String& dirName,
                         const String& dirName2)
{
  hr_assert(!isFilePath(path));
  return appendDir(appendDir(path, dirName), dirName2);
}


String
herschel::file::appendDir(const String& path, const String& dirName1,
                         const String& dirName2, const String& dirName3)
{
  hr_assert(!isFilePath(path));
  return appendDir(appendDir(appendDir(path, dirName1),
                             dirName2),
                   dirName3);
}


String
herschel::file::appendDir(const String& path, const String& dirName1,
                         const String& dirName2, const String& dirName3,
                         const String& dirName4)
{
  hr_assert(!isFilePath(path));
  return appendDir(appendDir(appendDir(appendDir(path, dirName1),
                                       dirName2),
                             dirName3),
                   dirName4);
}


String
herschel::file::appendFile(const String& path, const String& name)
{
  hr_assert(!isFilePath(path));
  return path + name;
}


String
herschel::file::appendExt(const String& path, const String& ext)
{
  return path + "." + ext;
}


String
herschel::file::append(const String& path, const String& name)
{
  if (isFilePath(path))
    return path + "/" + name;
  return path + name;
}


String
herschel::file::workingDir()
{
  int size = 256;
  std::vector<char> buffer;

  buffer.resize(256);

  while (1) {
    zstring value = ::getcwd(&buffer[0], size);
    if (value)
      return String(value, ::strlen(value)) + "/";
    else if (errno != ERANGE)
      return String();

    size *= 2;
    buffer.resize(size);
  }

  hr_invalid("?");
  return String();
}


String
herschel::file::homeDir()
{
  char *home = ::getenv("HOME");
  if (home)
    return String(home);
  return String();
}


class CanonicalPathElt
{
public:
  CanonicalPathElt()
    : fIsPathElt(false)
  { }


  CanonicalPathElt(const String& name, bool isPathElt)
    : fName(name),
      fIsPathElt(isPathElt)
  { }


  CanonicalPathElt(const CanonicalPathElt& other)
    : fName(other.fName),
      fIsPathElt(other.fIsPathElt)
  { }


  CanonicalPathElt& operator=(const CanonicalPathElt& other)
  {
    fName = other.fName;
    fIsPathElt = other.fIsPathElt;
    return *this;
  }


  const String& name() const
  {
    return fName;
  }


  bool isPath() const
  {
    return fIsPathElt;
  }


  void setIsPath(bool value)
  {
    fIsPathElt = value;
  }


  bool isEmpty() const
  {
    return fName.isEmpty();
  }


private:
  String fName;
  bool fIsPathElt;
};


class CanonicalPath
{
public:
  using CNEltVector = std::vector<CanonicalPathElt>;

  CanonicalPath()
  { }

  CanonicalPath(const CanonicalPath& other)
    : fElts(other.fElts)
  { }

  CanonicalPath& operator=(const CanonicalPath& other)
  {
    fElts = other.fElts;
    return *this;
  }

  CanonicalPath(const String& path)
  {
    int pidx = 0;
    int qidx = 0;
    int plen = path.length();

    while (pidx < plen) {
      qidx = path.indexOf('/', pidx);

      if (qidx >= pidx) {
        fElts.push_back(CanonicalPathElt(path.part(pidx, qidx), K(isPathElt)));
      }
      else {
        fElts.push_back(CanonicalPathElt(path.part(pidx, plen), !K(isPathElt)));
        break;
      }
      pidx = qidx + 1;
    }
  }


  void makeLastEltToPath()
  {
    hr_assert(!fElts.empty());
    fElts.back().setIsPath(true);
  }


  String toString() const
  {
    StringBuffer buf;

    for (CNEltVector::const_iterator it = fElts.begin();
         it != fElts.end();
         it++)
    {
      buf << it->name();
      if (it->isPath())
        buf << "/";
    }

    return buf.toString();
  }


  //! makes an path absolute (replace ~, ./ in the beginning, prepand current
  //! working directory
  void makeAbsolut(const String& baseDir)
  {
    static const String sTilde = String("~");
    static const String sDot = String(".");

    if (!fElts.empty()) {
      if (fElts[0].name() == sTilde) {
        CanonicalPath home(file::homeDir());

        home.makeLastEltToPath();
        fElts.erase(fElts.begin());
        fElts.insert(fElts.begin(), home.fElts.begin(), home.fElts.end());
      }

      if (fElts[0].name() == sDot) {
        CanonicalPath base(baseDir);

        base.makeLastEltToPath();
        fElts.erase(fElts.begin());
        fElts.insert(fElts.begin(), base.fElts.begin(), base.fElts.end());
      }
      //! if the first element is empty, than it is a root dir, therefore keep
      //! it as is
      else if (!fElts[0].isEmpty()) {
        CanonicalPath base(baseDir);

        base.makeLastEltToPath();
        fElts.insert(fElts.begin(), base.fElts.begin(), base.fElts.end());
      }
    }
  }


  // removes double slashs, empty path elements, etc.
  void removeDblPaths()
  {
    static const String sDot = String(".");

    for (size_t i = 1; i < fElts.size(); ) {
      if (fElts[i].isEmpty() || fElts[i].name() == sDot)
        fElts.erase(fElts.begin() + i);
      else
        i++;
    }
  }


  void resolveDDots()
  {
    CNEltVector newElts;
    static const String sDdot = String("..");

    for (CNEltVector::const_iterator it = fElts.begin();
         it != fElts.end();
         it++)
    {
      if (it->name() == sDdot && newElts.size() > 0) {
        if (newElts.back().name() != sDdot)
          newElts.erase(newElts.end() - 1);
        else
          newElts.push_back(*it);
      }
      else
        newElts.push_back(*it);
    }

    fElts = newElts;
  }


private:
  std::vector<CanonicalPathElt> fElts;
};


String
herschel::file::canonicalPathName(const String& path, const String& baseDir)
{
  CanonicalPath cn(path);

  cn.makeAbsolut(baseDir);
  cn.removeDblPaths();
  cn.resolveDDots();

  return cn.toString();
}


String
herschel::file::canonicalPathName(const String& path)
{
  return canonicalPathName(path, workingDir());
}


//----------------------------------------------------------------------------

static String
checkForFileWithExts(const String& fullPath,
                     const StringVector& altExtensions)
{
  if (file::isFile(fullPath))
    return fullPath;

  if (!file::hasExtension(fullPath)) {
    for (StringVector::const_iterator eit = altExtensions.begin();
         eit != altExtensions.end();
         eit++)
    {
      String extFullPath = file::appendExt(fullPath, *eit);
      if (file::isFile(extFullPath))
        return extFullPath;
    }
  }

  return String();
}


String
herschel::file::lookupInPath(const String& pattern,
                             const StringVector& searchPath,
                             const StringVector& altExtensions)
{
  if (file::isAbsolutePath(pattern)) {
    String result = checkForFileWithExts(file::canonicalPathName(pattern),
                                         altExtensions);
    if (!result.isEmpty())
      return result;
  }
  else {
    for (StringVector::const_iterator it = searchPath.begin();
         it != searchPath.end();
         it++)
    {
      String realPath = file::canonicalPathName(*it);
      String result = checkForFileWithExts(file::canonicalPathName(pattern,
                                                                   realPath),
                                           altExtensions);
      if (!result.isEmpty())
        return result;
    }
  }

  return String();
}

