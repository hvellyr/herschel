/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

#ifndef bootstrap_file_h
#define bootstrap_file_h

#include <vector>


namespace heather
{
  class String;

  typedef std::vector<String> StringVector;

  namespace file
  {
    bool isFilePath(const String& path);

    bool isFile(const String& path);
    bool isDir(const String& path);

    bool isAbsolutePath(const String& path);
    bool hasExtension(const String& path);

    String appendDir(const String& path, const String& dirName);
    String appendFile(const String& path, const String& name);
    String appendExt(const String& path, const String& ext);

    String namePart(const String& path);
    String dirPart(const String& path);

    //! returns the basename of a file, i.e. it removes the suffix (the last
    //! part after a '.' incl. the '.')
    String baseName(const String& path);

    String workingDir();
    String homeDir();

    //! Returns the canonicalized form of path 
    String canonicalPathName(const String& path);

    //! Like canonicalPathName(String) but with \p path being relative to \p
    //! baseDir.
    String canonicalPathName(const String& path, const String& baseDir);

    String lookupInPath(const String& pattern,
                        const StringVector& searchPath,
                        const StringVector& altExtensions);

  };                            // namespace
};                              // namespace

#endif                          // bootstrap_file_h
