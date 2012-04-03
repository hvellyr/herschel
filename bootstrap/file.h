/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

#ifndef bootstrap_file_h
#define bootstrap_file_h

#include <vector>


namespace herschel
{
  class String;

  typedef std::vector<String> StringVector;

  //! The \c herschel::file namespace contains functions for dealing with file
  //! system paths.
  //!
  //! File system paths are represented as simple String instances.  Paths
  //! ending with a directory separator (for all platforms uniformly given as
  //! \c /) are treated as directories, the other as files.
  //!
  //! Note that most of the function do not check the real file system, but
  //! work solely on the path representation.

  namespace file
  {
    //! Check whether \p path points to a file and the file really exists.
    bool isFile(const String& path);

    //! Check whether \p path points to a directory and the directory really exists.
    bool isDir(const String& path);

    //! Transform \p path into a directory path, i.e. if it is not ending in a
    //! directory separator (\c /) yet, append one and return the new form.
    String makeDir(const String& path);

    //! Indicates whether \p path is a possible file path.  A file path is a
    //! not empty string not ending in a directory separator.
    bool isFilePath(const String& path);

    //! Indicates whether \p path is a absolute path.

    //! A path is recognized as being absolute if it is 1) not empty, 2)
    //! starts with a root marker (on un*x like systems with a directory
    //! separator), or 3) starts with the home directory marker (~).

    //! For example, <tt>../abc/x.txt</tt>, <tt>x.txt</tt>, and <tt>.</tt> are
    //! not absolute, but <tt>/usr/lib/</tt>, <tt>~/x.txt</tt>, and
    //! <tt>c:/x.txt</tt> are.
    bool isAbsolutePath(const String& path);

    //! Indicates whether \p path has an extension, i.e. whether its last path
    //! part contains a extension separator (\c .).  Both files and
    //! directories can have extensions.
    bool hasExtension(const String& path);

    //! Appends a new path step \p name to \p path and returns the new path.
    //! Note that this function works even if \p path is not a directory path.
    String append(const String& path, const String& name);
    //! Appends \p dirName to \p path and makes a new directory path from it.
    //! \p path must be a directory path.
    String appendDir(const String& path, const String& dirName);
    //! Appends \p dirName and \p dirName2 to \p path and makes a new
    //! directory path from it.  \p path must be a directory path.
    String appendDir(const String& path, const String& dirName,
                     const String& dirName2);
    //! Appends \p dirName, \p dirName2, and \p dirName3 to \p path and makes
    //! a new directory path from it.  \p path must be a directory path.
    String appendDir(const String& path, const String& dirName1,
                     const String& dirName2, const String& dirName3);
    //! Appends \p dirName, \p dirName2, \p dirName3, and \p dirName4 to \p
    //! path and makes a new directory path from it.  \p path must be a
    //! directory path.
    String appendDir(const String& path, const String& dirName1,
                     const String& dirName2, const String& dirName3,
                     const String& dirName4);
    //! Appends \p name to \p path and makes a new file path from it. \p path
    //! must be a directory path.
    String appendFile(const String& path, const String& name);
    //! Appends \p ext as extension to \p path.  An extension separator (\c .)
    //! is inserted between \p path and \p ext.
    String appendExt(const String& path, const String& ext);

    //! Returns the name part of \p path, i.e. the last path component.  For
    //! directories this is the directory name and for files the file name.
    //! For root directories the name part is the empty string.
    String namePart(const String& path);

    //! Returns the directory part of \p path, i.e. the part of \p path except
    //! for the last path component.
    String dirPart(const String& path);

    //! Returns the basename of a file, i.e. it removes the extension (the
    //! last part after a '.' incl. the '.').  For example, the basename of
    //! <tt>~/abc/x.txt</tt> is <tt>~/abc/x</tt>
    String baseName(const String& path);

    //! Returns the path to the current working directory.  Note that the
    //! current working directory can be changed by system calls from other
    //! parts of the application.
    String workingDir();

    //! Returns the home directory for the user, who's executing the
    //! application.
    String homeDir();

    //! Returns the canonicalized form of \p path.  The canonical path form is
    //! a normalized form of \p path.  Esp. the following steps are done:
    //!
    //! 1) \p path is made absolute (if \p path is not an absolute path it is
    //! resolved relative to \p basedir);
    //!
    //! 2) unnecessary path steps are removed, i.e. double directory
    //! separators, "dot only" path steps;
    //!
    //! 3) upsteps (<tt>..</tt>) are resolved and removed as possible.
    //!
    //! For example, within <tt>/home/mfi/tmp/</tt> as working dir, the path
    //! <tt>../abc/x.txt</tt> is canonicallized as
    //! <tt>/home/mfi/abc/x.txt</tt>, the path <tt>/x/y/../../z/.//n.txt</tt>
    //! as <tt>/z/n.txt</tt>, and <tt>~/./././</tt> as <tt>/home/mfi/</tt> (if
    //! \c mfi is the currently logged in user).
    String canonicalPathName(const String& path, const String& baseDir);

    //! Returns the canonicalized form of \p path, with relative paths
    //! normalized to the current working dir (if necessary).  See
    //! \c canonicalPathName().
    String canonicalPathName(const String& path);

    //! Search for a existing file matches \p pattern in all directories in \p
    //! searchPath.  If \p pattern has no extension it is tested with all
    //! alternative extensions in \p altExtensions.
    //!
    //! With searchPath=<tt>['/usr/lib/', '/home/mfi/x/', '~/']</tt>,
    //! altExtensions=<tt>['cpp', 'cxx', 'C']</tt>, and the pattern <tt>'n/m'</tt> the following
    //! paths will be checked:
    //!
    //! <pre>
    //! /usr/lib/n/m
    //! /usr/lib/n/m.cpp
    //! /usr/lib/n/m.cxx
    //! /usr/lib/n/m.C
    //! /home/mfi/x/n/m
    //! /home/mfi/x/n/m.cpp
    //! /home/mfi/x/n/m.cxx
    //! /home/mfi/x/n/m.C
    //! ~/n/m
    //! ~/n/m.cpp
    //! ~/n/m.cxx
    //! ~/n/m.C
    //! </pre>
    String lookupInPath(const String& pattern,
                        const StringVector& searchPath,
                        const StringVector& altExtensions);

  };                            // namespace
};                              // namespace

#endif                          // bootstrap_file_h
