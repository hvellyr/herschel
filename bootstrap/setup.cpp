/* -*-c++-*-

   This file is part of the heather package

   Copyright (c) 2010 Gregor Klinke
   All rights reserved.
*/

//----------------------------------------------------------------------------

#include "common.h"
#include "setup.h"
#include "properties.h"
#include "str.h"
#include "file.h"

#include <vector>

using namespace heather;


/* ----------------------------------------------------------------------
   navigate in installation, find default resource files
   ---------------------------------------------------------------------- */
#if defined(OS_mac) || defined(OS_win) || defined(OS_linux)

namespace heather
{
extern StringVector findSysResourceBundle();
};

#else

namespace heather
{
  /* a generic implementation */
  StringVector
  findSysResourceBundle()
  {
    StringVector result;

#if defined(IS_DEBUG)
    /* check whether we run from the development folder */
    {
      String cwd = file::workingDir();
      if (file::isFile(file::appendFile(cwd, String("temp/debug/heather"))) ||
          file::isFile(file::appendFile(cwd, String("temp/release/heather"))) ) {

        result.push_back(file::appendDir(cwd, String("lib")));
        return result;
      }
    }
#endif

    result.push_back(file::appendDir(
                       file::makeDir(String(HEA_INSTDIR_pkglibdir)),
                       String(VERSION)),
                       String("include"));
    return result;
  }
};

#endif


namespace heather
{
  static StringVector
  findResourceBundle()
  {
    return findSysResourceBundle();
  }
};


void
heather::setupDefaultPath()
{
  StringVector syspaths = findResourceBundle();

  for (StringVector::iterator it = syspaths.begin(), e = syspaths.end();
       it != e; ++it)
  {
    if (!it->isEmpty())
      Properties::addSystemDir(*it);
  }
}

