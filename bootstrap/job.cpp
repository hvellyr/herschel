/* -*-c++-*-

   This file is part of the herschel package

   Copyright (c) 2010-2011 Gregor Klinke
   All rights reserved.

   This source code is released under the BSD License.
*/

//----------------------------------------------------------------------------

#include "job.hpp"

#include "log.hpp"
#include "str.hpp"
#include "strbuf.hpp"


#if defined(OS_mac) || defined(OS_linux)

#  include <vector>

#  include <stdio.h>
#  include <stdlib.h>
#  include <string.h>
#  include <sys/types.h>
#  include <sys/wait.h>
#  include <unistd.h>


using namespace herschel;

int herschel::startProcess(const String& cmd, const std::vector<String>& args,
                           bool logCalls)
{
  char buffer[16000];
  char* ptr = &buffer[0];
  std::vector<char const*> argv;
  argv.resize(args.size() + 2);

  int i = -1;
  for (; i < int(args.size()); i++) {
    StrHelper helper(i == -1 ? cmd : args[i]);
    int len = strlen(helper.c_str()) + 1;

    argv[i + 1] = ptr;
    memcpy(ptr, helper.c_str(), len);
    ptr += len;
  }
  argv[++i] = nullptr;

  if (logCalls) {
    StringBuffer buf;
    for (size_t j = 0; j < argv.size(); j++) {
      if (argv[j])
        buf << argv[j] << " ";
    }

    log(kInfo, buf.toString());
  }

  int status = 0;
  pid_t pid;

  StrHelper cmdbuf(cmd);

  pid = fork();
  if (pid == 0) {
    /* This is the child process.  Execute the shell command. */
    execvp(cmdbuf.c_str(), (char* const*)&argv[0]);
    _exit(EXIT_FAILURE);
  }
  else if (pid < 0) {
    /* The fork failed.  Report failure.  */
    status = -1;
  }
  else {
    /* This is the parent process.  Wait for the child to complete.  */
    if (waitpid(pid, &status, 0) != pid)
      status = -1;

    if (WIFEXITED(status))
      status = 0;
    else {
      fprintf(stderr, "ERROR: subprocess '%s' crashed\n", (zstring)StrHelper(cmd));
      status = -1;
    }
  }

  return status;
}


#elif defined(OS_win)


#else
#  error Unsupported OS
#endif
