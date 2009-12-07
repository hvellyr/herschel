#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "sys/stat.h"
#include <stdio.h>
#include <sys/types.h>
#include <limits.h>
#include <sys/time.h>
#include <utime.h>
#include <sys/wait.h>
#include <dirent.h>

#if defined(SEXP_Linux)
#  include <time.h>
#endif

#include "chibi/sexp.h"

#if defined(SEXP_OSX)
#include <CoreServices/CoreServices.h>

static sexp arc_fsref_to_path(sexp ctx, FSRef* ref)
{
  CFURLRef url = CFURLCreateFromFSRef(NULL, ref);

  if (url) {
    sexp result = SEXP_NULL;
    CFStringRef str = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
    
    if (str) {
      char path[PATH_MAX];
      if (CFStringGetCString(str, path, PATH_MAX, kCFStringEncodingUTF8))
        result = sexp_c_string(ctx, path, -1);
      
      CFRelease(str);
    }
    
    CFRelease(url);
    
    return result;
  }

  return SEXP_NULL;
}
#endif

/* (getenv key) => string */
static sexp arc_getenv(sexp ctx, sexp arg1)
{
  char* value;

  if (!sexp_stringp(arg1))
    return sexp_type_exception(ctx, "not a string", arg1);

  value = getenv(sexp_string_data(arg1));
  if (value)
    return sexp_c_string(ctx, value, -1);
  
  return SEXP_FALSE;
}


/* (mkdir path permission) => bool */
static sexp arc_mkdir(sexp ctx, sexp arg1, sexp arg2)
{
  if (!sexp_stringp(arg1))
    return sexp_type_exception(ctx, "not a string", arg1);
  if (!sexp_integerp(arg2))
    return sexp_type_exception(ctx, "not an integer", arg2);

  if (mkdir(sexp_string_data(arg1), 
            (unsigned int)sexp_unbox_integer(arg2)) == 0)
    return SEXP_TRUE;

  return SEXP_FALSE;
}


/* (unlink path) => bool */
static sexp arc_unlink(sexp ctx, sexp path)
{
  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (unlink(sexp_string_data(path)) == 0)
    return SEXP_TRUE;

  return SEXP_FALSE;
}


/* (rmdir path) => bool */
static sexp arc_rmdir(sexp ctx, sexp path)
{
  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (rmdir(sexp_string_data(path)) == 0)
    return SEXP_TRUE;

  return SEXP_FALSE;
}

  
/* (symlink from to) */
static sexp arc_symlink(sexp ctx, sexp from, sexp to)
{
  if (!sexp_stringp(from))
    return sexp_type_exception(ctx, "not a string", from);
  if (!sexp_stringp(to))
    return sexp_type_exception(ctx, "not a string", to);

  if (symlink(sexp_string_data(from), sexp_string_data(to)) == 0)
    return SEXP_TRUE;
  
  return SEXP_FALSE;
}


/* (chdir path) => bool */
static sexp arc_chdir(sexp ctx, sexp path)
{
  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (chdir(sexp_string_data(path)) == 0)
    return SEXP_TRUE;

  return SEXP_FALSE;
}


/* (file-exists? path) => bool */
static sexp arc_file_existsq(sexp ctx, sexp path)
{
  struct stat fattr;
    
  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (stat(sexp_string_data(path), &fattr) == 0)
    return SEXP_TRUE;
  
  return SEXP_FALSE;
}


/* (file-directory? path) => bool */
static sexp arc_file_directoryq(sexp ctx, sexp path)
{
  struct stat fattr;
    
  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (stat(sexp_string_data(path), &fattr) == 0 
      && (fattr.st_mode & S_IFMT) == S_IFDIR)
    return SEXP_TRUE;
  
  return SEXP_FALSE;
}


/* (file-executable? path) => bool */
static sexp arc_file_executableq(sexp ctx, sexp path)
{
  struct stat fattr;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);
  
  if (stat(sexp_string_data(path), &fattr) == 0 &&
      (fattr.st_mode & (S_IEXEC | S_IXGRP | S_IXOTH)) != 0)
    return SEXP_TRUE;
  
#if defined(OS_WIN)
  {
    int namelen = strlen(name);
    if (sexp_string_length(path) > 4) {
      char* suffix = &sexp_string_data(path)[sexp_string_length(path) - 4];
      if (strcmp(suffix, ".cmd") == 0 ||
          strcmp(suffix, ".com") == 0 ||
          strcmp(suffix, ".exe") == 0 ||
          strcmp(suffix, ".bat") == 0) 
        return SEXP_TRUE;
    }
  }
#endif
  
  return SEXP_FALSE;
}


/* (current_time) => int */
static sexp arc_current_time(sexp ctx)
{
  time_t now = time(NULL);
  return sexp_make_integer((unsigned long)now);
}


/* (tempdir) => string */
static sexp arc_tempdir(sexp ctx)
{
#if defined(SEXP_OSX)
  FSRef ref;
  OSErr err = FSFindFolder(kUserDomain, kChewableItemsFolderType, 
                           kCreateFolder, &ref);
  if (err == noErr)
    return arc_fsref_to_path(ctx, &ref);

#else
  char* tmppath = getenv("TMPDIR");
  if (tmppath)
    return sexp_c_string(ctx, tmppath, -1);

  return sexp_c_string(ctx, "/tmp", -1);
#endif

  return SEXP_FALSE;
}


/* (homedir) => string */
static sexp arc_homedir(sexp ctx)
{
#if defined(SEXP_OSX)
  FSRef ref;
  OSErr err = FSFindFolder(kUserDomain, kCurrentUserFolderType,
                           kDontCreateFolder, &ref);
  if (err == noErr)
      return arc_fsref_to_path(ctx, &ref);

#elif defined(SEXP_BEOS)

  char* home = getenv("HOME");
  if (home)
    return sexp_c_string(ctx, home, -1);
  
  return sexp_c_string(ctx, "/boot/home", -1);
#else

  char* home = getenv("HOME");
  if (home)
    return sexp_c_string(ctx, home, -1);
#endif

  return SEXP_FALSE;
}


/* (getcwd) => string */
static sexp arc_getcwd(sexp ctx)
{
  char cwd[PATH_MAX];
  getcwd(cwd, PATH_MAX);

  return sexp_c_string(ctx, cwd, -1);
}


/* (getpid) => int */
static sexp arc_getpid(sexp ctx)
{
  return sexp_make_integer(getpid());
}


/* (utime path [actime [modtime]]) => boolean */
static sexp arc_utime(sexp ctx, sexp_uint_t argc, sexp* args)
{
  if (argc >= 1) {
    sexp path = args[-1];
    sexp actime = SEXP_NULL;
    sexp modtime = SEXP_NULL;
    struct utimbuf buf;

    if (argc >= 2) {
      actime = args[-2];

      if (argc >= 3)
        modtime = args[-3];
    }
    
    if (!sexp_stringp(path))
      return sexp_type_exception(ctx, "not a string", path);

    if (actime != SEXP_NULL && !sexp_integerp(actime))
      return sexp_type_exception(ctx, "not an integer", actime);

    if (modtime != SEXP_NULL && !sexp_integerp(modtime))
      return sexp_type_exception(ctx, "not an integer", modtime);
      
    buf.actime = (time_t)sexp_unbox_integer(actime);
    buf.modtime = (time_t)sexp_unbox_integer(modtime);
    
    if (utime(sexp_string_data(path), &buf) == 0)
      return SEXP_TRUE;
  }
  
  return SEXP_FALSE;
}


/* (stat path) => #(dev ino mode nlink uid gid rdev size atime mtime 
                    ctime blksize blocks) ) */
static sexp arc_stat(sexp ctx, sexp path)
{
  struct stat fattr;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (stat(sexp_string_data(path), &fattr) == 0) {
    sexp v = sexp_make_vector(ctx, sexp_make_integer(13), SEXP_VOID);
    sexp_vector_data(v)[0] = sexp_make_integer(fattr.st_dev);
    sexp_vector_data(v)[1] = sexp_make_integer(fattr.st_ino);
    sexp_vector_data(v)[2] = sexp_make_integer(fattr.st_mode);
    sexp_vector_data(v)[3] = sexp_make_integer(fattr.st_nlink);
    sexp_vector_data(v)[4] = sexp_make_integer(fattr.st_uid);
    sexp_vector_data(v)[5] = sexp_make_integer(fattr.st_gid);
    sexp_vector_data(v)[6] = sexp_make_integer(fattr.st_rdev);
    sexp_vector_data(v)[7] = sexp_make_integer(fattr.st_size);
    sexp_vector_data(v)[8] = sexp_make_integer(fattr.st_atime);
    sexp_vector_data(v)[9] = sexp_make_integer(fattr.st_mtime);
    sexp_vector_data(v)[10] = sexp_make_integer(fattr.st_ctime);
    sexp_vector_data(v)[11] = sexp_make_integer(fattr.st_blksize);
    sexp_vector_data(v)[12] = sexp_make_integer(fattr.st_blocks);
/*       sexp_make_integer(fattr.st_type)); */
/*       sexp_make_integer(fattr.st_perms)); */
  }
  
  return SEXP_FALSE;
}


/* (mtime path) => int ) */
static sexp arc_mtime(sexp ctx, sexp path)
{
  struct stat fattr;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (stat(sexp_string_data(path), &fattr) == 0)
      return sexp_make_integer(fattr.st_mtime);
  
  return SEXP_FALSE;
}


/* (mtime path) => int ) */
static sexp arc_file_size(sexp ctx, sexp path)
{
  struct stat fattr;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (stat(sexp_string_data(path), &fattr) == 0)
      return sexp_make_integer(fattr.st_size);
  
  return SEXP_FALSE;
}


/* (system command args) => int) */
static sexp arc_system(sexp ctx, sexp command, sexp args)
{
  int status = 0;
  pid_t pid = 0;
  int i = 0;
  char** cmd_args = NULL;
  int cmd_args_count = 0;
  int cmdn = 0;

  if (!sexp_stringp(command))
    return sexp_type_exception(ctx, "not a string", command);

  /* at least the command name itself */
  cmd_args_count = 1;

  if (sexp_nullp(args)) {
    /* NOP */
  }
  else if (sexp_vectorp(args)) {
    for (i = 0; i < sexp_vector_length(args); i++) {
      if (!sexp_stringp(sexp_vector_data(args)[i]))
        return sexp_type_exception(ctx, "arg list contains non-strings", 
                                   sexp_vector_data(args)[i]);
      
      cmd_args_count++;
    }
  }
  else if (sexp_listp(ctx, args) == SEXP_TRUE) {
  }
  else
    return sexp_type_exception(ctx, "neither vector, list, nor null", args);
  

  /* +1 for the terminating NULL ptr */
  cmd_args_count++;
  cmd_args = calloc(cmd_args_count, sizeof(char*));
  cmd_args[cmdn++] = sexp_string_data(command);
    
  if (sexp_vectorp(args)) {
    for (i = 0; i < sexp_vector_length(args); i++) {
      cmd_args[cmdn++] = sexp_string_data(sexp_vector_data(args)[i]);
    }
  }
  else if (sexp_listp(ctx, args) == SEXP_TRUE) {
  }
  cmd_args[cmdn++] = NULL;
    
  pid = fork();
  if (pid == 0) {
    /* This is the child process.  Execute the shell command. */

/*
      if (new_in >= 0) {
        close(STDIN_FILENO);
        dup2(new_in, STDIN_FILENO);
      }
      if (new_out >= 0) {
        close(STDOUT_FILENO);
        dup2(new_out, STDOUT_FILENO);
      }
      if (new_err >= 0) {
        close(STDERR_FILENO);
        dup2(new_err, STDERR_FILENO);
      }
*/
    execvp(sexp_string_data(command), cmd_args);
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
  }
  
  free(cmd_args);
  
  return sexp_make_integer(status);
}


/* (chmod path mode) => boolean */
static sexp arc_chmod(sexp ctx, sexp path, sexp mode)
{
  unsigned int file_mode = 0644;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "not a string", path);

  if (sexp_integerp(mode))
    file_mode = sexp_unbox_integer(mode);
  else if (sexp_symbolp(mode)) {
    if (sexp_equalp(ctx, mode, sexp_intern(ctx, "file")) != SEXP_FALSE)
      file_mode = 0644;
    else if (sexp_equalp(ctx, mode, sexp_intern(ctx, "exec")) != SEXP_FALSE)
      file_mode = 0755;
    else
      return sexp_type_exception(ctx, "neither integer nor symbol", mode);

    if (chmod(sexp_string_data(path), file_mode) == 0)
      return SEXP_TRUE;
  }

  return SEXP_FALSE;
}


#define BUFFER_SIZE 4096

/* (copy-file from to) => boolean */
static sexp arc_copy_file(sexp ctx, sexp from, sexp to)
{
  FILE* dst = NULL;
  FILE* src = NULL;
  char* buffer = NULL;
  int bytesread = 0;

  if (!sexp_stringp(from))
    return sexp_type_exception(ctx, "not a string", from);
  if (!sexp_stringp(to))
    return sexp_type_exception(ctx, "not a string", to);
        
  if (!(src = fopen(sexp_string_data(from), "rb")))
    goto errhd;

  if (!(dst = fopen(sexp_string_data(to), "wb")))
    goto errhd;
    
  buffer = malloc(BUFFER_SIZE);

  while (1) {
    bytesread = fread(buffer, 1, BUFFER_SIZE, src);
    if (bytesread == 0) {
      if (ferror(src))
        goto errhd;
      else
        break;
    }
    
    if (fwrite(buffer, 1, bytesread, dst) != bytesread)
      goto errhd;
  }
  
  free(buffer);
  buffer = NULL;

  if (fclose(dst) != 0) {
    dst = NULL;
    goto errhd;
  }
  fclose(src);

  return SEXP_TRUE;

errhd:
  if (buffer)
    free(buffer);
  if (dst)
    fclose(dst);
  if (src)
    fclose(src);

  return SEXP_FALSE;
}


/* (quit code) */
static sexp arc_quit(sexp ctx, sexp code)
{
  if (!sexp_integerp(code))
    return sexp_type_exception(ctx, "not an integer", code);

  exit(sexp_unbox_integer(code));
  
  return SEXP_FALSE;
}



void arc_fsys_init(sexp ctx)
{
  sexp env = sexp_context_env(ctx);

#define DEFINE_EXTFUNC_0(_schemename, _func)     \
  env_define(ctx, env,                          \
             sexp_intern(ctx, _schemename),     \
             sexp_make_foreign0(ctx, _schemename, &_func))

#define DEFINE_EXTFUNC_1(_schemename, _func)     \
  env_define(ctx, env,                          \
             sexp_intern(ctx, _schemename),     \
             sexp_make_foreign1(ctx, _schemename, &_func))

#define DEFINE_EXTFUNC_2(_schemename, _func)     \
  env_define(ctx, env,                          \
             sexp_intern(ctx, _schemename),     \
             sexp_make_foreign2(ctx, _schemename, &_func))

#define DEFINE_EXTFUNC_X(_schemename, _func, _num, _var) \
  env_define(ctx, env,                              \
             sexp_intern(ctx, _schemename),         \
             sexp_make_foreign(ctx, _schemename, &_func, _num, _var))

  DEFINE_EXTFUNC_1("sys:getenv", arc_getenv);
  DEFINE_EXTFUNC_1("sys:unlink", arc_unlink);
  DEFINE_EXTFUNC_1("sys:rmdir", arc_rmdir);
  DEFINE_EXTFUNC_1("sys:chdir", arc_chdir);
  DEFINE_EXTFUNC_1("sys:file-exists?", arc_file_existsq);
  DEFINE_EXTFUNC_1("sys:file-directory?", arc_file_directoryq);
  DEFINE_EXTFUNC_1("sys:file-executable?", arc_file_executableq);

  DEFINE_EXTFUNC_1("sys:stat", arc_stat);
  DEFINE_EXTFUNC_1("sys:mtime", arc_mtime);
  DEFINE_EXTFUNC_1("sys:file-size", arc_file_size);

  DEFINE_EXTFUNC_1("quit", arc_quit);

  DEFINE_EXTFUNC_0("sys:current-time", arc_current_time);
  DEFINE_EXTFUNC_0("sys:tempdir", arc_tempdir);
  DEFINE_EXTFUNC_0("sys:homedir", arc_homedir);
  DEFINE_EXTFUNC_0("sys:getcwd", arc_getcwd);
  DEFINE_EXTFUNC_0("sys:getpid", arc_getpid);

  DEFINE_EXTFUNC_X("sys:utime", arc_utime, 0, 1);
    
  DEFINE_EXTFUNC_2("sys:mkdir", arc_mkdir);
  DEFINE_EXTFUNC_2("sys:symlink", arc_symlink);
  DEFINE_EXTFUNC_2("sys:chmod", arc_chmod);
  DEFINE_EXTFUNC_2("sys:copy-file", arc_copy_file);
  DEFINE_EXTFUNC_2("sys:system", arc_system);
}
