#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"
#include "sys/stat.h"
#include <stdio.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/wait.h>
#include <dirent.h>

#include "chibi/sexp.h"


struct dirport_obj {
  DIR* dir;
};

static void dirport_obj_free(sexp ctx, void* obj) {
  struct dirport_obj* diro = (struct dirport_obj*)obj;

  if (diro) {
    if (diro->dir)
      closedir(diro->dir);
    free(diro);
  }
}


static void dirport_obj_mark(sexp ctx, sexp obj) {
}


static void dirport_obj_display(sexp ctx, void* obj, sexp out) {
  struct dirport_obj* diro = (struct dirport_obj*)obj;
  if (diro) {
    sexp_write_string(ctx, "#<dir-port> ", out);
  }
}


static extobj_class dirport_obj_class = {
  dirport_obj_free,
  dirport_obj_mark,
  dirport_obj_display
};


static struct dirport_obj* dirport_obj_alloc(const char* path)
{
  DIR* dir;

  if ((dir = opendir(path)) != NULL) {
    struct dirport_obj* obj = calloc(1, sizeof(struct dirport_obj));
    obj->dir = dir;
    return obj;
  }

  return NULL;
}


static sexp arc_dirport_p(sexp ctx, sexp port)
{
  if (sexp_extobj_p(port) && sexp_extobj_isa(port) == &dirport_obj_class)
    return SEXP_TRUE;
  return SEXP_FALSE;
}


static sexp arc_open_dir_port(sexp ctx, sexp path)
{
  struct dirport_obj* obj;

  if (!sexp_stringp(path))
    return sexp_type_exception(ctx, "open-dir-port: not a string", path);

  if ((obj = dirport_obj_alloc(sexp_string_data(path))))
    return sexp_make_extobj(ctx, &dirport_obj_class, obj);

  return SEXP_FALSE;
}


static sexp arc_read_dir_port(sexp ctx, sexp port)
{
  struct dirport_obj* obj;

  if (arc_dirport_p(ctx, port) == SEXP_FALSE)
    return sexp_type_exception(ctx, "read-dir-port: not a dir-port", port);

  obj = (struct dirport_obj*)sexp_extobj_obj(port);
  if (obj && obj->dir) {
    struct dirent* dp = readdir(obj->dir);
      
    if (dp)
      return sexp_c_string(ctx, dp->d_name, -1);
  }

  return SEXP_EOF;
}


static sexp arc_close_dir_port(sexp ctx, sexp port)
{
  struct dirport_obj* obj;

  if (arc_dirport_p(ctx, port) == SEXP_FALSE)
    return sexp_type_exception(ctx, "close-dir-port: not a dir-port", port);

  obj = (struct dirport_obj*)sexp_extobj_obj(port);
  if (obj && obj->dir) {
    closedir(obj->dir);
    obj->dir = NULL;

    return SEXP_TRUE;
  }

  return SEXP_FALSE;
}


void arc_dirport_init(sexp ctx)
{
  sexp env = sexp_context_env(ctx);

  env_define(ctx, env,
             sexp_intern(ctx, "open-dir-port"), 
             sexp_make_foreign1(ctx, "open-dir-port", &arc_open_dir_port));
  env_define(ctx, env,
             sexp_intern(ctx, "close-dir-port"), 
             sexp_make_foreign1(ctx, "close-dir-port", &arc_close_dir_port));
  env_define(ctx, env,
             sexp_intern(ctx, "read-dir-port"), 
             sexp_make_foreign1(ctx, "read-dir-port", &arc_read_dir_port));
  env_define(ctx, env,
             sexp_intern(ctx, "dirport?"), 
             sexp_make_foreign1(ctx, "dirport?", &arc_dirport_p));
}
