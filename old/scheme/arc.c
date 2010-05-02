/* main.c -- chibi-scheme command-line app              */
/* Copyright (c) 2009 Alex Shinn.  All rights reserved. */
/* BSD-style license: http://synthcode.com/license.txt  */

#include <sys/stat.h>
#include "chibi/eval.h"

#include "dirport.h"
#include "fsys.h"
#include "scmenv.h"


static const char* arc_init_file = INIT_FILE_NAME;

char *chibi_module_dir = NULL;

static char* concat_str(const char* one, const char* two)
{
  int len1 = strlen(one);
  int len2 = strlen(two);
  char* three = malloc(len1 + len2 + 2);
  strcpy(three, one);
  strcat(three, "/");
  strcat(three, two);

  return three;
}


static sexp find_module_file(sexp ctx, const char* file, int req)
{
  sexp res = SEXP_FALSE;
  char *path;
  struct stat buf;
  const char* home_dir = NULL;

  if (stat(file, &buf) == 0)
    return sexp_c_string(ctx, (char*)file, -1);

  home_dir = getenv("ARC_HOME");
  if (!home_dir)
    home_dir = ARC_HOME;

  path = concat_str(home_dir, file);
  if (stat(path, &buf) == 0)
    res = sexp_c_string(ctx, path, -1);

  if (res == SEXP_FALSE) {
    const char* arc_init_dir = getenv("ARC_INIT_DIR");
    if (arc_init_dir) {
      free(path);

      path = concat_str(arc_init_dir, file);

      if (stat(path, &buf) == 0)
        res = sexp_c_string(ctx, path, -1);
    }
  }

  if (req && res == SEXP_FALSE) {
    fprintf(stderr, "ERROR: couldn't open input file: '%s'\n", path);
    exit(-1);
  }

  free(path);

  return res;
}


void repl (sexp ctx) {
  sexp tmp, res, env, in, out, err;
  sexp_gc_var(ctx, obj, s_obj);
  sexp_gc_preserve(ctx, obj, s_obj);
  env = sexp_context_env(ctx);
  sexp_context_tracep(ctx) = 1;
  in = sexp_eval_string(ctx, "(current-input-port)", env);
  out = sexp_eval_string(ctx, "(current-output-port)", env);
  err = sexp_eval_string(ctx, "(current-error-port)", env);
  while (1) {
    sexp_write_string(ctx, "> ", out);
    sexp_flush(ctx, out);
    obj = sexp_read(ctx, in);
    if (obj == SEXP_EOF)
      break;
    if (sexp_exceptionp(obj)) {
      sexp_print_exception(ctx, obj, err);
    } else {
      tmp = sexp_env_bindings(env);
      sexp_context_top(ctx) = 0;
      res = sexp_eval(ctx, obj, env);
#if USE_WARN_UNDEFS
      sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, err);
#endif
      if (res != SEXP_VOID) {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
      }
    }
  }
  sexp_gc_release(ctx, obj, s_obj);
}


static void arc_define_program_args(sexp ctx, sexp env, int argc, char** argv)
{
  sexp args_vector = sexp_make_vector(ctx, sexp_make_integer(argc), SEXP_VOID);
  int i;

  for (i = 0; i < argc; i++)
    sexp_vector_data(args_vector)[i] = sexp_c_string(ctx, argv[i], -1);

  env_define(ctx, env, sexp_intern(ctx, "*args*"), args_vector);
}


static sexp arc_home(sexp ctx)
{
  char* home = getenv("ARC_HOME");
  if (home)
    return sexp_c_string(ctx, home, -1);

  return sexp_c_string(ctx, ARC_HOME, -1);
}


void run_main(int argc, char **argv)
{
  sexp env;
  sexp out = NULL;
  sexp ctx;

  sexp_gc_var(ctx, str, s_str);

  ctx = sexp_make_context(NULL, NULL, NULL);
  sexp_gc_preserve(ctx, str, s_str);

  env = sexp_context_env(ctx);
  out = sexp_eval_string(ctx, "(current-output-port)", env);

  arc_dirport_init(ctx);
  arc_fsys_init(ctx);

  env_define(ctx, env,
             sexp_intern(ctx, "arc:home"),
             sexp_make_foreign0(ctx, "arc:home", &arc_home));


  if (argc >= 2) {
    if (strcmp(argv[1], "-s") == 0) {
      if (strcmp(argv[2], "-") == 0) {
        sexp_load(ctx, str = find_module_file(ctx, sexp_init_file, 1), env);
        arc_define_program_args(ctx, env, 0, NULL);
        repl(ctx);
        goto handle_end;
      }
      else {
        sexp_load(ctx, str = find_module_file(ctx, sexp_init_file, 1), env);

        if (argc > 3)
          arc_define_program_args(ctx, env, argc - 3, &argv[3]);
        else
          arc_define_program_args(ctx, env, 0, NULL);

        sexp_load(ctx, str = sexp_c_string(ctx, argv[2], -1), env);

        goto handle_end;
      }
    }
  }

  sexp_load(ctx, str = find_module_file(ctx, sexp_init_file, 1), env);

  arc_define_program_args(ctx, env, argc - 1, &argv[1]);

  sexp_load(ctx, str = find_module_file(ctx, arc_init_file, 1), env);

handle_end:
  sexp_gc_release(ctx, str, s_str);
}


int main (int argc, char **argv)
{
  sexp_scheme_init();
  run_main(argc, argv);

  return 0;
}

