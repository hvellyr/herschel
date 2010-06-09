#include "chibi/sexp.h"


sexp sexp_make_foreign0(sexp ctx, const char* name, foreign_call0 func_ptr)
{
  sexp func = sexp_alloc_type(ctx, foreign, SEXP_FOREIGN);
  sexp_foreign_num_args(func) = 0;
  sexp_foreign_func0(func) = func_ptr;
  sexp_foreign_flags(func) = 0;
  sexp_foreign_name(func) = name;
  return func;
}

sexp sexp_make_foreign1(sexp ctx, const char* name, foreign_call1 func_ptr)
{
  sexp func = sexp_alloc_type(ctx, foreign, SEXP_FOREIGN);
  sexp_foreign_num_args(func) = 1;
  sexp_foreign_func1(func) = func_ptr;
  sexp_foreign_flags(func) = 0;
  sexp_foreign_name(func) = name;
  return func;
}

sexp sexp_make_foreign2(sexp ctx, const char* name, foreign_call2 func_ptr)
{
  sexp func = sexp_alloc_type(ctx, foreign, SEXP_FOREIGN);
  sexp_foreign_num_args(func) = 2;
  sexp_foreign_func2(func) = func_ptr;
  sexp_foreign_flags(func) = 0;
  sexp_foreign_name(func) = name;
  return func;
}

sexp sexp_make_foreign3(sexp ctx, const char* name, foreign_call3 func_ptr)
{
  sexp func = sexp_alloc_type(ctx, foreign, SEXP_FOREIGN);
  sexp_foreign_num_args(func) = 3;
  sexp_foreign_func3(func) = func_ptr;
  sexp_foreign_flags(func) = 0;
  sexp_foreign_name(func) = name;
  return func;
}


sexp sexp_make_foreign(sexp ctx, const char* name, foreign_callX func_ptr, 
                       sexp_uint_t num_args, int variadic)
{
  sexp func = sexp_alloc_type(ctx, foreign, SEXP_FOREIGN);
  sexp_foreign_num_args(func) = (unsigned short) (sexp_uint_t) num_args;
  sexp_foreign_func(func) = func_ptr;
  sexp_foreign_flags(func) = variadic ? 1 : 0;
  sexp_foreign_name(func) = name;
  return func;
}


sexp sexp_make_extobj(sexp ctx, extobj_class* isa, void* obj)
{
  sexp eo = sexp_alloc_type(ctx, extobj, SEXP_EXTOBJ);
  sexp_extobj_isa(eo) = isa;
  sexp_extobj_obj(eo) = obj;
  return eo;
}



