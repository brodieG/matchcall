#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_match_call (  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP eval_formals,
  SEXP user_formals, SEXP parent_offset, SEXP sys_frames,
  SEXP sys_calls);

static const
R_CallMethodDef callMethods[] = {
  {"match_call", (DL_FUNC) &MC_match_call, 8},
  {NULL, NULL, 0}
};

void R_init_matchcall(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info,
  NULL, callMethods,
  NULL, NULL);
}

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                  MAIN FUN                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

/* Normal version, a little slower but more flexible */

SEXP MC_match_call (
  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP eval_formals,
  SEXP user_formals, SEXP parent_offset, SEXP sys_frames, SEXP sys_calls
) {
  R_xlen_t par_off, i, frame_len, call_len;
  SEXPTYPE sys_frames_type, sys_calls_type, type_tmp;
  const char * dots_char;

  // User Inputs

  if(
    TYPEOF(dots) != STRSXP || XLENGTH(dots) != 1 ||
    (
      strcmp((dots_char = CHAR(asChar(dots))), "expand") &&
      strcmp(dots_char, "exclude") && strcmp(dots_char, "include")
  ) )
    error("Argument `dots` must be character(1L) in `c(\"expand\", \"exclude\", \"include\")`.");
  if(TYPEOF(default_formals) != LGLSXP || XLENGTH(default_formals) != 1L)
    error("Argument `default.formals` must be logical(1L).");
  if(TYPEOF(empty_formals) != LGLSXP || XLENGTH(empty_formals) != 1L)
    error("Argument `empty.formals` must be logical(1L).");
  if(TYPEOF(eval_formals) != LGLSXP || XLENGTH(eval_formals) != 1L)
    error("Argument `empty.formals` must be logical(1L).");
  if(TYPEOF(user_formals) != LGLSXP || XLENGTH(user_formals) != 1L)
    error("Argument `user.formals` must be logical(1L).");
  if(
    (
      TYPEOF(parent_offset) != INTSXP && TYPEOF(parent_offset) != REALSXP
    ) || XLENGTH(parent_offset) != 1L || (par_off = asInteger(parent_offset)) < 0
  )
    error("Argument `parent_offset` must be integer(1L) and not less than zero.");

  // Internal inputs

  if(
    (sys_frames_type = TYPEOF(sys_frames)) == NILSXP ||
    (sys_calls_type = TYPEOF(sys_calls)) == NILSXP
  )
    error("Logic Error: unable to retrieve contexts and calls; contact maintainer.");
  if(sys_frames_type != VECSXP)
    error(
      "Logic Error: unexpected system frames type %s ; contact maintainer.",
      type2char(sys_frames_type)
    );
  if(sys_calls_type != VECSXP)
    error(
      "Logic Error: unexpected system calls type %s ; contact maintainer.",
      type2char(sys_calls_type)
    );
  if((frame_len = XLENGTH(sys_frames)) != (call_len = XLENGTH(sys_calls)))
    error(
      "Logic Error: length mismatch between calls (%d) and frames (%d); contact maintainer",
      XLENGTH(sys_calls), XLENGTH(sys_frames)
    );
  if((R_xlen_t) par_off > frame_len)
    error(
      "Argument `parent.offset` (%d) is greater than stack depth (%d)",
      par_off, frame_len
    );
  for(i = 0; i < frame_len; i++) {
    if((type_tmp = TYPEOF(VECTOR_ELT(sys_frames, i))) != ENVSXP)
      error(
        "Logic Error: system frames contains non-environment (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
    if((type_tmp = TYPEOF(VECTOR_ELT(sys_calls, i))) != ENVSXP)
      error(
        "Logic Error: system frames contains non-language (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
  }

  return ScalarLogical(1);
}
