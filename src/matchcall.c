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
SEXP MC_test (SEXP x);

static const
R_CallMethodDef callMethods[] = {
  {"match_call", (DL_FUNC) &MC_match_call, 8},
  {"test", (DL_FUNC) &MC_test, 1},
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
|                                  HELPER                                      |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_test(SEXP x) {
    return ScalarLogical(asReal(x) > 1);
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
  R_xlen_t par_off, frame_len = 0;
  SEXPTYPE sys_frames_type, sys_calls_type, type_tmp;
  SEXP sys_frame, sys_call;
  const char * dots_char;

  // - Validate ----------------------------------------------------------------

  // User Inputs

  if(
    TYPEOF(dots) != STRSXP || XLENGTH(dots) != 1 ||
    (
      strcmp((dots_char = CHAR(asChar(dots))), "expand") &&
      strcmp(dots_char, "exclude") && strcmp(dots_char, "include")
  ) )
    error("Argument `dots` must be character(1L) in `c(\"expand\", \"exclude\", \"include\")`.");
  if(
    TYPEOF(default_formals) != LGLSXP || XLENGTH(default_formals) != 1L ||
    asLogical(default_formals) == NA_LOGICAL
  )
    error("Argument `default.formals` must be logical(1L) and not NA.");
  if(
    TYPEOF(empty_formals) != LGLSXP || XLENGTH(empty_formals) != 1L ||
    asLogical(empty_formals) == NA_LOGICAL
  )
    error("Argument `empty.formals` must be logical(1L) and not NA.");
  if(
    TYPEOF(eval_formals) != LGLSXP || XLENGTH(eval_formals) != 1L ||
    asLogical(eval_formals) == NA_LOGICAL
  )
    error("Argument `empty.formals` must be logical(1L) and not NA.");
  if(
    TYPEOF(user_formals) != LGLSXP || XLENGTH(user_formals) != 1L ||
    asLogical(user_formals) == NA_LOGICAL
  )
    error("Argument `user.formals` must be logical(1L) and not NA.");
  if(
    (
      TYPEOF(parent_offset) != INTSXP && TYPEOF(parent_offset) != REALSXP
    ) || XLENGTH(parent_offset) != 1L || (par_off = asInteger(parent_offset)) < 1
  )
    error("Argument `parent_offset` must be integer(1L) and not less than one.");

  // Validate internal inputs; these should be the call and frame stack.  Haven't
  // figured out a way to get these directly from C so we rely on generating them
  // in R and feeding them to this function

  if(
    (sys_frames_type = TYPEOF(sys_frames)) == NILSXP ||
    (sys_calls_type = TYPEOF(sys_calls)) == NILSXP
  )
    error("This function must be invoked within a closure.");
  if(sys_frames_type != LISTSXP)
    error(
      "Logic Error: unexpected system frames type %s, should be a list of dotted pairs; contact maintainer.",
      type2char(sys_frames_type)
    );
  if(sys_calls_type != LISTSXP)
    error(
      "Logic Error: unexpected system calls type %s, should be a list of dotted pairs ; contact maintainer.",
      type2char(sys_calls_type)
    );
  for(
    sys_frame = sys_frames, sys_call = sys_calls;
    sys_frame != R_NilValue && sys_call != R_NilValue;
    sys_frame = CDR(sys_frames), sys_call = CDR(sys_calls), frame_len++
  ) {
    if((type_tmp = TYPEOF(CAR(sys_frame)) != ENVSXP))
      error(
        "Logic Error: system frames contains non-environment (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
    if((type_tmp = TYPEOF(CAR(sys_call)) != LANGSXP))
      error(
        "Logic Error: system frames contains non-language (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
  }
  if(sys_frame != R_NilValue || sys_call != R_NilValue)
    error("Logic Error: Call stack and frame stack of different lengths; contact maintainer.");

  if((R_xlen_t) par_off > frame_len)
    error(
      "Argument `parent.offset` (%d) is greater than stack depth (%d)",
      par_off, frame_len
    );

  // - Validate ----------------------------------------------------------------

  return ScalarLogical(1);
}
