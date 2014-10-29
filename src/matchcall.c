#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_match_call (
  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP eval_formals,
  SEXP user_formals, SEXP parent_offset, SEXP sys_frames,
  SEXP sys_calls, SEXP sys_pars);
SEXP MC_test (SEXP x);

static const
R_CallMethodDef callMethods[] = {
  {"match_call", (DL_FUNC) &MC_match_call, 9},
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
  SEXP s, t, u;
  PROTECT(s = allocList(2));
  SETCAR(s, ScalarLogical(1));
  SETCADR(s, ScalarLogical(0));

  t = CAR(s);
  u = CADR(s);

  UNPROTECT(1);
  return ScalarLogical(asReal(x) > 1);
}
// Based on subDots in src/main/unique.c

SEXP getDots(SEXP rho)
{
    SEXP rval, dots, a, b, t;
    int len,i;

    // Not sure that `findVars` makes sense here; do we really want to start
    // diving down the lexical stack to find dots?

    dots = findVar(R_DotsSymbol, rho);

    if (dots == R_UnboundValue)
      error("... used in a situation where it does not exist");

    if (dots == R_MissingArg)
      return dots;

    len = length(dots);
    PROTECT(rval=allocList(len));
    for(a = dots, b = rval, i = 1; i <= len; a = CDR(a), b = CDR(b), i++) {
      SET_TAG(b, TAG(a));
      t = CAR(a);
      while (TYPEOF(t) == PROMSXP)
        t = PREXPR(t);
      SETCAR(b, t);
    }
    UNPROTECT(1);
    return rval;
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                  MAIN FUN                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

/* Normal version, a little slower but more flexible */

SEXP MC_match_call (
  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP eval_formals,
  SEXP user_formals, SEXP parent_offset, SEXP sys_frames, SEXP sys_calls,
  SEXP sys_pars
) {
  R_xlen_t par_off, frame_len = 0, frame_stop, call_stop, par_off_count;  // Being a bit sloppy about what is really an int vs R_xlen_t; likely need to clean up at some point
  SEXPTYPE sys_frames_type, sys_calls_type, type_tmp;
  SEXP sys_frame, sys_call, sf_target, sc_target, fun, actuals, t2, t1;
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
    error("Argument `n` must be integer(1L) and not less than one.");

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
  if(TYPEOF(sys_pars) != INTSXP)
    error(
      "Logic Error: unexpected system calls type %s, should be a list of an integer vector ; contact maintainer.",
      type2char(TYPEOF(sys_pars))
    );

  // - Retrieve Call & Frame ---------------------------------------------------

  // Need to count frames b/c we need to calculate the offset from the end of
  // the frame list


  for(
    sys_frame = sys_frames, sys_call = sys_calls;
    sys_call != R_NilValue && sys_frame != R_NilValue;
    sys_frame = CDR(sys_frame), sys_call = CDR(sys_call)
  ) {
    frame_len++;
    if((type_tmp = TYPEOF(CAR(sys_frame)) != ENVSXP))
      error(
        "Logic Error: system frames contains non-environment (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
    if((type_tmp = TYPEOF(CAR(sys_call)) != LANGSXP))  // match.call allows EXPRSXP, and takes the first element, but we don't
      error(
        "Logic Error: system frames contains non-language (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
  }
  if(sys_frame != R_NilValue || sys_call != R_NilValue) {
    error("Logic Error: Call stack and frame stack of different lengths; contact maintainer.");
  }
  if(frame_len == 1)
    error("You must run `match_call` within a closure, but it appears you are doing so from top level.");
  if(frame_len <= par_off)
    error(
      "Argument `n` (%d) is greater than stack depth (%d)",
      par_off, frame_len - 1
    );
  if(frame_len != XLENGTH(sys_pars)) {
    error(
      "Logic Error: Mismatch between number of frames (%d) and length of `sys.parents()` (%d); contact maintainer",
      frame_len, XLENGTH(sys_pars)
    );
  }

  call_stop = INTEGER(sys_pars)[frame_len - 1];  // First parent

  for(par_off_count = par_off; par_off_count > 1; par_off_count--) {
    call_stop = call_stop > 1 ? INTEGER(sys_pars)[call_stop - 1] : 0;      // Find parent call using `sys.parents()` data
  }
  frame_stop = call_stop ? INTEGER(sys_pars)[call_stop - 1] : 0; // Now the frame to evaluate the parent call in
  // PrintValue(sys_calls);
  // PrintValue(sys_pars);
  // Rprintf("fstop: %d cstop: %d paroff: %d framelen: %d", frame_stop, call_stop,
  //   par_off, frame_len);

  // Now that we know what frame we want, get it

  if(!frame_stop) {
    sf_target = R_GlobalEnv;                 // Ran out of frames, so look in global env
  } else if(frame_stop > 0) {
    for(
      sys_frame = sys_frames, frame_len = 1;
      sys_frame != R_NilValue; sys_frame = CDR(sys_frame), frame_len++
    )
      if(frame_len == frame_stop) sf_target = CAR(sys_frame);
  } else {
    error("Logic Error: attempting to match to negative frame; contact maintainer.");
  }
  // Get call as well

  for(
    sys_call = sys_calls, frame_len = 1; sys_call != R_NilValue;
    sys_call = CDR(sys_call), frame_len++
  )
    if(frame_len == call_stop) sc_target = PROTECT(CAR(sys_call));

  // - Dots --------------------------------------------------------------------

  // Pull out function from relevant frame

  if(TYPEOF(CAR(sc_target)) == SYMSXP)
    PROTECT(fun = findFun(CAR(sc_target), sf_target));
  else
    PROTECT(fun = eval(CAR(sc_target), sf_target));

  if (TYPEOF(fun) != CLOSXP)
      error("Unable to find a closure from within which `match_call` was called");

  PROTECT(actuals = CDR(sc_target));

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

  t2 = R_MissingArg;
  for (t1 = actuals ; t1 != R_NilValue ; t1 = CDR(t1) ) {
    if (CAR(t1) == R_DotsSymbol) {
      t2 = PROTECT(getDots(sf_target));
      break;
    }
  }
  SEXP tail;

  if (t2 != R_MissingArg && strcmp(CHAR(asChar(dots)), "exclude")) {  /* so we did something above */
    if(CAR(actuals) == R_DotsSymbol ) {
      UNPROTECT(2);
      actuals = listAppend(t2, CDR(actuals));
      PROTECT(actuals);
    } else {
      for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
        if( CADR(t1) == R_DotsSymbol ) {
          tail = CDDR(t1);
          SETCDR(t1, t2);
          UNPROTECT(2);
          actuals = listAppend(actuals, tail);
          PROTECT(actuals);
          break;
        }
      }
    }
  } else { /* get rid of it */
    if( CAR(actuals) == R_DotsSymbol ) {
        UNPROTECT(1);
        actuals = CDR(actuals);
        PROTECT(actuals);
    } else {
      for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
        if(CADR(t1) == R_DotsSymbol) {
          tail = CDDR(t1);
          SETCDR(t1, tail);
          break;
  } } } }
  // Reconstruct the original call in case we messed with dots

  SETCDR(sc_target, actuals);

  // - Invoke `match.call` -----------------------------------------------------

  // Manufacture call to `match.call` now that we have found the dots (this is
  // taken from Writing R Extensions).  Note, we're erring on the side of
  // overPROTECTing here.

  SEXP t, u, v;  // Need to create a quoted version of the call we captured
  u = v = PROTECT(allocList(2));
  SET_TYPEOF(u, LANGSXP);
  SETCAR(u, PROTECT(install("quote")));
  v = PROTECT(CDR(u));
  SETCAR(v, sc_target);

  t = PROTECT(allocList(4));
  SETCAR(t, PROTECT(install("match.call")));
  SETCADR(t, fun);
  SETCADDR(t, u);
  SETCADDDR(t, PROTECT(ScalarLogical(0)));  // Do not expand dots ever, done below
  SET_TYPEOF(t, LANGSXP);

  // PrintValue(t);

  SEXP match_res;

  match_res = PROTECT(eval(t, sf_target));

  // - Manipulate Result -------------------------------------------------------

  SEXP matched, matched2;
  matched = CDR(match_res);

  // Add default formals if needed

  if(LOGICAL(default_formals)[0] || asLogical(empty_formals)) {
    SEXP formals, form_cpy, matched_tail, matched_prev;
    int one_match = 0; // Indicates we've had one TAG match between formals and matched args, which changes our appending strategy
    formals = FORMALS(fun);
    matched_prev = matched;

    /*
    Logic here is to compare matched arguments and formals pair-wise. In theory
    these are in the same order with potentially default arguments missing, so
    we just loop and sub in defaults when they are missing from matched
    arguments.  Some complexities arise from illegally missing formals, but
    basically they can just be treated the same way
    */

    for(
      matched2 = matched; formals != R_NilValue;
      formals=CDR(formals)
    ) {
      if(TAG(matched2) != TAG(formals)) {
        int missing=0;

        if(CAR(formals) == R_MissingArg) {  // This is an illegally missing formal
          missing=1;
          if(!asLogical(empty_formals)) {
            continue;
        } }
        /*
        strategy is to make a copy of the formals, append, advance one, and
        then re-attach the rest of the match arguments
        */
        if((asLogical(empty_formals) && missing) || asLogical(default_formals)) {
          if(one_match) {  // Already have one matched
            form_cpy = PROTECT(duplicate(formals));
            matched_tail = matched2;
            matched2 = matched_prev;
            SETCDR(matched2, form_cpy);
            UNPROTECT(1);
            matched2 = CDR(matched2);
            SETCDR(matched2, matched_tail);
          } else {         // Don't have any matched yet, so add default formals at front
            form_cpy = PROTECT(duplicate(formals));
            matched_tail = matched2;
            matched2 = form_cpy;
            SETCDR(matched2, matched_tail);
            matched = matched_prev = matched2;
            one_match = 1;
            UNPROTECT(1);
          }
        }
      } else {
        one_match = 1;
      }
      if(matched2 != R_NilValue)
        matched_prev = matched2;
      matched2 = CDR(matched2);  // Need to advance here b/c if we continue, we do not want to advance matched2
  } }
  // Expand or drop dots as appropriate

  if(!strcmp("exclude", CHAR(asChar(dots)))) {  // Has to be expand or exclude
    if(TAG(matched) == R_DotsSymbol) {
      matched = CDR(matched);                   // Drop dots
    } else {
      for(matched2 = matched; matched2 != R_NilValue; matched2 = CDR(matched2)) {
        if(TAG(CDR(matched2)) == R_DotsSymbol) {
          tail = CDDR(matched2);
          SETCDR(matched2, tail);               // Drop dots
          break;
    } } }
  } else if (!strcmp("expand", CHAR(asChar(dots)))) {
    if(TAG(matched) == R_DotsSymbol) {
      if(TYPEOF(CAR(matched)) != 2)
        error("Logic Error, expected a pair list as the values");
      matched = listAppend(CAR(matched), CDR(matched));  // Expand dots
    } else {
      for(matched2 = matched; matched2 != R_NilValue; matched2 = CDR(matched2)) {
        if(TAG(CDR(matched2)) == R_DotsSymbol) {
          tail = CDDR(matched2);
          SETCDR(matched2, CADR(matched2));
          listAppend(matched2, tail); // Expand dots
          break;
    } } }
  } else if(strcmp("include", CHAR(asChar(dots)))) {
    error("Logic Error: unexpected `dots` argument value %s", CHAR(asChar(dots)));
  }
  SETCDR(match_res, matched);

  // - Finalize ----------------------------------------------------------------

  UNPROTECT(10);
  return match_res;
}
