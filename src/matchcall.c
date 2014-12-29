#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_match_call (
  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP user_formals,
  SEXP parent_offset, SEXP definition, SEXP sys_frames, SEXP sys_calls,
  SEXP sys_pars);
SEXP MC_test1 (SEXP x);
SEXP MC_test2 ();
SEXP MC_test3 (SEXP x, SEXP y, SEXP z);
SEXP MC_get_frame_data(SEXP sys_frames, SEXP sys_calls, SEXP sys_pars, int par_off);
SEXP MC_get_fun(SEXP frame, SEXP call);

// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

SEXP MC_SYM_matchcall;
SEXP MC_SYM_quote;
SEXP MC_SYM_calls;
SEXP MC_SYM_frames;
SEXP MC_SYM_pars;

static const
R_CallMethodDef callMethods[] = {
  {"match_call", (DL_FUNC) &MC_match_call, 9},
  {"test1", (DL_FUNC) &MC_test1, 1},
  {"test2", (DL_FUNC) &MC_test2, 0},
  {"test3", (DL_FUNC) &MC_test3, 3},
  {NULL, NULL, 0}
};

void R_init_matchcall(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  MC_SYM_quote = install("quote");
  MC_SYM_matchcall = install("match.call");
  MC_SYM_calls = install("sys.calls");
  MC_SYM_frames = install("sys.frames");
  MC_SYM_pars = install("sys.parents");

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_RegisterCCallable("matchcall", "MC_match_call", (DL_FUNC) MC_match_call);
  R_RegisterCCallable("matchcall", "MC_get_frame_data", (DL_FUNC) MC_get_frame_data);
  R_RegisterCCallable("matchcall", "MC_get_fun", (DL_FUNC) MC_get_fun);
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                  HELPER                                      |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_test1(SEXP x) {
  return R_NilValue;
}
SEXP MC_test2() {
  SEXP c1 = PROTECT(list1(MC_SYM_calls)); SET_TYPEOF(c1, LANGSXP); eval(c1, R_GlobalEnv);
  SEXP c2 = PROTECT(list1(MC_SYM_frames)); SET_TYPEOF(c2, LANGSXP); eval(c2, R_GlobalEnv);
  SEXP c3 = PROTECT(list1(MC_SYM_pars)); SET_TYPEOF(c3, LANGSXP); eval(c3, R_GlobalEnv);
  UNPROTECT(3);
  return R_NilValue;
}
SEXP MC_test3(SEXP x, SEXP y, SEXP z) {
  return R_NilValue;
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
/*
Extracts relevant frame and call based on offset, and returns a linked list with:
  - CAR: the call
  - CADR: the stack parent of the call
  - CADDR: the frame of the call
*/

SEXP MC_get_frame_data(SEXP sys_frames, SEXP sys_calls, SEXP sys_pars, int par_off) {

  SEXPTYPE sys_frames_type, sys_calls_type, type_tmp;

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

  SEXP sys_frame, sys_call;
  int frame_len=0;

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
        "Logic Error: system calls contains non-language (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
    if(frame_len > 500000)
      error("Logic Error: frame depth > 500K, not supported; contact maintainer.");
  }
  if(sys_frame != R_NilValue || sys_call != R_NilValue) {
    error("Logic Error: Call stack and frame stack of different lengths; contact maintainer.");
  }
  if(frame_len == 1)
    error("You must run `match_call` within a closure, but it appears you are doing so from top level.");
  if(frame_len <= par_off)
    error(
      "Logic Error: offset (%d) is greater than stack depth (%d)",
      par_off, frame_len - 1
    );
  if(frame_len != XLENGTH(sys_pars)) {
    error(
      "Logic Error: Mismatch between number of frames (%d) and length of `sys.parents()` (%d); contact maintainer",
      frame_len, XLENGTH(sys_pars)
    );
  }
  // Need to get call and the parent frame of the call based on the offset value
  // Unless no offset, then use last call in stack

  int par_off_count, frame_stop, frame_stop_prev, call_stop, call_stop_prev;
  call_stop = call_stop_prev = frame_len;

  // Find parent call using `sys.parents()` data

  for(par_off_count = par_off; par_off_count >= 1; par_off_count--) {
    call_stop_prev = call_stop;  // trac previous
    call_stop = call_stop > 1 ? INTEGER(sys_pars)[call_stop - 1] : 0;
  }
  frame_stop = call_stop ? INTEGER(sys_pars)[call_stop - 1] : 0; // Now the frame to evaluate the parent call in
  frame_stop_prev = call_stop_prev ? INTEGER(sys_pars)[call_stop_prev - 1] : 0;

  if(frame_stop_prev < frame_stop)
    error("Logic Error: previous frame must be greater than next; contact maintainer.");
  if(frame_stop < 0)
    error("Logic Error: frame must be positive; contact maintainer.");
  // Now that we know what frame we want, get it

  SEXP sf_target=R_NilValue, sf_target_prev=R_NilValue, sc_target;

  for(
    sys_frame = sys_frames, frame_len = 1;
    sys_frame != R_NilValue; sys_frame = CDR(sys_frame), frame_len++
  ) {
    if(frame_len == frame_stop) sf_target = CAR(sys_frame);
    if(frame_len == frame_stop_prev) sf_target_prev = CAR(sys_frame);
  }
  if(sf_target == R_NilValue) sf_target = R_GlobalEnv;
  if(sf_target_prev == R_NilValue) sf_target_prev = R_GlobalEnv;

  // Get call as well

  int call_len, found_call=0;

  for(
    sys_call = sys_calls, call_len = 1; sys_call != R_NilValue;
    sys_call = CDR(sys_call), call_len++
  ) {
    if(call_len == call_stop) {
      sc_target = CAR(sys_call);
      found_call = 1;
      break;
    }
  }
  if(!found_call)
    error("Logic Error: unable to find call in call stack; contact maintainer.");

  // Return everything in R list

  return(list3(sc_target, sf_target, sf_target_prev));
}
/*
Get fun from call and frame data
*/
SEXP MC_get_fun(SEXP frame, SEXP call) {
  SEXP fun;
  if(TYPEOF(CAR(call)) == SYMSXP)
    PROTECT(fun = findFun(CAR(call), frame));
  else
    PROTECT(fun = eval(CAR(call), frame));
  if(TYPEOF(fun) != CLOSXP)
    error("Logic Error: Unable to find a closure to match; contact maintainer.");
  UNPROTECT(1);
  return(fun);
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                  MAIN FUN                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP MC_match_call (
  SEXP dots, SEXP default_formals, SEXP empty_formals, SEXP user_formals,
  SEXP parent_offset, SEXP definition, SEXP sys_frames, SEXP sys_calls,
  SEXP sys_pars
) {
  R_xlen_t par_off;  // Being a bit sloppy about what is really an int vs R_xlen_t; likely need to clean up at some point
  SEXP fun, actuals, t2, t1;
  const char * dots_char;
  int def_frm, empt_frm, usr_frm;

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
    (def_frm = asLogical(default_formals)) == NA_LOGICAL
  )
    error("Argument `default.formals` must be logical(1L) and not NA.");
  if(
    TYPEOF(empty_formals) != LGLSXP || XLENGTH(empty_formals) != 1L ||
    (empt_frm = asLogical(empty_formals)) == NA_LOGICAL
  )
    error("Argument `empty.formals` must be logical(1L) and not NA.");
  if(
    TYPEOF(user_formals) != LGLSXP || XLENGTH(user_formals) != 1L ||
    (usr_frm = asLogical(user_formals)) == NA_LOGICAL
  )
    error("Argument `user.formals` must be logical(1L) and not NA.");
  if(
    (
      TYPEOF(parent_offset) != INTSXP && TYPEOF(parent_offset) != REALSXP
    ) || XLENGTH(parent_offset) != 1L || (par_off = asInteger(parent_offset)) < 0
  )
    error("Argument `n` must be integer(1L) and not less than zero.");
  if(definition != R_NilValue && TYPEOF(definition) != CLOSXP)
    error("Argument `definition` must be a closure if provided.");

  // Technically unncessary as done as part of MC_get_frame_data, but done here
  // for error message clarity

  R_xlen_t stack_depth;
  if((stack_depth = length(sys_frames)) <= par_off) {
    error("Argument `n` must be less than stack stack depth (%d)\n", stack_depth);
  }
  // - Get Frame and Calls -----------------------------------------------------

  SEXP frame_call = PROTECT(MC_get_frame_data(sys_frames, sys_calls, sys_pars, par_off));
  SEXP sf_target, sc_target;

  // These should still be protected as they are pointed to by sys_frames and
  // sys_calls

  sf_target = CADR(frame_call);
  sc_target = CAR(frame_call);

  // - Dots --------------------------------------------------------------------

  // Get function definition from relevant frame, or use provided one if available

  if(definition != R_NilValue) {
    fun = PROTECT(definition); // unnecessary PROTECT for stack balance
  } else {
    fun = PROTECT(MC_get_fun(sf_target, sc_target));
  }
  PROTECT(actuals = CDR(sc_target));

  /* Note, this is more or less directly from R sources...

     If there is a ... symbol then expand it out in the sysp env
     We need to take some care since the ... might be in the middle
     of the actuals  */

  t2 = R_MissingArg;
  int found_dots = 0;
  for (t1 = actuals ; t1 != R_NilValue ; t1 = CDR(t1) ) {
    if (CAR(t1) == R_DotsSymbol) {
      t2 = PROTECT(getDots(sf_target));
      found_dots = 1;
      break;
    }
  }
  if(!found_dots) PROTECT(R_MissingArg); // stack balance

  SEXP tail;

  if (t2 != R_MissingArg && strcmp(dots_char, "exclude")) {  /* so we did something above */
    if(CAR(actuals) == R_DotsSymbol ) {
      actuals = listAppend(t2, CDR(actuals));
    } else {
      for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
        if( CADR(t1) == R_DotsSymbol ) {
          tail = CDDR(t1);
          SETCDR(t1, t2);
          actuals = listAppend(actuals, tail);
          break;
    } } }
  } else if(t2 != R_MissingArg) { /* get rid of it */
    if(CAR(actuals) == R_DotsSymbol ) {
        actuals = CDR(actuals);
    } else {
      for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
        if(CADR(t1) == R_DotsSymbol) {
          tail = CDDR(t1);
          SETCDR(t1, tail);
          break;
    } } }
  }
  // Reconstruct the original call in case we messed with dots

  SETCDR(sc_target, actuals);

  // - Invoke `match.call` -----------------------------------------------------

  // Manufacture call to `match.call` now that we have found the dots (this is
  // taken from Writing R Extensions).

  SEXP t, u;  // Need to create a quoted version of the call we captured
  u = PROTECT(list2(MC_SYM_quote, sc_target));
  SET_TYPEOF(u, LANGSXP);

  t = PROTECT(list4(MC_SYM_matchcall, fun, u, ScalarLogical(0)));
  SET_TYPEOF(t, LANGSXP);

  SEXP match_res = PROTECT(eval(t, sf_target));

  // - Manipulate Result -------------------------------------------------------

  SEXP match_track2, match_track = R_NilValue;
  SEXP matched, matched2;
  matched = CDR(match_res);

  // Dummy head so we can use same logic when inserting at front or middle or
  // back of pair list

  SEXP matched_prev_cpy = PROTECT(allocList(1));
  SEXP match_track_prev = PROTECT(allocList(1));
  SETCDR(matched_prev_cpy, matched);
  SEXP matched_prev = matched_prev_cpy;

  /*
  Logic here is to compare matched arguments and formals pair-wise. In theory
  these are in the same order with potentially default arguments missing, so
  we just loop and sub in defaults when they are missing from matched
  arguments.  Some complexities arise from illegally missing formals, but
  basically they can just be treated the same way.
  */

  SEXP formals, form_cp, form_tag;
  formals = PROTECT(duplicate(FORMALS(fun)));

  for(
    matched2 = matched, form_cp = formals;
    form_cp != R_NilValue; form_cp = CDR(form_cp)
  ) {
    int form_mode = 1, form_len = 1, form_drop = 0;
    SEXP form_new = R_NilValue, form_new_last = R_NilValue;

    if(TAG(matched2) != (form_tag = TAG(form_cp))) {
      // This is an illegally missing formal
      if(CAR(form_cp) == R_MissingArg) {
        // If we don't want to keep missing formals, or dots are missing and we
        // are asking to expand them, dump the formal
        form_mode = 3;
        if(
          !empt_frm ||
          (form_tag == R_DotsSymbol &&
            (!strcmp("expand", dots_char) || !strcmp("exclude", dots_char))
          )
        ) continue;
      } else {
        form_mode = 2;  // This is a default formal
        if(!def_frm) continue;
        if(form_tag == R_DotsSymbol)
          error("Logic Error: default value provided for dots arguments; contact maintainer.");
      }
      // Add the formal to our list

      form_new = PROTECT(allocList(1));
      SETCAR(form_new, CAR(form_cp));
      SET_TAG(form_new, form_tag);
    } else if (usr_frm) {
      // User provided formal, only special handling we need is dots
      form_mode = 1;
      PROTECT(R_NilValue);  // stack balance
      if(form_tag == R_DotsSymbol) {
        if(!strcmp("exclude", dots_char)) {
          form_drop = 1;
          form_new = R_NilValue;
        } else if(!strcmp("expand", dots_char)) {
          form_drop = 1;
          form_new = CAR(matched2);
          form_len = length(form_new);
      } }
    } else if (!usr_frm) {
      PROTECT(R_NilValue);  // stack balance
      form_mode = form_drop = 1;
      form_new = R_NilValue;
    } else {
      error("Logic Error: should never get here 445; contact maintainer.");
    }
    // Update our originally matched call

    if(form_drop) matched2 = CDR(matched2);
    if(form_new != R_NilValue) {
      for(
        form_new_last = form_new; CDR(form_new_last) != R_NilValue;
        form_new_last = CDR(form_new_last)
      ) NULL;
      if(matched2 != R_NilValue) listAppend(form_new, matched2);
    } else {
      form_new = matched2;
    }
    if(matched_prev_cpy != R_NilValue) {
      SETCDR(matched_prev_cpy, form_new);
    } else error("Logic Error: matched prev should never be null; contact maintainer.");

    // if matched user formal, need to advance to match to next formal in loop

    if(!form_drop) {
      if(form_new_last != R_NilValue) {
        matched_prev_cpy = form_new_last;
      } else if(matched2 != R_NilValue) {
        matched_prev_cpy = matched2;
      }
      if(form_mode == 1) matched2 = CDR(matched2);
    }
    // Update the tracking list

    SEXP track_new_cp, track_new_cp_last = R_NilValue,
      track_new = PROTECT(allocList(form_len));

    for(
      track_new_cp = track_new; track_new_cp != R_NilValue;
      track_new_cp = CDR(track_new_cp)
    ) {
      SETCAR(track_new_cp, ScalarInteger(form_mode));
      if(CDR(track_new_cp) == R_NilValue) track_new_cp_last = track_new_cp;
    }
    if(track_new_cp_last == R_NilValue)
      error("Logic Error: last value not set in tracking list; contact maintainer.");

    SETCDR(match_track_prev, track_new);
    UNPROTECT(2);

    // Advance the pointers

    match_track_prev = CDR(match_track_prev);
  }
  SETCDR(match_res, CDR(matched_prev));

  // - Finalize ----------------------------------------------------------------

  UNPROTECT(10);
  return match_res;
}
