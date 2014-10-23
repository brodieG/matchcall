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
  SEXP sys_frame, sys_call, sf_target, sc_target, fun;
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
    if((type_tmp = TYPEOF(CAR(sys_call)) != LANGSXP))  // match.call allows EXPRSXP, and takes the first element, but we don't
      error(
        "Logic Error: system frames contains non-language (%s) element; contact maintainer.",
        type2char(type_tmp)
      );
    if(frame_len == par_off) {
      sf_target = CAR(sys_frame);
      sc_target = CAR(sys_call);
    }
  }
  if(sys_frame != R_NilValue || sys_call != R_NilValue)
    error("Logic Error: Call stack and frame stack of different lengths; contact maintainer.");

  if((R_xlen_t) par_off <= frame_len)
    error(
      "Argument `parent.offset` (%d) is greater than stack depth (%d)",
      par_off, frame_len
    );
  // Pull out function from relevant frame

  if(TYPEOF(CAR(sc_target)) == SYMSXP)
    PROTECT(fun = findFun(CAR(sc_target), sf_target));
  else
    PROTECT(fun = eval(CAR(sc_target), sf_target));

  if (TYPEOF(fun) != CLOSXP)
      error("Unable to find a closure from within which `match_call` was called");

  // - Do Matching -------------------------------------------------------------

  // SEXP formals, actuals, rlist;
  // SEXP funcall, f, b, rval, sysp, t1, t2, tail;
  // RCNTXT *cptr;
  // int expdots;

  // checkArity(op,args);

  // funcall = CADR(args);

  // if (TYPEOF(funcall) == EXPRSXP)
  // funcall = VECTOR_ELT(funcall, 0);

  //   if (TYPEOF(funcall) != LANGSXP)
  // error(_("invalid '%s' argument"), "call");

    /* Get the function definition */
    // sysp = R_GlobalContext->sysparent;   // Take this to be the parent frame

  //   if (TYPEOF(CAR(args)) == NILSXP) {
  // /* Get the env that the function containing */
  // /* matchcall was called from. */
  // cptr = R_GlobalContext;
  // while (cptr != NULL) {
  //     if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == sysp)
  //   break;
  //     cptr = cptr->nextcontext;
  // }
  // if ( cptr == NULL ) {
  //     sysp = R_GlobalEnv;
  //     errorcall(R_NilValue,
  //         "match.call() was called from outside a function");
  // } else
  //     sysp = cptr->sysparent;
  // if (cptr != NULL)
  //     /* Changed to use the function from which match.call was
  //        called as recorded in the context.  This change is
  //        needed in case the current function is computed in a
  //        way that cannot be reproduced by a second computation,
  //        or if it is a registered S3 method that is not
  //        lexically visible at the call site.

  //        There is one particular case where this represents a
  //        change from previous semantics: The definition is NULL,
  //        the call is supplied explicitly, and the function in
  //        the call is NOT the current function.  The new behavior
  //        is to ignore the function in the call and use the
  //        current function.  This is consistent with (my reading
  //        of) the documentation in both R and Splus.  However,
  //        the old behavior of R was consistent with the behavior
  //        of Splus (and inconsistent with the documentation in
  //        both cases).

  //        The previous semantics for this case can be restored by
  //        having the .Internal receive an additional argument
  //        that indicates whether the call was supplied explicitly
  //        or missing, and using the function recorded in the
  //        context only if the call was not supplied explicitly.
  //        The documentation should also be changed to be
  //        consistent with this behavior.  LT */
  //     PROTECT(b = duplicate(cptr->callfun));
  // else if ( TYPEOF(CAR(funcall)) == SYMSXP )
  //     PROTECT(b = findFun(CAR(funcall), sysp));
  // else
  //     PROTECT(b = eval(CAR(funcall), sysp));

  // if (TYPEOF(b) != CLOSXP)
  //     error(_("unable to find a closure from within which 'match.call' was called"));

  //   }
  //   else {
  // /* It must be a closure! */
  // PROTECT(b = CAR(args));
  // if (TYPEOF(b) != CLOSXP)
  //     error(_("invalid '%s' argument"), "definition");
  //   }

  //   /* Do we expand ... ? */

  //   expdots = asLogical(CAR(CDDR(args)));
  //   if (expdots == NA_LOGICAL)
  // error(_("invalid '%s' argument"), "expand.dots");

  //   /* Get the formals and match the actual args */

  //   formals = FORMALS(b);
  //   PROTECT(actuals = duplicate(CDR(funcall)));

  //   /* If there is a ... symbol then expand it out in the sysp env
  //      We need to take some care since the ... might be in the middle
  //      of the actuals  */

  //   t2 = R_MissingArg;
  //   for (t1=actuals ; t1!=R_NilValue ; t1 = CDR(t1) ) {
  // if (CAR(t1) == R_DotsSymbol) {
  //   t2 = subDots(sysp);
  //   break;
  // }
  //   }
  //   /* now to splice t2 into the correct spot in actuals */
  //   if (t2 != R_MissingArg ) {  /* so we did something above */
  // if( CAR(actuals) == R_DotsSymbol ) {
  //     UNPROTECT(1);
  //     actuals = listAppend(t2, CDR(actuals));
  //     PROTECT(actuals);
  // }
  // else {
  //     for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
  //   if( CADR(t1) == R_DotsSymbol ) {
  //       tail = CDDR(t1);
  //       SETCDR(t1, t2);
  //       listAppend(actuals,tail);
  //       break;
  //   }
  //     }
  // }
  //   } else { /* get rid of it */
  // if( CAR(actuals) == R_DotsSymbol ) {
  //     UNPROTECT(1);
  //     actuals = CDR(actuals);
  //     PROTECT(actuals);
  // }
  // else {
  //     for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
  //   if( CADR(t1) == R_DotsSymbol ) {
  //       tail = CDDR(t1);
  //       SETCDR(t1, tail);
  //       break;
  //   }
  //     }
  // }
  //   }
  //   rlist = matchArgs(formals, actuals, call);

  //   /* Attach the argument names as tags */

  //   for (f = formals, b = rlist; b != R_NilValue; b = CDR(b), f = CDR(f)) {
  // SET_TAG(b, TAG(f));
  //   }


  //   /* Handle the dots */

  //   PROTECT(rlist = ExpandDots(rlist, expdots));

  //   /* Eliminate any unmatched formals and any that match R_DotSymbol */
  //   /* This needs to be after ExpandDots as the DOTSXP might match ... */

  //   rlist = StripUnmatched(rlist);

  //   PROTECT(rval = allocSExp(LANGSXP));
  //   SETCAR(rval, duplicate(CAR(funcall)));
  //   SETCDR(rval, rlist);
  //   UNPROTECT(4);
  //   return rval;


  // - Finalize ----------------------------------------------------------------

  return ScalarLogical(1);
}
