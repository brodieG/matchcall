#' Retrieve All Closure Arguments
#'
#' Similar to \code{`\link{match.call}`}, but includes formals that are not
#' specified by user (e.g. default values).
#'
#' Must be called within closure you are trying to retrieve arguments for, or
#' optionally further down the call stack if you adjust the \code{`parent.offset`}
#' argument accordingly.
#'
#' @note There is a lot of non-standard dynamic scoping going on within this function
#' to deal with potential corner cases, so if you are using it in a particularly
#' non-standard context it may not work as expected.  Efforts have been taken to
#' attempt to ensure that behavior is as expected in most scenarios, but I can't
#' fully guarantee this.
#'
#' @export
#' @param dots "exclude": do not include dots, "include": include them, "expand":
#'   include and expand (note last two only include dots if there are actually
#'   args matched by dots)
#' @param default.formals set to TRUE to include formals not specified in call
#'   though under no circumstances will it return ellipses even if you do
#'   something like \code{function(a, ...=list(1, 2, 3))} which oddly R appears
#'   to tolerate in closure definitions.
#' @param empty.formals set to TRUE to include formals that were not specified
#'   and do not have default values
#' @param eval.formals set to TRUE if you want the argument values to be
#'   evaluated (defaults in invoking function body, rest in parent of invoking
#'   function).  Note it is the argument expression that is evaluated, not the
#'   argument itself.  This should only make a difference in functions where
#'   the argument is modified in the function body before \code{`match_call`}
#'   is invoked (this will return the original expression, not the modified one).
#' @param user.formals set to FALSE if you want to exclude arguments that the
#'   user specified; this should almost never be needed unless you specifically
#'   want to know what arguments are using default values
#' @param parent.offset positive integer 1 length how many parents up the chain
#'   should the match_call() be done on
#' @return the call that invoked the function match_call() is invoked from (as a
#'   list if `eval.formals`==TRUE)
#' @useDynLib matchcall, .registration=TRUE, .fixes="MC_"
#' @examples
#' fun <- function(a, b, c=TRUE, ...) {
#'   match_call(default.formals=TRUE, dots="include")
#' }
#' fun(5, 6, x=list(1:10, FALSE))

match_call <- function(dots="expand", default.formals=FALSE, empty.formals=FALSE,
  eval.formals=FALSE, user.formals=TRUE, parent.offset=0L, bypass.checks=FALSE)
  .Call(
    MC_match_call,
    dots, default.formals, empty.formals, eval.formals, user.formals,
    parent.offset, sys.frames(), sys.calls()
  )

#' Kept here for easy reference
#'
#' @keywords internal

match_call_old <- function(dots="expand", default.formals=FALSE, empty.formals=FALSE,
  eval.formals=FALSE, user.formals=TRUE, parent.offset=0L, bypass.checks=FALSE) {
  if(!is_int(parent.offset)) stop("Argument `parent.offset` must be a 1 length integer vector.")

  if(!bypass.checks) {
    attempt <- try( {  # folding both of these into one try block to try to limit try overhead
      inner.frame <- parent.frame(1L + parent.offset)
      fun.frame <- parent.frame(2L + parent.offset)
      call <- sys.call(sys.parent(parent.offset + 1L))
      fun.obj <- get(as.character(call[[1L]]), envir=fun.frame)
    } )
    # All this validation costs 20 microseconds
    if(inherits(attempt, "try-error")) {
      stop(
        "Unable to retrieve function frame or function itself; make sure ",
        "`match_call` is invoked from within a closure"
    ) }
    if(identical(inner.frame, sys.frame(0L))) {
      stop(
        "Parent frame is R_GlobalEnv, so it seems you are not running this ",
        "function from a closure or you have the wrong `parent.offset`."
    ) }
    if(typeof(fun.obj) != "closure") {  # identical slow
      stop("Function may only be invoked within a closure.")
    }
    if(!is.character(dots) || length(dots) != 1L || is.na(match(dots, c("exclude", "include", "expand")))) {
      stop("Argument `dots` must be 'exclude', 'include', or 'expand'.")
    }
    if(!is.logical(default.formals) || length(default.formals) != 1L) {
      stop("Argument `default.formals` must be a one length logical vector.")
    }
    if(!is.logical(empty.formals) || length(empty.formals) != 1L) {
      stop("Argument `empty.formals` must be a one length logical vector.")
    }
    if(!is.logical(eval.formals) || length(eval.formals) != 1L) {
      stop("Argument `eval.formals` must be a one length logical vector.")
    }
  } else { # same as above, but outside try block
    inner.frame <- parent.frame(1L + parent.offset)
    fun.frame <- parent.frame(2L + parent.offset)
    call <- sys.call(sys.parent(parent.offset + 1L))
    fun.obj <- get(as.character(call[[1L]]), envir=fun.frame)
  }
  # - Argument Matching -------------------------------------------------------

  # Here we match arguments provided in call to formals, and make sure each is
  # tagged with it's full name

  # Get call and formals
  call.full.fun <- call[[1]]
  call.formals <- formals(fun.obj)
  call.formals.remain <- call.formals[names(call.formals) != "..."]

  # First pass match call.  In order for `match.call` to work, we need to
  # capture the correct dots, and then evaluate substitute the values underlying
  # the dots back into the call before using `match.call`.  Note we don't
  # use substitute directly on the call for fear other parts of the call outside
  # of dots would get substituted.  `match.call` just doesn't like it when
  # `...` are part of a call; it can do the matching fine so long as:
  # - you evaluate it in an environment where the first dots it will encounter
  #   in the lexical (?need to confirm) stack are the dots it is attempting to
  #   match again
  # - There are no expressions in the `...` (e.g. `1+1`, or even `(1)`)
  # If these conditions are not met then you have to resort to the workaround
  # below.  The nice thing about this workaround is that it should be robust
  # to changes in `match.call` treatment of dots in `calls`.

  dummy_fun <- as.function.default(c(call.formals, TRUE))

  # as.list.default to optimize, actually was substantial portion of execution

  if(any(dots.loc <- quote(...) == as.list.default(call[-1L]))) {
    # if there are dots in the call, replace them with the actual contents of the dots
    dots.loc.num <- which(dots.loc) + 1L
    dots.sub <- as.list.default(substitute(list(...), fun.frame))[-1L]
    call <- as.call(append(as.list.default(call[-dots.loc.num]), dots.sub, dots.loc.num - 1L))
  }
  call.final <- match.call(dummy_fun, call, expand.dots=FALSE)
  call.final <- as.list.default(call.final[-1L])  # remove fun name for now, will add it back later

  # Now deal with rest of formals, etc.

  call.formals.remain <- call.formals.remain[
    match(names(call.formals.remain), names(call.final), nomatch=0L) > 0L
  ]
  if(eval.formals) {   # user args evaled in calling scope, rest in function scope
    call.final <- lapply(call.final, eval, fun.frame)
    call.formals.remain <- lapply(call.formals.remain, eval, inner.frame)
  }
  if(default.formals && empty.formals){
    call.final <- c(call.final, call.formals.remain)
  } else if (default.formals && length(call.formals.remain) > 0L) {
    index.vec <- vapply(
      call.formals.remain,
      function(x) !(is.symbol(x) && nchar(as.character(x)) == 0L && length(x) == 1L),
      logical(1L)
    )
    call.final <- c(call.final, call.formals.remain[index.vec])
    call.formals.remain <- call.formals.remain[!index.vec]
  }
  # Order like in formals

  index.vec <- match(names(call.final), call.frm.nms <- names(call.formals))
  if(any(is.na(index.vec))) stop("Logic Error; any formals matched should match perfectly; seek assistance;")
  call.final <- call.final[call.frm.nms]
  if(dots == "expand" && !is.na(ellip.index <- match("...", names(call.final)))) {
    call.final <- append(call.final[names(call.final) != "..."], call.final[["..."]], ellip.index - 1L)
  }
  if(eval.formals) {
    c(call.full.fun, call.final)
  } else {
    as.call(c(call.full.fun, call.final))
  }
}

