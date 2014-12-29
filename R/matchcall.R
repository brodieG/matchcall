#' Match Closure Arguments To Formals
#'
#' Similar to \code{`\link{match.call}`}, but is designed specifically to match
#' calls from the dynamic call stack.  Which call is matched is controlled by the
#' \code{`n`} argument, which is analgous to the \code{`n`} argument
#' for \code{`\link{sys.parent}`}.  Also addresses some potentially unexpected
#' behavior with \code{`\link{match.call}`} with calls involving dots in corner
#' cases.
#'
#' You can also use \code{`match.call`} to match arbitrary calls from the
#' call stack, but it is simpler to do so using \code{`match_call`}, particularly
#' when the call stack and the lexical stack are not the same. See examples
#' for illustration of differences between \code{`\link{match.call}`} and
#' \code{`match_call`}.
#'
#' \code{`match_call`} will also include unspecified defaults and missing
#' arguments if requested (see \code{`default.formals`} and \code{`empty.formals`}).
#'
#' For a more detailed discussion of the exact nature of the dots corner case issues
#' with \code{`match.call`}, see the vignette.  Briefly: these crop up when
#' there are multiple calls using dots involved.
#'
#' @export
#' @param n integer(1L) how many frames to look up the call stack, analogous to
#'   the \code{`n`} parameter for \code{`\link{sys.parent}`}.
#' @param dots character(1L) "exclude": do not include dots, "include": include
#'   them, "expand": include and expand (note last two only include dots if
#'   there are actually args matched by dots)
#' @param default.formals set to TRUE to include formals not specified in call
#'   though under no circumstances will it return ellipses even if you do
#'   something like \code{function(a, ...=list(1, 2, 3))} which oddly R appears
#'   to tolerate in closure definitions.
#' @param empty.formals set to TRUE to include formals that were not specified
#'   and do not have default values
#' @param user.formals set to FALSE if you want to exclude arguments that the
#'   user specified; this should almost never be needed unless you specifically
#'   want to know what arguments are using default values (in which case you
#'   also be setting `default.formals` to TRUE)
#' @param definition same as \code{`definition`} in \code{`\link{match.call}`},
#'   though shouldn't be needed most of the time
#' @return the parent call that led to the invocation of match_call()
#' @useDynLib matchcall, .registration=TRUE, .fixes="MC_"
#' @examples
#' # Compare `match.call` and `match_call`
#' fun1 <- function(a, b) {
#'   cat("**Matching Parent Call**\n")
#'   print(match.call())
#'   print(match_call())
#'
#'   cat("\n**Matching Grand-Parent Call**\n")
#'   print(match.call(call=sys.call(sys.parent())))
#'   print(match_call(2))
#' }
#' fun2 <- function(c, d) fun1(a + 1, b - 1)
#' fun2(25, pi() + 3)
#'
#' # Recover default formals
#' fun <- function(a, b, c=TRUE, ...) {
#'   match_call(default.formals=TRUE, dots="include")
#' }
#' fun(5, 6, x=list(1:10, FALSE))
#'
#' # If Dynamic and Lexical Stacks not the same, we need to use `definition`
#' # param to `match.call`; `match_call` on the other hand works just fine
#'
#' fun3a <- function(x) fun4a()
#' fun4a <- function() match_call(2)
#'
#' fun3b <- function(x) fun4b()  # Note: fun4b defined outside fun3b
#' fun4b <- function() match.call(definition=fun3b, call=sys.call(sys.parent()))
#'
#' fun3a(1 + 1)       # `match_call` works
#' fun3b(1 + 1)       # `match.call` also works, but only if we explicitly specify `definition`

match_call <- function(
  n=1L, dots="expand", default.formals=FALSE, empty.formals=FALSE,
  user.formals=TRUE, definition=NULL
)
  .Call(
    MC_match_call,
    dots, default.formals, empty.formals, user.formals,
    n, definition, sys.frames(), sys.calls(), sys.parents()  # note slightly faster than `sys.frame` and `sys.call`, for some reason
  )
#' Returns match type for each argument
#'
#' @keywords internal

match_call_internal <- function(
  n=1L, dots="expand", default.formals=FALSE, empty.formals=FALSE,
  user.formals=TRUE, definition=NULL
)
  .Call(
    MC_match_call_internal,
    dots, default.formals, empty.formals, user.formals,
    n, definition, sys.frames(), sys.calls(), sys.parents()  # note slightly faster than `sys.frame` and `sys.call`, for some reason
  )
#' Help Test Fun
#'
#' @keywords internal
#' @export

mc_test1 <- function(x)
  .Call(MC_test1, x)
#' @export

mc_test2 <- function()
  .Call(MC_test2)
#' @export

mc_test3 <- function()
  .Call(MC_test3, sys.calls(), sys.frames(), sys.parents())

