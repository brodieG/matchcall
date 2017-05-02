matchcall 0.2.2
--------------------------------------------------------------------------------

This package is now deprecated in favor of the internal `match.call` since that
one added the `envir` argument in R 3.2.0 and that fixes most corner cases this
package was designed to address.

matchcall 0.2.1
--------------------------------------------------------------------------------

Internal changes with no user visible behavior changes (see git log if you're
really curious).

matchcall 0.2.0
--------------------------------------------------------------------------------

Initial feature "complete" release, passes R CMD check, etc.

Behavior Changes:

* `n=0` now allowed as an argument value for `match_call`

Features:

* `empty.formals` now working
* `user.formals` now working
* removed `eval.formals` option since no real use case
* `MC_match_call` underlying C function exported with `R_RegisterCCallable` for
  use in other packages

matchcall 0.1.2
--------------------------------------------------------------------------------

* `default.args` option is now working
* Added valgrind tests to catch protection issues

matchcall 0.1.1
--------------------------------------------------------------------------------

* Fixed bug that treated all frames / calls in `sys.frames()` / `sys.calls()` as
  parents, when in reality we need to look at `sys.parents()` (Issue #1)
