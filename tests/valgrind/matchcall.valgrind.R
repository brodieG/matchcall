library(matchcall)

  gctorture(TRUE)

  # try(match_call(dots=1))
  # try(match_call(dots="explode"))
  # try(match_call(dots=NA_character_))
  # try(match_call(0))
  # try(match_call(default.formals=c(TRUE, FALSE)))
  # try(match_call(empty.formals=NA))

  fun <- function(x, y, z=TRUE) match_call()

  fun(1, 2, 3)
  fun(z=3, 1, 2)
  fun(y=5, 6)

  fun2 <- function(a, b, ...) match_call()

  fun2(1, 2, 3, 4)
  fun2(1, 2, x=3, w=4)
  fun2(x=3, 1, 2, list(4))

  fun3 <- function(a, b, ...) match_call(dots="include")

  fun3(1, 2, 3, 4)
  fun3(1, 2, x=3, w=4)
  fun3(x=3, 1, 2, list(4))

  fun4 <- function(a, b, ...) match_call(dots="exclude")

  fun4(1, 2, 3, 4)
  fun4(1, 2, x=3, w=4)
  fun4(x=3, 1, 2, list(4))
  fun0 <- function(...) {
    fun_gpar <- function(b, ...) {
      fun_par <- function(a)
        match_call(2, dots="include")
      fun_par()
    }
    fun_gpar(...)
  }
  fun0(999, 2 + 2, q=3 * pi(), 4)

  fun1 <- function(a, ...) fun_gpar(a, ...)
  fun_gpar <- function(b, ...) fun_par()
  fun_par <- function() match_call(2, dots="include")

  fun1(3, "test", x=45, zest="lemon")

  fun2 <- function(a, ...) {
    fun_gpar <- function(b, ...) (function(...) match_call(2, dots="include"))()
    fun_gpar(a, ...)
  }
  fun2(3, "test", x=45, zest="lemon")

  fun3 <- function(a, ...) {
    fun_gpar <- function(b, c, d, ...) (function() match_call(2, dots="include"))()
    fun_gpar(a, ...)
  }
  fun3(3, "test", 59, x=45, zest="lemon", 58)
  fun3(3, "test", 59, x=45, zest="lemon", (58), (60))

  fun1 <- function(a, b) {
    cat("**Matching Parent Call**\n")
    print(match.call())
    print(match_call())

    cat("\n**Matching Grand-Parent Call**\n")
    print(match.call(fun2, sys.call(sys.parent())))
    print(match_call(2))
  }
  fun2 <- function(c, d) fun1(a + 1, b - 1)
  fun2(25, pi() + 3)

  fun1 <- function(x, y, z=TRUE, w=letters[1:3]) match_call(default.formals=TRUE)

  fun1(1, 2, 3)
  fun1(z=3, 1, 2)
  try(fun1(q=5, 6, "hello"))
  fun1(1, 2, 3, 4)
  fun1(z=1, w=2, 3, 4)

  fun2 <- function(x, ..., y, z=TRUE, w=letters[1:3]) match_call(default.formals=TRUE, dots="include")

  fun2(1, 2, 3)
  fun2(z=3, 1, 2)
  fun2(q=5, 6, "hello")
  fun2(1, 2, 3, 4)
  fun2(z=1, w=2, 3, 4)
  gctorture(FALSE)
