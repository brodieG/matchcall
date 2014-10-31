library(matchcall)

unitizer_sect("Input Errors", {
  match_call(dots=1)
  match_call(dots="explode")
  match_call(dots=NA_character_)
  match_call(0)
  match_call(default.formals=c(TRUE, FALSE))
  match_call(empty.formals=NA)
})
unitizer_sect("Simple Tests", {
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
})
unitizer_sect("Examples that break match.call", {
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
})
unitizer_sect(
  "Doc Example",
  compare=unitizerItemTestsFuns(output=identical),  # Need to check output here
{
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

  fun <- function(a, b, c=TRUE, ...)
    match_call(default.formals=TRUE, dots="include")
  fun(5, 6, x=list(1:10, FALSE))
})
unitizer_sect(
  "Default Formals", {

  fun1 <- function(x, y, z=TRUE, w=letters[1:3]) match_call(default.formals=TRUE)

  fun1(1, 2, 3)
  fun1(z=3, 1, 2)
  fun1(q=5, 6, "hello")
  fun1(1, 2, 3, 4)
  fun1(z=1, w=2, 3, 4)

  fun2 <- function(x, ..., y, z=TRUE, w=letters[1:3]) match_call(default.formals=TRUE, dots="include")

  fun2(1, 2, 3)
  fun2(z=3, 1, 2)
  fun2(q=5, 6, "hello")
  fun2(1, 2, 3, 4)
  fun2(z=1, w=2, 3, 4)

})
unitizer_sect(
  "Empty Formals", {

  fun1 <- function(x, y, z=TRUE, w=letters[1:3]) match_call(empty.formals=TRUE)

  fun1()

  fun2 <- function(x, y, ..., z, w=letters[1:3])
    match_call(default.formals=TRUE, dots="include", empty.formals=TRUE)

  fun2()
  fun2(z=3)
  fun2(y=5)
  fun2(1, 2, 3, 4)

  fun3 <- function(x, y, ..., z, w=letters[1:3])
    match_call(empty.formals=TRUE, dots="exclude")

  fun3()

  fun4 <- function(x, y, ..., z, w=letters[1:3])
    match_call(empty.formals=TRUE, dots="expand")

  fun4()   # Expand dots even if empty, should disappear

})
unitizer_sect(
  "Exclude User Formals", {

  fun1 <- function(x, y, z=TRUE, w=letters[1:3])
    match_call(user.formals=FALSE, default.formals=TRUE)

  fun1()
  fun1(z=3)
  fun1(z=3, w=5)
  fun1(1, 2)

  fun2 <- function(x, y, z=TRUE, w=letters[1:3])
    match_call(user.formals=FALSE, default.formals=TRUE, empty.formals=TRUE)

  fun2()
  fun2(z=3)
  fun2(z=3, w=5)
  fun2(1, 2)

  fun3 <- function(x, y, ..., z=TRUE, w=letters[1:3])
    match_call(user.formals=FALSE, default.formals=TRUE, empty.formals=TRUE)

  fun3()
  fun3(z=3)
  fun3(z=3, w=5)
  fun3(z=3, w=5, q="hello")
  fun3(1, 2)

  fun4 <- function(x, y, ..., z=TRUE, w=letters[1:3])
    match_call(user.formals=FALSE, default.formals=TRUE, empty.formals=TRUE, dots="include")

  fun4()
  fun3(z=3, w=5, q="hello")  # should drop dots

})
