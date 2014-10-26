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
