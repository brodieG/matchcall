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
})
