library(matchcall)

unitizer_sect("Input Errors", {
  match_call(dots=1)
  match_call(dots="explode")
  match_call(dots=NA_character_)
  match_call(parent.offset=0)
  match_call(default.formals=c(TRUE, FALSE))
  match_call(empty.formals=NA)
})
