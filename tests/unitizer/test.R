library(matchcall)

  fun1 <- function(x, y, z=TRUE, w=letters[1:3])
    match_call(user.formals=FALSE, default.formals=TRUE)

  fun1()
  fun1(z=3)
  fun1(z=3, w=5)
  fun1(1, 2)
