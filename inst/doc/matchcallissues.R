## ------------------------------------------------------------------------
fun0 <- function(...) {
  fun_gpar <- function(b, ...) {
    fun_par <- function(a)
      match.call(fun_gpar, sys.call(sys.parent()), expand.dots=FALSE)
    fun_par()
  }
  fun_gpar(...)
}
fun0(999, 2 + 2, 3 * pi(), 4)

## ------------------------------------------------------------------------
fun2 <- function(a, ...) {
  fun_gpar <- function(b, ...) {
    fun_par <- function(...) match.call(fun_gpar, sys.call(sys.parent()), expand.dots=F)
    fun_par()
  }
  fun_gpar(a, ...)
}
fun2(3, "test", x=45, zest="lemon")

## ------------------------------------------------------------------------
fun3 <- function(a, ...) {
  fun_gpar <- function(b, c, d, ...) {
    fun_par <- function() match.call(fun_gpar, sys.call(sys.parent()), expand.dots=F)
    fun_par()
  }
  fun_gpar(a, ...)
}
fun3(3, "test", 59, x=45, zest="lemon", 58)

## ------------------------------------------------------------------------
fun3(3, "test", 59, x=45, zest="lemon", (58), (60))

