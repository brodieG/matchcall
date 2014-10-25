## Benchmarks

Just the bare call to the C function including the calls to `sys.frames` and `sys.calls` takes as long as `match.call`.  Here we've changed `match_call()` to return `R_NilValue` at the first function line:

```R
fun <- function(x, y, z=TRUE) match_call()
fun2 <- function(x, y, z=TRUE) match.call()

library(microbenchmark)
microbenchmark(fun(1, 2, 3), fun2(1, 2, 3))
```

Produces

```
Unit: microseconds
          expr   min    lq median    uq     max neval
  fun(1, 2, 3) 2.416 2.746 3.0710 3.474 137.333   100
 fun2(1, 2, 3) 2.204 2.475 2.6925 2.969  11.608   100
```

With "normal" match.call (as of commit b8e48dd), we get:

```
Unit: microseconds
          expr   min     lq median     uq    max neval
  fun(1, 2, 3) 4.447 4.9665 5.2555 5.7095 23.915   100
 fun2(1, 2, 3) 2.331 2.5860 2.8365 3.0685 42.980   100
```

There is probably room for improvement, but given that we `eval` `match.call` from within `match_call`, it's hard to imagine it will get that much better.
