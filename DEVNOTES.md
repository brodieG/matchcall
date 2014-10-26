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

With "normal" match_call (as of commit 71355fb), we get:

```
Unit: microseconds
          expr   min     lq median     uq     max neval
  fun(1, 2, 3) 4.521 5.2715 5.6135 6.0715 131.933   100
 fun2(1, 2, 3) 2.315 2.6755 3.0280 3.3140  21.376   100
```

There is probably room for improvement, but given that we `eval` `match.call` from within `match_call`, it's hard to imagine it will get that much better.  The actual `match.call` call inside `match_call` looks to take about 1.2us:

```
# Exiting right before `match.call`
Unit: microseconds
         expr   min    lq median     uq     max neval
 fun(1, 2, 3) 3.004 3.292  3.513 3.8445 171.104  2000
# Exiting right after
Unit: microseconds
         expr   min     lq median    uq    max neval
 fun(1, 2, 3) 4.243 4.7125  4.994 5.412 41.397  2000
```

So maybe we can save half a microsecond someplace.
