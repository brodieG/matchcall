matchcall
=========

## Overview

Similar to `match.call`, but is designed specifically to match calls from the
dynamic call stack.  Which call is matched is controlled by the `n` argument,
which is analgous to the `n` argument for `sys.parent`.  Also addresses some
potentially unexpected behavior with `match.call` with calls involving dots in
corner cases.

You can also use `match.call` to match arbitrary calls from the call stack,
but it is simpler to do so using `match_call`, particularly when the call stack
and the lexical stack are not the same. See examples for illustration of
differences between `match.call` and `match_call`.

`match_call` will also include unspecified defaults and missing arguments if
requested.

For a more detailed discussion of the exact nature of the dots corner case issues
with `match.call`, see the vignette.  Briefly: these crop up when there are
multiple calls using dots involved.

For more details, see:

* Vignette
* `?match_call`

## Installation Instructions

This package is still in early development and only available on github:

```
library(devtools)
install_github("brodieg/matchcall")
```
