matchcall
=========

**Pre-Release**

Lots of outstanding issues.  Only `n` and `dots` arguments do anything right now.

## Overview

Similar to `match.call`, but is designed specifically to match calls from the
dynamic call stack.  Which call is matched is controlled by the `n` argument,
which is analgous to the `n` argument for `sys.parent`.

Also addresses some odd `match.call` behavior in corner cases.  See:

* Vignette
* `?match_call`

## Installation Instructions

This package is still in early development and only available on github:

```
library(devtools)
install_github("brodieg/matchcall")
```
