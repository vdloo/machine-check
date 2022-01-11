# Examples

A directory with examples of how to structure your checks-to-perform directory.

First configure your checks in the `checks-to-perform` directory.

Each file must be named the same as the `perform-*-checks` function it provides.

So a file named `core.rkt` should `(provide perform-core-checks` and `(define perform-core-checks ...)` etc.

First copy a test example file to `checks-to-perform`:
```
$ cp examples/simple/main.rkt.example checks-to-perform/main.rkt
```

Then run:
```
$ make run  # or ./main.rkt or machine-check
--------------------
FAILURE
name:       check-false
location:   machine-check/check-helpers.rkt:83:5
params:     '(#t)
message:    "Package 'some-package' was not found installed"
```
