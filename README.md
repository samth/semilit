# Semi-Literate programming for Racket

## Installation

```
% raco pkg install semilit
```

## Use

Simply use `semilit` before the language specification on the `#lang` line,
and the whole file is read by the `semilit` reader.  This reader ignores all
lines that don't begin with `>`, and drops the `>` before passing the lines
on to the underlying language.

```
#lang semilit racket

This is not part of the program
> (define x 1) ;; part of the program
```
