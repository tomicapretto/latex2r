# latex2r 0.2.0

## Maintenance and fixes

* Fixed a bug related to implicit multiplication failing when using CARET (#2)
* Added tests for implicit multiplication

## Internal

* The `Token` R6 class now has a `print.Token` S3 method. Useful for debugging.

# latex2r 0.1.3

## New features

* Added a `NEWS.md` file to track changes to the package.
* Implicit multiplication. Now the parser interprets things such as "xy" as "x * y". 
Explicit multiplication still works.

## Maintenance and fixes

* Only variable names can have subscripts. 
* `latex2fun()` utils were updated so now, for example, any letter can be a function argument. 
In general the process of transforming a latex expression to a function is more robust.
* `pi` is not detected as function argument anymore.
* `latex2fun()` raises an error if there is an assignment within the expression.
* Example translations in README

## Deprecation

* Deleted `latexInput()` and `launch_app()`. Thus, the package does not need either 
shiny or mathquill anymore.
