# latex2r 0.1.3

## New features

* Added a `NEWS.md` file to track changes to the package.
* Implicit multiplication. Now the parser interprets things such as "xy" as "x * y". 
Explicit multiplication still works.

## Maintenance and fixes

* Only variable names can have subscripts. 

## Deprecation

* Deleted `latexInput()` and `launch_app()`. Thus, the package does not need either 
shiny or mathquill anymore.
