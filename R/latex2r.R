#' Translate LaTeX formula to R code.
#'
#' Take a LaTeX formula and attempts to find an equivalent representation in R.
#' If `interactive=TRUE` it pops up an interactive REPL that does the same, but interactively.
#'
#' @param x string
#' @param interactive boolean
#'
#' @return string
#' @export
#'
#' @examples
#' latex2r("\\beta_1^{\\frac{x+1}{x^2 \\cdot y}}")
latex2r = function(x, interactive = FALSE) {
  if (interactive) {
    Reina$new()$run_prompt()
  } else {
    Reina$new()$run_line(x)
  }
}
