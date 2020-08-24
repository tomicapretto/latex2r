#' Run an interactive LaTeX to R translator which uses [latexInput()]
#'
#' @export
launch_app = function() {
  shiny::runApp(system.file("shiny", package = "latex2r"))
}
