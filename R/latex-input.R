#' Latex Input
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`;
#'
#' @return A latex input control that can be added to a UI definition.
#' @export
#'

latexInput = function(inputId, label, width = NULL) {
  js_var = paste0(inputId, "_jsvar")
  input_latex = paste0(inputId, "_latex")

  shiny::addResourcePath(
    prefix = 'wwwLatex2r',
    directoryPath = system.file('www', package='latex2r')
  )
  shiny::tagList(
    shiny::singleton(shiny::tags$head(
        shiny::tags$script(src = "wwwLatex2r/jquery.min.js"),
        shiny::tags$script(src = "wwwLatex2r/mathquill.min.js"),
        shiny::tags$link(
          rel = "stylesheet", type = "text/css",
          href = "wwwLatex2r/mathquill.css"
        ),
        shiny::tags$script("var MQ = MathQuill.getInterface(2);")
    )),

    shiny::div(
      class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
      shiny::tags$label(
        label, class = "control-label",
        class = if (is.null(label))"shiny-label-null", `for` = inputId
      ),
      shiny::tags$span(
        class = "mathquill-editable form-control",
        style = "height:auto;",
        id = inputId,
        shiny::tags$input(
          type = "text"
        )
      )
    ),

    shiny::tags$script(
      glue::glue("
        var <<js_var>> = MQ.MathField($('#<<inputId>>')[0], {
          handlers: {
            edit: update_latex
          }
        });
        function update_latex() {
          var code = <<js_var>>.latex();
          Shiny.onInputChange('<<input_latex>>', code)
        }
        update_latex();", .open = "<<", .close = ">>"
      )
    )
  )
}


# tags$script("
# $('.mathquill-editable').each(function() {
#   MQ.MathField(this);
# });")
