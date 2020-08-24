library(shiny)
library(latex2r)

ui = fluidPage(
  br(),
  textInput("text", "Usual textInput"),
  latexInput('id1', "Math input"),
  actionButton("translate", "Translate"),
  br(), br(),
  verbatimTextOutput("out_txt1")
)
server = function(input, output) {
  observeEvent(input$translate, {
    txt = input$id1_latex
    output$out_txt1 = renderText({
      paste0(
        "Original: ", txt, "\n",
        "Translated: ", latex2r(txt), "\n"
      )
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
