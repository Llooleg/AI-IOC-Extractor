#' Launch the Cybersecurity Shiny App
#'
#' @return Runs the Shiny application.
#' @export
run_app <- function() {

  ui <- app_ui()
  server <- app_server

  shiny::shinyApp(ui = ui, server = server)
}
