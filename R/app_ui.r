app_ui <- function() {

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
      )
    ),

    shiny::tags$nav(
      class = "main-menu",
      shiny::tags$ul(
        shiny::tags$li(shiny::actionLink("nav_home", label = list(shiny::tags$i(class = "fa fa-home fa-2x"), shiny::tags$span(class = "nav-text", "???????")))),
        shiny::tags$li(shiny::actionLink("nav_articles", label = list(shiny::tags$i(class = "fa fa-book fa-2x"), shiny::tags$span(class = "nav-text", "??????????")))),
        shiny::tags$li(shiny::actionLink("nav_topics", label = list(shiny::tags$i(class = "fa fa-tags fa-2x"), shiny::tags$span(class = "nav-text", "????")))),
        shiny::tags$li(shiny::actionLink("nav_chat", label = list(shiny::tags$i(class = "fa fa-comments fa-2x"), shiny::tags$span(class = "nav-text", "AI ???"))))
      )
    ),

    shiny::tags$div(
      style = "width: 75%; margin: 0 auto; padding-top: 20px;",
      shiny::uiOutput("current_page")
    )
  )
}
