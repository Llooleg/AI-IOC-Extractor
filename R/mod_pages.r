page_home <- function(v) {
  shiny::tagList(
    shiny::h1("?????????? ???????"),
    shiny::p(sprintf("????????? ??????????: %s", format(v$last_updated, "%d.%m.%Y %H:%M:%S"))),
    shiny::actionButton("refresh_db", " ????????", icon = shiny::icon("sync")),
    shiny::hr(),
    shiny::h3("????? ?????????? ? ??????????? ??????????")
  )
}

page_articles <- function(v) {
  shiny::tagList(
    shiny::h2("??????????"),
    shiny::dateRangeInput("date_range", "????????? ??????:",
      start = min(v$articles$date, na.rm = TRUE),
      end = max(v$articles$date, na.rm = TRUE),
      language = "ru"
    ),
    DT::DTOutput("articles_table")
  )
}

page_topics <- function(all_topics) {
  shiny::tagList(
    shiny::h2("????"),
    DT::DTOutput("topics_table")
  )
}

page_article_detail <- function(v, selected_id) {
  article <- v$articles[v$articles$id == selected_id(), ]

  shiny::tagList(
    shiny::actionButton("back_to_list", " ?????", icon = shiny::icon("arrow-left")),
    shiny::hr(),
    shiny::h1(article$title),
    shiny::p(shiny::strong("???: "), article$tag),
    shiny::p(shiny::strong("??????: "), paste(unlist(article$authors), collapse = ", ")),
    shiny::p(shiny::strong("????: "), article$date),
    shiny::div(
      style = "background: rgba(255,255,255,0.1); padding: 20px;",
      shiny::h4("?????????"),
      shiny::p(article$abstract)
    )
  )
}

page_chat <- function(chat_data) {
  shiny::tagList(
    shiny::h2("AI ???"),
    shiny::div(
      class = "full-page-chat",
      shiny::div(
        id = "chat-scroll-area",
        class = "chat-body-large",
        lapply(chat_data$history, function(m) {
          shiny::div(class = paste0("chat-msg msg-", m$role), m$text)
        })
      ),
      shiny::div(
        class = "chat-input-area",
        shiny::textInput("user_input", NULL, placeholder = "??????? ??? ??????..."),
        shiny::actionButton("send_msg", "?????????", icon = shiny::icon("paper-plane"))
      )
    )
  )
}
