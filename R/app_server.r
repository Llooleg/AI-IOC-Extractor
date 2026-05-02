app_server <- function(input, output, session) {

  fetch_data <- function() {
    db <- mongolite::mongo(
      collection = "metadata",
      db = "cybersecurity_articles",
      url = sprintf(
        "mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin",
        Sys.getenv("MONGO_USER"),
        Sys.getenv("MONGO_PASS"),
        Sys.getenv("MONGO_HOST")
      )
    )
    db$find("{}")
  }

  v <- shiny::reactiveValues(
    articles = fetch_data(),
    last_updated = Sys.time()
  )

  current_view <- shiny::reactiveVal("home")
  selected_id <- shiny::reactiveVal(NULL)
  topic_filter <- shiny::reactiveVal("")

  # Precompute topics every refresh
  all_topics <- shiny::reactive({
    v$articles |>
      dplyr::select(id, categories) |>
      tidyr::unnest(categories) |>
      dplyr::group_by(categories) |>
      dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(count))
  })

  # Chat history
  chat_data <- shiny::reactiveValues(
    history = list(list(role = "ai", text = "??????! ? ????? ???????? ?????? ?? ?????????????????."))
  )

  # Navigation
  shiny::observeEvent(input$nav_chat, current_view("chat"))
  shiny::observeEvent(input$nav_home, current_view("home"))
  shiny::observeEvent(input$nav_topics, current_view("topics"))
  shiny::observeEvent(input$nav_articles, {
    topic_filter("")
    current_view("articles")
  })
  shiny::observeEvent(input$back_to_list, current_view("articles"))

  # Refresh DB
  shiny::observeEvent(input$refresh_db, {
    shiny::withProgress(message = "??????????? ? ???? ??????...", value = 0, {
      v$articles <- fetch_data()
      v$last_updated <- Sys.time()
      shiny::setProgress(1)
    })
    shiny::showNotification(
      sprintf("???? ????????? ? %s", format(v$last_updated, "%H:%M:%S")),
      type = "message"
    )
  })

  # Topic selection
  shiny::observeEvent(input$topics_table_rows_selected, {
    idx <- input$topics_table_rows_selected
    topic_filter(all_topics()$categories[idx])
    current_view("articles")
  })

  # Article selection
  shiny::observeEvent(input$articles_table_rows_selected, {
    dat <- filtered_data()
    selected_id(dat$id[input$articles_table_rows_selected])
    current_view("article_detail")
  })

  # Chat
  shiny::observeEvent(input$send_msg, {
    user_text <- input$user_input
    req(user_text)

    chat_data$history[[length(chat_data$history) + 1]] <- list(role = "user", text = user_text)
    shiny::updateTextInput(session, "user_input", value = "")

    ai_resp <- tryCatch({
      resp <- httr2::request("http://ai-container:8000/v1/chat") |>
        httr2::req_body_json(list(message = user_text)) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      resp$choices[[1]]$message$content
    }, error = function(e) "?????? ??? ????????? ??????.")

    chat_data$history[[length(chat_data$history) + 1]] <- list(role = "ai", text = ai_resp)
  })

  # Filtering
  filtered_data <- shiny::reactive({
    df <- v$articles
    if (!is.null(input$date_range)) {
      df <- df |>
        dplyr::filter(
          !is.na(date),
          as.Date(date) >= input$date_range[1],
          as.Date(date) <= input$date_range[2]
        )
    }
    df
  })

  # UI switcher
  output$current_page <- shiny::renderUI({
    switch(
      current_view(),
      "home" = page_home(v),
      "articles" = page_articles(v),
      "topics" = page_topics(all_topics),
      "article_detail" = page_article_detail(v, selected_id),
      "chat" = page_chat(chat_data)
    )
  })

  # Tables
  output$articles_table <- DT::renderDT({
    df <- filtered_data()
    if (!"tag" %in% colnames(df)) df$tag <- NA

    df <- df |>
      dplyr::mutate(
        authors = vapply(authors, function(x) paste(unlist(x), collapse = ", "), FUN.VALUE = ""),
        tag = vapply(tag, function(x) paste(unlist(x), collapse = ", "), FUN.VALUE = "")
      ) |>
      dplyr::select(
        "????????" = title,
        "??????" = authors,
        "????" = date,
        "?????????" = categories,
        "???" = tag
      )

    DT::datatable(
      df,
      selection = "single",
      filter = "top",
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        search = list(search = topic_filter())
      )
    )
  })

  output$topics_table <- DT::renderDT({
    DT::datatable(all_topics(), selection = "single", options = list(pageLength = 20))
  })
}
