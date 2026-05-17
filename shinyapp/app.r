library(shiny)
library(bslib)
library(mongolite)
library(DT)
library(dplyr)
library(tidyr)
library(httr2)
library(future)
library(promises)

plan(multisession)

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
  on.exit(db$disconnect())

  data <- db$find("{}")
  if (nrow(data) > 0) {
    data <- data %>% mutate(date = as.Date(date))
  }
  return(data)
}

# Консультация по конкретной статье через MCP сервер
# Передаёт контекст статьи и историю чата на /article-chat
ask_article_ai <- function(article_id, article_title, article_abstract, messages) {
  mcp_url <- Sys.getenv("MCP_SERVER_URL", unset = "http://127.0.0.1:8000")

  resp <- tryCatch(
    {
      httr2::request(mcp_url) |>
        httr2::req_url_path_append("article-chat") |>
        httr2::req_body_json(list(
          article_id       = article_id,
          article_title    = article_title,
          article_abstract = article_abstract,
          messages         = messages
        ), auto_unbox = TRUE) |>
        httr2::req_timeout(120) |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector = FALSE)
    },
    error = function(e) list(error = e$message)
  )

  if (!is.null(resp$error)) {
    return(paste("Ошибка соединения с AI-сервером:", resp$error))
  }
  as.character(resp$answer %||% "Пустой ответ от AI-сервера")
}

# Вспомогательный оператор null-coalescing
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

main_ui <- page_navbar(
  title = "AI IOC Extractor",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  bg = "#d4d4d4",
  inverse = FALSE,
  id = "main_nav",
  nav_spacer(),
  nav_panel("Главная",
    value = "home", icon = icon("home"),
    layout_columns(
      col_widths = 12,
      card(
        card_header("Управление данными", class = "bg-primary text-white"),
        card_body(
          h3("Добро пожаловать в электронную библиотеку"),
          p(uiOutput("last_updated_text")),
          actionButton("refresh_db", " Обновить базу данных", icon = icon("sync"), class = "btn-primary w-25")
        )
      )
    )
  ),
  nav_panel("Библиотека",
    value = "articles", icon = icon("book"),
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center bg-primary text-white",
          "Список статей",
          div(
            class = "d-flex gap-2 align-items-center",
            uiOutput("active_filter_ui"),
            uiOutput("date_filter_ui")
          )
        ),
        card_body(
          DTOutput("articles_table")
        )
      )
    )
  ),
  nav_panel("Разделы",
    value = "sections", icon = icon("tags"),
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white d-flex justify-content-between align-items-center",
          "Категории и Теги",
          actionButton("apply_section_filters", "Применить выбранные фильтры", icon = icon("check"), class = "btn-light btn-sm text-primary fw-bold", style = "background-color: white;")
        ),
        card_body(
          DTOutput("sections_table")
        )
      )
    )
  ),
  nav_panel("AI",
    value = "chat", icon = icon("comments"),
    layout_columns(
      col_widths = 12,
      card(
        height = "80vh",
        card_header("AI Чат", class = "bg-primary text-white"),
        card_body(
          class = "overflow-auto",
          uiOutput("chat_history")
        ),
        card_footer(
          fluidRow(
            column(10, textInput("user_input", label = NULL, placeholder = "Введите ваш вопрос...", width = "100%")),
            column(2, actionButton("send_msg", "Отправить", icon = icon("paper-plane"), class = "btn-primary w-100", style = "margin-top: 0;"))
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Детали статьи", value = "article_detail", icon = icon("file-alt"),
    uiOutput("article_detail_ui")
  )
)

ui <- main_ui

server <- function(input, output, session) {
  nav_hide("main_nav", "article_detail")
  # --- Состояние и Реактивность ---
  v <- reactiveValues(
    articles = data.frame(),
    last_updated = Sys.time()
  )

  observe({
    withProgress(message = "Загрузка базы данных...", value = 0.5, {
      v$articles <- tryCatch(
        {
          fetch_data()
        },
        error = function(e) {
          showNotification("Ошибка подключения к БД или БД пуста.", type = "error")
          data.frame(id = character(), title = character(), date = as.Date(character()), categories = I(list()), authors = I(list()), abstract = character(), tag = I(list()))
        }
      )
      setProgress(1, detail = "Готово!")
    })
  }) %>% bindEvent(TRUE, once = TRUE)

  sanitize_list_col <- function(col) {
    lapply(col, function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) {
        return(character(0))
      }
      as.character(unlist(x))
    })
  }

  all_sections <- reactive({
    req(nrow(v$articles) > 0)

    cat_df <- data.frame(id = character(), item = character(), type = character())
    tag_df <- data.frame(id = character(), item = character(), type = character())
    ctx_df <- data.frame(id = character(), item = character(), type = character())

    if ("categories" %in% names(v$articles)) {
      cat_df <- v$articles %>%
        select(id, item = categories) %>%
        mutate(item = sanitize_list_col(item)) %>%
        unnest(item, keep_empty = FALSE) %>%
        mutate(type = "Категория")
    }
    if ("tags" %in% names(v$articles)) {
      tag_df <- v$articles %>%
        select(id, item = tags) %>%
        mutate(item = sanitize_list_col(item)) %>%
        unnest(item, keep_empty = FALSE) %>%
        mutate(type = "Тег")
    }
    if ("context_tags" %in% names(v$articles)) {
      ctx_df <- v$articles %>%
        select(id, item = context_tags) %>%
        mutate(item = sanitize_list_col(item)) %>%
        unnest(item, keep_empty = FALSE) %>%
        mutate(type = "Контекст")
    }

    bind_rows(cat_df, tag_df, ctx_df) %>%
      filter(!is.na(item), item != "") %>%
      group_by(item, type) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      select("Название" = item, "Тип" = type, "Количество" = count)
  })

  # --- UI Элементы Главной ---
  output$last_updated_text <- renderUI({
    tags$b(paste("Последнее обновление:", format(v$last_updated, "%d.%m.%Y %H:%M:%S")))
  })

  observeEvent(input$refresh_db, {
    withProgress(message = "Подключение к базе данных...", value = 0.5, {
      v$articles <- tryCatch(
        {
          fetch_data()
        },
        error = function(e) {
          showNotification("Ошибка при обновлении БД", type = "error")
          v$articles
        }
      )
      v$last_updated <- Sys.time()
      setProgress(1, detail = "Данные обновлены")
    })
    showNotification(paste("База данных успешно обновлена в", format(v$last_updated, "%H:%M:%S")), type = "message")
  })

  # --- Фильтрация и Таблицы ---
  output$date_filter_ui <- renderUI({
    req(nrow(v$articles) > 0)
    available_dates <- as.Date(v$articles$date[!is.na(v$articles$date)])
    start_date <- if (length(available_dates) > 0) min(available_dates) else Sys.Date() - 30
    end_date <- if (length(available_dates) > 0) max(available_dates) else Sys.Date()

    dateRangeInput("date_range", NULL,
      start = start_date, end = end_date,
      language = "ru", separator = " : ", width = "250px"
    )
  })

  active_section_filters <- reactiveVal(data.frame(type = character(), value = character(), stringsAsFactors = FALSE))

  output$active_filter_ui <- renderUI({
    filts <- active_section_filters()
    if (nrow(filts) > 0) {
      badges <- lapply(1:nrow(filts), function(i) {
        span(class = "badge bg-info me-1 fs-6 mb-1", paste(filts$type[i], ":", filts$value[i]))
      })
      div(
        class = "d-flex align-items-center me-3 border border-info rounded p-1 flex-wrap",
        tagList(badges),
        actionButton("clear_filter", "Сбросить", class = "btn-sm btn-danger ms-2 py-0")
      )
    }
  })

  observeEvent(input$clear_filter, {
    active_section_filters(data.frame(type = character(), value = character(), stringsAsFactors = FALSE))
    proxy <- dataTableProxy("sections_table")
    selectRows(proxy, NULL)
  })

  filtered_data <- reactive({
    data <- v$articles
    req(nrow(data) > 0)

    if (!is.null(input$date_range)) {
      data <- data %>%
        filter(!is.na(date), as.Date(date) >= input$date_range[1], as.Date(date) <= input$date_range[2])
    }

    filts <- active_section_filters()
    if (nrow(filts) > 0) {
      match_scores <- sapply(1:nrow(data), function(i) {
        score <- 0
        cat_list <- if ("categories" %in% names(data) && !is.null(data$categories[[i]])) unlist(data$categories[[i]]) else character(0)
        tag_list <- if ("tags" %in% names(data) && !is.null(data$tags[[i]])) unlist(data$tags[[i]]) else character(0)
        ctx_list <- if ("context_tags" %in% names(data) && !is.null(data$context_tags[[i]])) unlist(data$context_tags[[i]]) else character(0)

        for (j in 1:nrow(filts)) {
          if (filts$type[j] == "Категория" && (filts$value[j] %in% cat_list)) score <- score + 1
          if (filts$type[j] == "Тег" && (filts$value[j] %in% tag_list)) score <- score + 1
          if (filts$type[j] == "Контекст" && (filts$value[j] %in% ctx_list)) score <- score + 1
        }
        score
      })

      data$match_score <- match_scores
      data <- data %>%
        filter(match_score > 0) %>%
        arrange(desc(match_score), desc(date))
    } else {
      data <- data %>% arrange(desc(date))
    }

    data
  })

  output$articles_table <- renderDT({
    data_to_display <- filtered_data()
    req(nrow(data_to_display) > 0)

    if (!"authors" %in% names(data_to_display)) data_to_display$authors <- NA

    display_df <- data_to_display %>%
      mutate(
        authors = sapply(authors, function(x) {
          if (is.null(x) || all(is.na(x))) {
            return("")
          }
          paste(unlist(x), collapse = ", ")
        }),
        sections = sapply(1:n(), function(i) {
          cat_html <- ""
          if ("categories" %in% names(data_to_display) && !is.null(data_to_display$categories[[i]])) {
            cat_list <- unlist(data_to_display$categories[[i]])
            if (length(cat_list) > 0 && !all(is.na(cat_list))) {
              cat_html <- paste0('<span class="badge bg-success me-1">', cat_list, "</span>", collapse = "")
            }
          }

          tag_html <- ""
          if ("tags" %in% names(data_to_display) && !is.null(data_to_display$tags[[i]])) {
            tag_list <- unlist(data_to_display$tags[[i]])
            if (length(tag_list) > 0 && !all(is.na(tag_list))) {
              tag_html <- paste0('<span class="badge bg-primary me-1">', tag_list, "</span>", collapse = "")
            }
          }

          ctx_html <- ""
          if ("context_tags" %in% names(data_to_display) && !is.null(data_to_display$context_tags[[i]])) {
            ctx_list <- unlist(data_to_display$context_tags[[i]])
            if (length(ctx_list) > 0 && !all(is.na(ctx_list))) {
              ctx_html <- paste0('<span class="badge bg-danger me-1">', ctx_list, "</span>", collapse = "")
            }
          }

          paste0(cat_html, tag_html, ctx_html)
        })
      )

    if ("match_score" %in% names(display_df)) {
      display_df <- display_df %>% select("Название" = title, "Совпадений" = match_score, "Авторы" = authors, "Дата" = date, "Разделы" = sections)
    } else {
      display_df <- display_df %>% select("Название" = title, "Авторы" = authors, "Дата" = date, "Разделы" = sections)
    }

    datatable(display_df,
      escape = FALSE,
      selection = "single",
      filter = "top",
      width = "100%",
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = FALSE,
        language = list(
          search = "Поиск:",
          info = "Показано с _START_ по _END_ из _TOTAL_ записей",
          paginate = list(previous = "Назад", `next` = "Вперед")
        )
      )
    )
  })

  output$sections_table <- renderDT({
    req(all_sections())
    datatable(all_sections(), selection = "multiple", options = list(scrollX = TRUE, pageLength = 15, language = list(search = "Поиск:")))
  })

  observeEvent(input$apply_section_filters, {
    req(input$sections_table_rows_selected)
    sections_df <- all_sections()
    selected_rows <- sections_df[input$sections_table_rows_selected, ]

    if (nrow(selected_rows) > 0) {
      active_section_filters(data.frame(type = selected_rows[["Тип"]], value = selected_rows[["Название"]], stringsAsFactors = FALSE))
      nav_select("main_nav", "articles")
    }
  })

  # --- Навигация и Детали статьи ---
  selected_id <- reactiveVal(NULL)

  observeEvent(input$articles_table_rows_selected, {
    req(input$articles_table_rows_selected)
    data <- filtered_data()
    selected_id(data$id[input$articles_table_rows_selected])
    article_chat$is_active <- FALSE
    article_chat$history <- list()

    nav_show("main_nav", "article_detail")
    nav_select("main_nav", "article_detail")
  })

  observeEvent(input$back_to_list, {
    nav_hide("main_nav", "article_detail")
    nav_select("main_nav", "articles")
  })

  article_chat <- reactiveValues(
    is_active = FALSE,
    is_loading = FALSE,
    history = list()
  )

  output$article_detail_ui <- renderUI({
    current_id <- selected_id()
    req(current_id)

    idx <- which(as.character(v$articles$id) == as.character(current_id))
    if (length(idx) == 0) {
      return(tagList(
        h3("Статья не найдена"),
        actionButton("back_to_list", "Назад", icon = icon("arrow-left"), class = "btn-secondary")
      ))
    }

    article <- v$articles[idx[1], ]

    categories_badges <- list()
    if ("categories" %in% names(article) && !is.null(article$categories[[1]]) && !all(is.na(article$categories[[1]]))) {
      categories_badges <- lapply(unlist(article$categories[[1]]), function(cat) {
        span(class = "badge bg-success me-1", cat)
      })
    }

    tags_badges <- list()
    if ("tags" %in% names(article) && !is.null(article$tags[[1]]) && !all(is.na(article$tags[[1]]))) {
      tags_badges <- lapply(unlist(article$tags[[1]]), function(t) {
        span(class = "badge bg-primary me-1", t)
      })
    }

    context_tags_badges <- list()
    if ("context_tags" %in% names(article) && !is.null(article$context_tags[[1]]) && !all(is.na(article$context_tags[[1]]))) {
      context_tags_badges <- lapply(unlist(article$context_tags[[1]]), function(ctx) {
        span(class = "badge bg-danger me-1", ctx)
      })
    }

    sections_display <- tagList(categories_badges, tags_badges, context_tags_badges)
    if (length(categories_badges) == 0 && length(tags_badges) == 0 && length(context_tags_badges) == 0) {
      sections_display <- span(class = "text-muted", "Нет разделов")
    }

    authors_display <- "Не указаны"
    if ("authors" %in% names(article) && !is.null(article$authors[[1]]) && !all(is.na(article$authors[[1]]))) {
      authors_display <- paste(unlist(article$authors[[1]]), collapse = ", ")
    }

    # UI для чата по статье
    article_chat_ui <- if (article_chat$is_active) {
      messages <- lapply(article_chat$history, function(m) {
        if (m$role == "ai") {
          div(class = "d-flex justify-content-start mb-2", div(class = "p-2 bg-secondary text-white rounded-3", style = "max-width: 85%;", m$text))
        } else {
          div(class = "d-flex justify-content-end mb-2", div(class = "p-2 bg-info text-white rounded-3", style = "max-width: 85%;", m$text))
        }
      })

      loading_indicator <- if (article_chat$is_loading) {
        div(
          class = "d-flex justify-content-start mb-2 text-muted",
          div(
            class = "p-2 bg-secondary text-white rounded-3", style = "max-width: 85%; opacity: 0.7;",
            tagList(icon("spinner", class = "fa-spin"), " ИИ анализирует...")
          )
        )
      } else {
        NULL
      }

      card(
        class = "mt-0 border-info h-100",
        card_header(
          class = "bg-info text-white d-flex justify-content-between align-items-center",
          "Консультация по статье",
          actionButton("close_article_chat", "Закрыть чат", class = "btn-sm text-info fw-bold", style = "background-color: white;")
        ),
        card_body(
          style = "height: 500px; overflow-y: auto;",
          tagList(messages, loading_indicator)
        ),
        card_footer(
          fluidRow(
            column(9, textInput("article_user_input", label = NULL, placeholder = "Ваш вопрос по тексту...", width = "100%")),
            column(3, actionButton("send_article_msg", "Спросить", icon = icon("paper-plane"), class = "btn-info w-100", style = "margin-top: 0;"))
          )
        )
      )
    } else {
      div(class = "mt-4", actionButton("init_article_ai", "Начать консультацию", icon = icon("robot"), class = "btn-info w-100 fs-5"))
    }

    card(
      card_header(
        class = "d-flex justify-content-between align-items-center bg-primary text-white",
        "Подробная информация",
        actionButton("back_to_list", " Назад к списку", icon = icon("arrow-left"), class = "btn-light btn-sm text-primary fw-bold", style = "background-color: white;")
      ),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          tagList(
            h2(article$title),
            tags$hr(),
            tags$p(tags$strong("Разделы: "), sections_display),
            tags$p(tags$strong("Авторы: "), authors_display),
            tags$p(tags$strong("Дата: "), as.character(article$date)),
            card(
              class = "mt-3 border-secondary bg-dark text-white",
              card_header("Аннотация", class = "text-muted"),
              card_body(article$abstract)
            )
          ),
          div(
            class = "h-100",
            article_chat_ui
          )
        )
      )
    )
  })

  # Обработчики для чата по статье
  observeEvent(input$init_article_ai, {
    article_chat$is_active <- TRUE
    article_chat$is_loading <- TRUE

    current_id <- selected_id()
    idx <- which(as.character(v$articles$id) == as.character(current_id))
    art_row <- v$articles[idx[1], ]
    art_id <- as.character(art_row$id)
    art_title <- as.character(art_row$title %||% "")
    art_abstract <- as.character(art_row$abstract %||% "")

    init_msg <- list(list(
      role    = "user",
      content = "Привет! Пожалуйста, кратко представь эту статью и скажи, чем ты можешь помочь."
    ))

    future_promise <- future(
      {
        tryCatch(
          ask_article_ai(art_id, art_title, art_abstract, init_msg),
          error = function(e) paste("Ошибка работы с ИИ:", e$message)
        )
      },
      globals = list(
        ask_article_ai = ask_article_ai,
        art_id         = art_id,
        art_title      = art_title,
        art_abstract   = art_abstract,
        init_msg       = init_msg,
        `%||%`         = `%||%`
      ),
      seed = TRUE
    )

    future_promise %...>% (function(resp) {
      article_chat$history <- list(list(role = "ai", text = resp))
      article_chat$is_loading <- FALSE
    }) %...!% (function(err) {
      article_chat$history <- list(list(role = "ai", text = "Системная ошибка ИИ."))
      article_chat$is_loading <- FALSE
    })
  })

  observeEvent(input$close_article_chat, {
    article_chat$is_active <- FALSE
    article_chat$history <- list()
  })

  observeEvent(input$send_article_msg, {
    user_text <- input$article_user_input
    req(user_text)

    article_chat$history <- append(article_chat$history, list(list(role = "user", text = user_text)))
    updateTextInput(session, "article_user_input", value = "")
    article_chat$is_loading <- TRUE

    # Конвертируем историю чата в формат OpenAI messages
    current_id <- selected_id()
    idx <- which(as.character(v$articles$id) == as.character(current_id))
    art_row <- v$articles[idx[1], ]
    art_id <- as.character(art_row$id)
    art_title <- as.character(art_row$title %||% "")
    art_abstract <- as.character(art_row$abstract %||% "")

    current_history <- article_chat$history
    oai_messages <- purrr::map(current_history, function(m) {
      list(
        role    = if (m$role == "ai") "assistant" else "user",
        content = m$text
      )
    })

    future_promise <- future(
      {
        tryCatch(
          ask_article_ai(art_id, art_title, art_abstract, oai_messages),
          error = function(e) paste("Ошибка работы с ИИ:", e$message)
        )
      },
      globals = list(
        ask_article_ai = ask_article_ai,
        art_id         = art_id,
        art_title      = art_title,
        art_abstract   = art_abstract,
        oai_messages   = oai_messages,
        `%||%`         = `%||%`
      ),
      seed = TRUE
    )

    future_promise %...>% (function(resp) {
      article_chat$history <- append(article_chat$history, list(list(role = "ai", text = resp)))
      article_chat$is_loading <- FALSE
    }) %...!% (function(err) {
      article_chat$history <- append(article_chat$history, list(list(role = "ai", text = "Системная ошибка ИИ.")))
      article_chat$is_loading <- FALSE
    })
  })

  # --- Общий AI Чат ---
  global_chat_data <- reactiveValues(
    history = list(list(role = "ai", text = "Привет! Я готов обсудить статьи по кибербезопасности. Вопросы?")),
    is_loading = FALSE
  )

  output$chat_history <- renderUI({
    messages <- lapply(global_chat_data$history, function(m) {
      if (m$role == "ai") {
        div(
          class = "d-flex justify-content-start mb-3",
          div(class = "p-3 bg-secondary text-white rounded-3", style = "max-width: 75%;", m$text)
        )
      } else {
        div(
          class = "d-flex justify-content-end mb-3",
          div(class = "p-3 bg-primary text-white rounded-3", style = "max-width: 75%;", m$text)
        )
      }
    })

    loading_indicator <- if (global_chat_data$is_loading) {
      div(
        class = "d-flex justify-content-start mb-3 text-muted",
        div(
          class = "p-3 bg-secondary text-white rounded-3", style = "max-width: 75%; opacity: 0.7;",
          tagList(icon("spinner", class = "fa-spin"), " AI печатает ответ...")
        )
      )
    } else {
      NULL
    }

    tagList(messages, loading_indicator)
  })

  observeEvent(input$send_msg, {
    user_text <- input$user_input
    req(user_text)

    global_chat_data$history <- append(global_chat_data$history, list(list(role = "user", text = user_text)))
    updateTextInput(session, "user_input", value = "")
    global_chat_data$is_loading <- TRUE

    current_history <- global_chat_data$history
    oai_messages <- purrr::map(current_history, function(m) {
      list(
        role    = if (m$role == "ai") "assistant" else "user",
        content = m$text
      )
    })


    mcp_url <- Sys.getenv("MCP_SERVER_URL", unset = "http://127.0.0.1:8000")

    future_promise <- future(
      {
        tryCatch(
          {
            resp <- httr2::request(mcp_url) |>
              httr2::req_url_path_append("chat") |>
              httr2::req_body_json(
                list(messages = oai_messages),
                auto_unbox = TRUE
              ) |>
              httr2::req_timeout(120) |>
              httr2::req_error(is_error = function(r) FALSE) |>
              httr2::req_perform() |>
              httr2::resp_body_json(simplifyVector = FALSE)

            as.character(resp$answer %||% "Пустой ответ от AI-сервера")
          },
          error = function(e) paste("Ошибка при обращении к AI-сервису:", e$message)
        )
      },
      globals = list(
        mcp_url      = mcp_url,
        oai_messages = oai_messages,
        `%||%`       = `%||%`
      ),
      seed = TRUE
    )

    future_promise %...>% (function(ai_response) {
      global_chat_data$history <- append(global_chat_data$history, list(list(role = "ai", text = ai_response)))
      global_chat_data$is_loading <- FALSE
    }) %...!% (function(error) {
      global_chat_data$history <- append(global_chat_data$history, list(list(role = "ai", text = "Произошла системная ошибка при обращении к AI.")))
      global_chat_data$is_loading <- FALSE
    })
  })
}

shinyApp(ui, server)
