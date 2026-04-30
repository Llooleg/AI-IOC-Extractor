library(shiny)
library(mongolite)
library(DT)
library(dplyr)
library(tidyr)
library(httr2)

db <- mongolite::mongo(
  collection = "metadata",
  db = "cybersecurity_articles",
  url = sprintf("mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin",
                Sys.getenv("MONGO_USER"), Sys.getenv("MONGO_PASS"), Sys.getenv("MONGO_HOST"))
)

fetch_data <- function() {
  db <- mongolite::mongo(
    collection = "metadata",
    db = "cybersecurity_articles",
    url = sprintf("mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin",
                  Sys.getenv("MONGO_USER"), Sys.getenv("MONGO_PASS"), Sys.getenv("MONGO_HOST"))
  )
  return(db$find('{}'))
}

all_articles <- db$find('{}')

all_articles <- all_articles %>%
  mutate(date = as.Date(date))

all_topics <- all_articles %>%
  select(id, categories) %>%
  unnest(categories) %>%
  group_by(categories) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  tags$nav(class = "main-menu",
    tags$ul(
      tags$li(actionLink("nav_home", label = list(tags$i(class = "fa fa-home fa-2x"), tags$span(class = "nav-text", "Главная")))),
      tags$li(actionLink("nav_articles", label = list(tags$i(class = "fa fa-book fa-2x"), tags$span(class = "nav-text", "Библиотека")))),
      tags$li(actionLink("nav_topics", label = list(tags$i(class = "fa fa-tags fa-2x"), tags$span(class = "nav-text", "Темы")))),
      tags$li(actionLink("nav_chat", label = list(tags$i(class = "fa fa-comments fa-2x"), tags$span(class = "nav-text", "AI Чат"))))
    )
  ),

  tags$div(style = "width: 75%; margin: 0 auto; padding-top: 20px;",
    uiOutput("current_page")
  )
)

server <- function(input, output, session) {
  chat_data <- reactiveValues(
    history = list(list(role = "ai", text = "Привет! Я готов обсудить статьи по кибербезопасности. О чем спросишь?"))
  )

  observeEvent(input$nav_chat, { current_view("chat") })
  observeEvent(input$send_msg, {
    user_text <- input$user_input
    req(user_text)
    # ПОМЕНЯТЬ КАК ГОТОВ СЕРВ БУДЕТ
    chat_data$history[[length(chat_data$history) + 1]] <- list(role = "user", text = user_text)
    updateTextInput(session, "user_input", value = "")
    tryCatch({
      resp <- request("http://ai-container:8000/v1/chat") %>%
        req_body_json(list(message = user_text)) %>%
        req_perform() %>%
        resp_body_json()
      ai_response <- resp$choices[[1]]$message$content
    }, error = function(e) {
      ai_response <- "Error occurred while fetching AI response."
    })

    chat_data$history[[length(chat_data$history) + 1]] <- list(role = "ai", text = ai_response)
  })


  v <- reactiveValues(
    articles = fetch_data(),
    last_updated = Sys.time()
  )
  current_view <- reactiveVal("home")
  selected_id <- reactiveVal(NULL)
  topic_filter <- reactiveVal("")

  observeEvent(input$nav_home, { current_view("home") })
  observeEvent(input$refresh_db, {
    withProgress(message = 'Подключение к базе данных...', value = 0, {
      v$articles <- fetch_data()
      v$last_updated <- Sys.time()
      setProgress(1, detail = "Данные обновлены")
    })
    showNotification(paste("База данных успешно обновлена в", format(v$last_updated, "%H:%M:%S")), type = "message")
  })

  observeEvent(input$nav_articles, {
    topic_filter("")
    current_view("articles")
  })
  observeEvent(input$nav_topics, { current_view("topics") })
  observeEvent(input$back_to_list, { current_view("articles") })

  observeEvent(input$topics_table_rows_selected, {
    req(input$topics_table_rows_selected)
    selected_topic <- all_topics$categories[input$topics_table_rows_selected]
    topic_filter(selected_topic)
    current_view("articles")
  })

  observeEvent(input$articles_table_rows_selected, {
    req(input$articles_table_rows_selected)
    data <- filtered_data()
    selected_id(data$id[input$articles_table_rows_selected])
    current_view("article_detail")
  })

  filtered_data <- reactive({
    data <- v$articles
    if (!is.null(input$date_range)) {
      data <- data %>% filter(as.Date(date) >= input$date_range[1] & 
                               as.Date(date) <= input$date_range[2])[cite: 2]
    }
    data
  })

  output$current_page <- renderUI({
    view <- current_view()
    if (view == "chat") {
      tagList(
        h2("AI Чат"),
        tags$div(class = "full-page-chat",
          tags$div(id = "chat-scroll-area", class = "chat-body-large",
            lapply(chat_data$history, function(m) {
              tags$div(class = paste0("chat-msg msg-", m$role), m$text)
            })
          ),
          tags$div(class = "chat-input-area",
            textInput("user_input", NULL, placeholder = "Введите ваш вопрос...", width = "100%"),
            actionButton("send_msg", "Отправить", icon = icon("paper-plane"), class = "btn-send")
          )
        )
      )
    } else if (view == "home") {
      tagList(
        h1("Управление данными"),
        p(paste("Последнее обновление:", format(v$last_updated, "%d.%m.%Y %H:%M:%S"))),
        actionButton("refresh_db", " Обновить", icon = icon("sync"), class = "btn-refresh"),
        hr(),
        tags$div(class = "welcome-section",
          h3("Добро пожаловать в электронную библиотеку")
        )
      )
    } else if (view == "articles") {
      tagList(
        h2("Библиотека"),
        dateRangeInput("date_range", "Временной фильтр:",
                       start = min(as.Date(all_articles$date), na.rm = TRUE),
                       end = max(as.Date(all_articles$date), na.rm = TRUE),
                       language = "ru", separator = " : "),
        tags$div(style = "background: rgba(0,0,0,0.3); padding: 15px; border-radius: 8px;",
          DTOutput("articles_table")
        )
      )
    } else if (view == "topics") {
      tagList(
        h2("Темы"),
        tags$div(style = "background: rgba(255,255,255,0.05); padding: 20px;",
          DTOutput("topics_table")
        )
      )
    } else if (view == "article_detail") {
      article <- v$articles %>% filter(id == selected_id())[cite: 2]
      tagList(
        actionButton("back_to_list", " Назад", icon = icon("arrow-left"), class = "btn-secondary"),
        hr(),
        tags$div(class = "article-card",
          h1(article$title),
          tags$p(tags$b("Тег: "), span(class = "badge", article$tag)),
          tags$p(tags$b("Авторы: "), paste(unlist(article$authors), collapse = ", ")),
          tags$p(tags$b("Дата: "), article$date),
          tags$div(style = "background: rgba(255,255,255,0.1); padding: 20px; border-radius: 10px; margin-top:20px;",
            h4("Аннотация"),
            p(article$abstract)
          )
        )
      )
    }
  })

  output$articles_table <- renderDT({
    display_df <- filtered_data() %>%
      mutate(
        authors = sapply(authors, function(x) paste(unlist(x), collapse = ", ")),
        tag = sapply(tag, function(x) if(is.null(x) || is.na(x)) "" else paste(unlist(x), collapse = ", "))
      ) %>%
      select(
        "Название" = title,
        "Авторы" = authors,
        "Дата" = date,
        "Категории" = categories,
        "Тег" = tag
      )

    datatable(display_df,
              selection = 'single',
              filter = 'top',
              options = list(
                pageLength = 10,
                autoWidth = TRUE,
                search = list(search = topic_filter()),
                language = list(
                  search = "Общий поиск:",
                  info = "Показано с _START_ по _END_ из _TOTAL_ записей",
                  paginate = list(previous = "Назад", `next` = "Вперед")
                )
              )) %>%
      formatStyle(columns = names(display_df), color = '#EEE')
  })

  output$topics_table <- renderDT({
    datatable(all_topics, selection = 'single', options = list(pageLength = 15)) %>%
      formatStyle(columns = names(all_topics), color = '#EEE')
  })
}

shinyApp(ui, server)