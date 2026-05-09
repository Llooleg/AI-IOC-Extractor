# Plumber HTTP API — связующее звено между Shiny и chat_handler.r
# Endpoint: POST /chat         — глобальный AI чат
# Endpoint: POST /article-chat — консультация по конкретной статье
# Endpoint: GET  /health       — проверка работоспособности

library(plumber)
library(here)

source(here("mcp_server", "chat_handler.r"))


#* @apiTitle MCP сервер — AI чат с доступом к БД
#* @apiDescription R Plumber сервер для оркестрации AI запросов к xAI Grok
#*   с инструментами доступа к MongoDB (cybersecurity articles).


#* Проверка работоспособности сервера
#* @get /health
#* @serializer json
function() {
  list(
    status    = "ok",
    model     = Sys.getenv("AI_MODEL", unset = "grok-4.3"),
    endpoint  = Sys.getenv("ENDPOINT"),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}


#* Глобальный AI чат — модель может использовать инструменты для запросов к БД
#*
#* @param req Запрос от Shiny
#* @post /chat
#* @serializer json
function(req) {
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(body)) {
    plumber::forward()
    return(list(error = "Не удалось разобрать JSON тело запроса"))
  }

  messages <- body$messages
  if (is.null(messages) || length(messages) == 0) {
    return(list(error = "Поле 'messages' обязательно"))
  }

  answer <- tryCatch(
    run_chat(messages = messages),
    error = function(e) {
      cli::cli_warn("Ошибка в run_chat(): {e$message}")
      glue::glue("Произошла ошибка при обращении к AI: {e$message}")
    }
  )

  list(answer = answer)
}


#* Консультация по конкретной статье (с контекстом)
#*
#* @param req Запрос от Shiny
#* @post /article-chat
#* @serializer json
function(req) {
  body <- tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (is.null(body)) {
    return(list(error = "Не удалось разобрать JSON тело запроса"))
  }

  # Ожидаем:
  # {
  #   article_id:      "...",
  #   article_title:   "...",
  #   article_abstract:"...",
  #   messages:        [{role, content}, ...]
  # }
  article_id <- as.character(body$article_id %||% "")
  article_title <- as.character(body$article_title %||% "")
  article_abstract <- as.character(body$article_abstract %||% "")
  messages <- body$messages %||% list()

  if (length(messages) == 0) {
    return(list(error = "Поле 'messages' обязательно"))
  }

  system_prompt <- paste0(
    "Ты — AI-ассистент, помогающий анализировать научные статьи по кибербезопасности.\n\n",
    "Сейчас пользователь консультируется по следующей статье:\n",
    if (nzchar(article_title)) glue::glue("Заголовок: {article_title}\n") else "",
    if (nzchar(article_id)) glue::glue("ID статьи: {article_id}\n") else "",
    if (nzchar(article_abstract)) glue::glue("Аннотация:\n{article_abstract}\n\n") else "\n",
    "У тебя есть инструменты для поиска связанных статей в базе данных. ",
    "Используй их при необходимости для расширения контекста.\n",
    "Отвечай на русском языке, будь точен и полезен."
  )

  answer <- tryCatch(
    run_chat(messages = messages, system_prompt = system_prompt),
    error = function(e) {
      cli::cli_warn("Ошибка в /article-chat run_chat(): {e$message}")
      glue::glue("Произошла ошибка при обращении к AI: {e$message}")
    }
  )

  list(answer = answer)
}
