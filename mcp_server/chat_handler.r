# Agentic loop: оркестрация запросов к xAI Grok с tool calling.
# Выполняет цикл: запрос к модели → вызов инструментов → повтор
# до получения финального текстового ответа.

source(here::here("mcp_server", "db_tools.r"))
source(here::here("mcp_server", "tool_definitions.r"))


# Маппинг имени инструмента → R-функции
TOOL_HANDLERS <- list(
  search_articles        = tool_search_articles,
  filter_by_tags         = tool_filter_by_tags,
  get_article_by_id      = tool_get_article_by_id,
  get_statistics         = tool_get_statistics,
  get_articles_by_date   = tool_get_articles_by_date,
  get_tags_and_categories = tool_get_tags_and_categories
)


#' Выполнить один вызов инструмента
#'
#' @param tool_name Имя инструмента
#' @param tool_args Список аргументов (уже распарсенный JSON)
#' @return JSON-строка с результатом (или ошибкой)
.execute_tool <- function(tool_name, tool_args) {
  handler <- TOOL_HANDLERS[[tool_name]]

  if (is.null(handler)) {
    result <- list(error = glue::glue("Неизвестный инструмент: {tool_name}"))
  } else {
    result <- tryCatch(
      handler(tool_args),
      error = function(e) {
        list(error = glue::glue("Ошибка выполнения {tool_name}: {e$message}"))
      }
    )
  }

  jsonlite::toJSON(result, auto_unbox = TRUE, null = "null")
}


#' Выполнить один запрос к xAI API
#'
#' @param messages Список сообщений в формате OpenAI
#' @param use_tools Логический, передавать ли инструменты модели
#' @return Распарсенный ответ от API (список)
.call_xai <- function(messages, use_tools = TRUE) {
  api_key  <- Sys.getenv("API_KEY")
  endpoint <- Sys.getenv("ENDPOINT")
  model    <- Sys.getenv("AI_MODEL", unset = "grok-4.3")

  if (!nzchar(api_key) || !nzchar(endpoint)) {
    stop("API_KEY или ENDPOINT не заданы в переменных окружения")
  }

  body <- list(
    model    = model,
    messages = messages
  )

  if (use_tools) {
    body$tools       <- TOOL_DEFINITIONS
    body$tool_choice <- "auto"
  }

  resp <- httr2::request(endpoint) |>
    httr2::req_url_path_append("chat", "completions") |>
    httr2::req_headers(
      "Authorization" = glue::glue("Bearer {api_key}"),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(120) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (status != 200L) {
    err_msg <- parsed$error$message %||% glue::glue("HTTP {status}")
    stop(glue::glue("xAI API ошибка: {err_msg}"))
  }

  parsed
}


#' Основной agentic loop с tool calling
#'
#' @param messages      Начальная история чата (список в формате OpenAI)
#' @param system_prompt Системный промпт (строка)
#' @param max_iterations Максимальное количество итераций tool calling
#' @return Строка с финальным ответом модели
run_chat <- function(messages,
                     system_prompt  = NULL,
                     max_iterations = 8L) {

  # Подготавливаем полный список сообщений
  full_messages <- list()

  if (!is.null(system_prompt) && nzchar(system_prompt)) {
    full_messages <- c(
      list(list(role = "system", content = system_prompt)),
      messages
    )
  } else {
    default_system <- paste0(
      "Ты — AI-ассистент для работы с базой данных научных статей по кибербезопасности. ",
      "У тебя есть доступ к инструментам для поиска и фильтрации статей. ",
      "Отвечай на русском языке, будь точен и лаконичен. ",
      "При необходимости используй инструменты для получения актуальных данных из БД."
    )
    full_messages <- c(
      list(list(role = "system", content = default_system)),
      messages
    )
  }

  # Agentic loop
  for (i in seq_len(max_iterations)) {
    resp <- .call_xai(full_messages, use_tools = TRUE)

    choice        <- resp$choices[[1]]
    finish_reason <- choice$finish_reason
    message_obj   <- choice$message

    # Добавляем ответ модели в историю
    full_messages <- c(full_messages, list(message_obj))

    # Финальный текстовый ответ
    if (finish_reason == "stop" || finish_reason == "end_turn") {
      return(as.character(message_obj$content %||% ""))
    }

    # Модель хочет вызвать инструменты
    if (finish_reason == "tool_calls") {
      tool_calls <- message_obj$tool_calls

      if (is.null(tool_calls) || length(tool_calls) == 0) {
        return(as.character(message_obj$content %||% ""))
      }

      # Выполняем все запрошенные инструменты
      tool_results <- purrr::map(tool_calls, function(tc) {
        tool_name <- tc[["function"]]$name
        tool_args <- tryCatch(
          jsonlite::fromJSON(tc[["function"]]$arguments, simplifyVector = FALSE),
          error = function(e) list()
        )

        result_str <- .execute_tool(tool_name, tool_args)

        list(
          role         = "tool",
          tool_call_id = tc$id,
          content      = result_str
        )
      })

      # Добавляем результаты в историю
      full_messages <- c(full_messages, tool_results)
      next
    }

    # Любой другой finish_reason — возвращаем что есть
    return(as.character(message_obj$content %||% ""))
  }

  # Если превысили max_iterations — последний запрос без инструментов
  cli::cli_warn("Tool calling: превышено max_iterations={max_iterations}, запрос без tools")

  resp          <- .call_xai(full_messages, use_tools = FALSE)
  message_obj   <- resp$choices[[1]]$message
  as.character(message_obj$content %||% "")
}
