# MongoDB tool functions — доступны модели через tool calling.
# Каждая функция принимает параметры в виде именованного списка `params`
# и возвращает JSON-совместимый список/символ для передачи обратно модели.

#' Подключение к MongoDB
#'
#' @return mongolite::mongo объект
.connect_db <- function() {
  mongolite::mongo(
    collection = "metadata",
    db = "cybersecurity_articles",
    url = sprintf(
      "mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin",
      Sys.getenv("MONGO_USER"),
      Sys.getenv("MONGO_PASS"),
      Sys.getenv("MONGO_HOST")
    )
  )
}

#' Поиск статей по ключевым словам в заголовке и аннотации
#'
#' @param params список с полями: query (str), limit (int)
#' @return JSON-строка с результатами
tool_search_articles <- function(params) {
  query_text <- as.character(params$query %||% "")
  limit_n <- as.integer(params$limit %||% 10L)
  limit_n <- min(max(limit_n, 1L), 50L)

  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)
  mongo_query <- jsonlite::toJSON(list(
    `$or` = list(
      list(title = list(`$regex` = query_text, `$options` = "i")),
      list(abstract = list(`$regex` = query_text, `$options` = "i"))
    )
  ), auto_unbox = TRUE)

  results <- db$find(
    query  = mongo_query,
    fields = '{"_id":0,"id":1,"title":1,"date":1,"abstract":1,"tags":1,"categories":1}',
    limit  = limit_n
  )

  if (nrow(results) == 0) {
    return(list(count = 0L, articles = list()))
  }

  articles <- purrr::map(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    list(
      id         = as.character(row$id),
      title      = as.character(row$title),
      date       = as.character(row$date),
      abstract   = substr(as.character(row$abstract), 1, 400),
      tags       = if (!is.null(row$tags[[1]])) unlist(row$tags[[1]]) else character(0),
      categories = if (!is.null(row$categories[[1]])) unlist(row$categories[[1]]) else character(0)
    )
  })

  list(count = nrow(results), articles = articles)
}


#' Фильтрация статей по тегам и/или категориям
#'
#' @param params список с полями: tags (arr), categories (arr),
#'               context_tags (arr), limit (int)
#' @return JSON-совместимый список с результатами
tool_filter_by_tags <- function(params) {
  tags <- as.character(unlist(params$tags %||% list()))
  categories <- as.character(unlist(params$categories %||% list()))
  context_tags <- as.character(unlist(params$context_tags %||% list()))
  limit_n <- as.integer(params$limit %||% 10L)
  limit_n <- min(max(limit_n, 1L), 50L)

  if (length(tags) == 0 && length(categories) == 0 && length(context_tags) == 0) {
    return(list(error = "Укажите хотя бы один фильтр: tags, categories или context_tags"))
  }

  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)

  conditions <- list()
  if (length(tags) > 0) conditions <- c(conditions, list(list(tags = list(`$in` = as.list(tags)))))
  if (length(categories) > 0) conditions <- c(conditions, list(list(categories = list(`$in` = as.list(categories)))))
  if (length(context_tags) > 0) conditions <- c(conditions, list(list(context_tags = list(`$in` = as.list(context_tags)))))

  mongo_query <- if (length(conditions) == 1) {
    jsonlite::toJSON(conditions[[1]], auto_unbox = TRUE)
  } else {
    jsonlite::toJSON(list(`$or` = conditions), auto_unbox = TRUE)
  }

  results <- db$find(
    query  = mongo_query,
    fields = '{"_id":0,"id":1,"title":1,"date":1,"tags":1,"categories":1,"context_tags":1}',
    limit  = limit_n
  )

  if (nrow(results) == 0) {
    return(list(count = 0L, articles = list()))
  }

  articles <- purrr::map(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    list(
      id           = as.character(row$id),
      title        = as.character(row$title),
      date         = as.character(row$date),
      tags         = if (!is.null(row$tags[[1]])) unlist(row$tags[[1]]) else character(0),
      categories   = if (!is.null(row$categories[[1]])) unlist(row$categories[[1]]) else character(0),
      context_tags = if (!is.null(row$context_tags[[1]])) unlist(row$context_tags[[1]]) else character(0)
    )
  })

  list(count = nrow(results), articles = articles)
}


#' Получить полные данные статьи по ID
#'
#' @param params список с полем: article_id (str)
#' @return список с данными статьи или ошибкой
tool_get_article_by_id <- function(params) {
  article_id <- as.character(params$article_id %||% "")
  if (!nzchar(article_id)) {
    return(list(error = "article_id не указан"))
  }

  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)

  result <- db$find(
    query  = jsonlite::toJSON(list(id = article_id), auto_unbox = TRUE),
    fields = '{"_id":0}',
    limit  = 1L
  )

  if (nrow(result) == 0) {
    return(list(error = glue::glue("Статья с id='{article_id}' не найдена")))
  }

  row <- result[1, ]
  list(
    id           = as.character(row$id),
    title        = as.character(row$title),
    date         = as.character(row$date),
    abstract     = as.character(row$abstract),
    authors      = if (!is.null(row$authors[[1]])) unlist(row$authors[[1]]) else character(0),
    tags         = if (!is.null(row$tags[[1]])) unlist(row$tags[[1]]) else character(0),
    categories   = if (!is.null(row$categories[[1]])) unlist(row$categories[[1]]) else character(0),
    context_tags = if (!is.null(row$context_tags[[1]])) unlist(row$context_tags[[1]]) else character(0)
  )
}


#' Статистика по коллекции статей
#'
#' @param params пустой список (параметры не нужны)
#' @return список со статистикой
tool_get_statistics <- function(params = list()) {
  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)

  total_count <- db$count("{}")

  top_tags <- db$aggregate('[
    {"$unwind": "$tags"},
    {"$group": {"_id": "$tags", "count": {"$sum": 1}}},
    {"$sort": {"count": -1}},
    {"$limit": 10}
  ]')

  top_cats <- db$aggregate('[
    {"$unwind": "$categories"},
    {"$group": {"_id": "$categories", "count": {"$sum": 1}}},
    {"$sort": {"count": -1}},
    {"$limit": 10}
  ]')

  date_range <- db$aggregate('[
    {"$group": {
      "_id": null,
      "min_date": {"$min": "$date"},
      "max_date": {"$max": "$date"}
    }}
  ]')

  list(
    total_articles = total_count,
    top_tags = if (nrow(top_tags) > 0) {
      purrr::map2(top_tags$`_id`, top_tags$count, ~ list(tag = .x, count = .y))
    } else {
      list()
    },
    top_categories = if (nrow(top_cats) > 0) {
      purrr::map2(top_cats$`_id`, top_cats$count, ~ list(category = .x, count = .y))
    } else {
      list()
    },
    date_range = if (nrow(date_range) > 0) {
      list(
        from = as.character(date_range$min_date),
        to   = as.character(date_range$max_date)
      )
    } else {
      list()
    }
  )
}


#' Получить статьи за период
#'
#' @param params список с полями: start_date (str, YYYY-MM-DD),
#'               end_date (str, YYYY-MM-DD), limit (int)
#' @return список с результатами
tool_get_articles_by_date <- function(params) {
  start_date <- as.character(params$start_date %||% "")
  end_date <- as.character(params$end_date %||% "")
  limit_n <- as.integer(params$limit %||% 15L)
  limit_n <- min(max(limit_n, 1L), 50L)

  if (!nzchar(start_date) || !nzchar(end_date)) {
    return(list(error = "Укажите start_date и end_date в формате YYYY-MM-DD"))
  }

  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)

  mongo_query <- jsonlite::toJSON(list(
    date = list(
      `$gte` = list(`$date` = glue::glue("{start_date}T00:00:00Z")),
      `$lte` = list(`$date` = glue::glue("{end_date}T23:59:59Z"))
    )
  ), auto_unbox = TRUE)

  results <- db$find(
    query  = mongo_query,
    fields = '{"_id":0,"id":1,"title":1,"date":1,"tags":1,"categories":1}',
    sort   = '{"date": -1}',
    limit  = limit_n
  )

  if (nrow(results) == 0) {
    return(list(count = 0L, articles = list()))
  }

  articles <- purrr::map(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    list(
      id         = as.character(row$id),
      title      = as.character(row$title),
      date       = as.character(row$date),
      tags       = if (!is.null(row$tags[[1]])) unlist(row$tags[[1]]) else character(0),
      categories = if (!is.null(row$categories[[1]])) unlist(row$categories[[1]]) else character(0)
    )
  })

  list(count = nrow(results), articles = articles)
}


#' Получить все уникальные теги и категории с частотами
#'
#' @param params список с полем: limit (int, max на каждый тип)
#' @return список с тегами и категориями
tool_get_tags_and_categories <- function(params = list()) {
  limit_n <- as.integer(params$limit %||% 30L)
  limit_n <- min(max(limit_n, 5L), 100L)

  db <- .connect_db()
  on.exit(db$disconnect(), add = TRUE)

  tags_agg <- db$aggregate(glue::glue('[
    {{"$unwind": "$tags"}},
    {{"$group": {{"_id": "$tags", "count": {{"$sum": 1}}}}}},
    {{"$sort": {{"count": -1}}}},
    {{"$limit": {limit_n}}}
  ]'))

  cats_agg <- db$aggregate(glue::glue('[
    {{"$unwind": "$categories"}},
    {{"$group": {{"_id": "$categories", "count": {{"$sum": 1}}}}}},
    {{"$sort": {{"count": -1}}}},
    {{"$limit": {limit_n}}}
  ]'))

  ctx_agg <- db$aggregate(glue::glue('[
    {{"$unwind": "$context_tags"}},
    {{"$group": {{"_id": "$context_tags", "count": {{"$sum": 1}}}}}},
    {{"$sort": {{"count": -1}}}},
    {{"$limit": {limit_n}}}
  ]'))

  list(
    tags = if (nrow(tags_agg) > 0) {
      purrr::map2(tags_agg$`_id`, tags_agg$count, ~ list(name = .x, count = .y))
    } else {
      list()
    },
    categories = if (nrow(cats_agg) > 0) {
      purrr::map2(cats_agg$`_id`, cats_agg$count, ~ list(name = .x, count = .y))
    } else {
      list()
    },
    context_tags = if (nrow(ctx_agg) > 0) {
      purrr::map2(ctx_agg$`_id`, ctx_agg$count, ~ list(name = .x, count = .y))
    } else {
      list()
    }
  )
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
