library(mongolite)
library(jsonlite)
library(dplyr)
library(purrr)

host <- Sys.getenv("MONGO_HOST")
user <- Sys.getenv("MONGO_USER")
pass <- Sys.getenv("MONGO_PASS")

mongodb_connection <- function(host, user, pass) {
  
  if (host == "" || user == "" || pass == "") {
    stop("Ошибка: Переменные окружения не найдены. Проверьте файл .Renviron и перезапустите R.")
  }
  url <- sprintf("mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin", 
                 user, pass, host)
  mongolite::mongo(collection = "metadata", db = "cybersecurity_articles", url = url)
}

mongodb_load_json <- function(file_path, collection_obj) {
  raw_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  processed_list <- map(raw_data, function(item) {
    # там где перечисление через | меняем на список
    if (!is.null(item$authors)) {
      item$authors <- trimws(unlist(strsplit(item$authors, "\\|")))
    }
    if (!is.null(item$categories)) {
      item$categories <- trimws(unlist(strsplit(item$categories, "\\|")))
    }
    
    # форматирование дат
    item$date <- as.POSIXct(item$date, format="%Y-%m-%d", tz="UTC")
    item$datestamp <- as.POSIXct(item$datestamp, format="%Y-%m-%d", tz="UTC")
    
    item$extracted_at <- Sys.time()
    return(item)
  })

  json_strings <- sapply(processed_list, function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE)
  })
  message("Файл обработан. \n")

  collection_obj$insert(json_strings)
  message(sprintf("Успешно загружено %d записей в MongoDB", length(json_strings)))
}

con <- mongodb_connection(host, user, pass)
path <- "Mongo/metadata.json"
if (file.exists(path)) {
  mongodb_load_json(path, con)
} else {
  message("Файл не найден.")
}