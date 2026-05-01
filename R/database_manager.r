library(mongolite)
library(jsonlite)
library(dplyr)
library(purrr)

host <- Sys.getenv("MONGO_HOST")
user <- Sys.getenv("MONGO_USER")
pass <- Sys.getenv("MONGO_PASS")

mongodb_connection <- function(host, user, pass) {
  if (host == "" || user == "" || pass == "") {
    stop("Error: Missing MongoDB credentials. Please set them in .Renviron or environment variables.")
  }
  url <- sprintf("mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin", 
                 user, pass, host)
  mongolite::mongo(collection = "metadata", db = "cybersecurity_articles", url = url)
}

mongodb_load_json <- function(file_path, collection_obj) {
  raw_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  processed_list <- map(raw_data, function(item) {
    if (!is.null(item$authors)) {
      item$authors <- trimws(unlist(strsplit(item$authors, "\\|")))
    }
    if (!is.null(item$categories)) {
      item$categories <- trimws(unlist(strsplit(item$categories, "\\|")))
    }
    
    item$date <- as.POSIXct(item$date, format="%Y-%m-%d", tz="UTC")
    item$datestamp <- as.POSIXct(item$datestamp, format="%Y-%m-%d", tz="UTC")
    
    item$extracted_at <- Sys.time()
    return(item)
  })

  json_strings <- sapply(processed_list, function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE)
  })
  message("Loading data into MongoDB...\n")

  collection_obj$insert(json_strings)
  message(sprintf("Successfully inserted %d documents into MongoDB", length(json_strings)))
}

