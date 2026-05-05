#' MongoDB connection establishment
#'
#' @param host Hostname of the MongoDB server (default: value from MONGO_HOST environment variable)
#' @param user Username for MongoDB authentication (default: value from MONGO_USER environment variable)
#' @param pass Password for MongoDB authentication (default: value from MONGO_PASS environment
#' @return Object representing the MongoDB collection connection
#' @export
mongodb_connection <- function(host = Sys.getenv("MONGO_HOST"),
                               user = Sys.getenv("MONGO_USER"),
                               pass = Sys.getenv("MONGO_PASS")) {
  if (host == "" || user == "" || pass == "") {
    stop("Error: Environment variables not found. Check the .Renviron file and restart R.")
  }
  url <- sprintf(
    "mongodb://%s:%s@%s:27017/cybersecurity?authSource=admin",
    user, pass, host
  )
  mongolite::mongo(collection = "metadata", db = "cybersecurity_articles", url = url)
}


#' First load JSON data into MongoDB
#'
#' @param file_path  Path to the JSON file containing the data
#' @param collection_obj A mongolite collection object to insert data into
#' @export
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
    item$date <- as.POSIXct(item$date, format = "%Y-%m-%d", tz = "UTC")
    item$datestamp <- as.POSIXct(item$datestamp, format = "%Y-%m-%d", tz = "UTC")
    item$extracted_at <- Sys.time()

    return(item)
  })

  json_strings <- sapply(processed_list, function(x) {
    jsonlite::toJSON(x, auto_unbox = TRUE)
  })
  collection_obj$insert(json_strings)
  message(sprintf("Successfully loaded %d records into MongoDB", length(json_strings)))
}


#' Update existing records in MongoDB with a progress bar
#'
#' @param file_path Path to the JSON file with updates
#' @param collection_obj A mongolite collection object
#'
mongodb_update_json <- function(file_path, collection_obj) {
  update_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  n_total <- length(update_data)
  if (n_total == 0) {
    stop("Bad json.")
  }

  message(sprintf("Updating %d documents...", n_total))

  pb <- utils::txtProgressBar(min = 0, max = n_total, style = 3)
  updated_count <- 0

  purrr::iwalk(update_data, function(item, index) {
    if (is.null(item$id)) {
      warning(sprintf("\nSkipped document at index %d without 'id'.", index))
      utils::setTxtProgressBar(pb, index)
      return(NULL)
    }

    doc_id <- item$id
    fields_to_update <- item
    fields_to_update$id <- NULL

    if (!is.null(fields_to_update$date)) {
      fields_to_update$date <- as.POSIXct(fields_to_update$date, format = "%Y-%m-%d", tz = "UTC")
    }

    fields_to_update$updated_at <- Sys.time()
    success <- collection_obj$update(
      query = sprintf('{"id": "%s"}', doc_id),
      update = sprintf('{"$set": %s}', jsonlite::toJSON(fields_to_update, auto_unbox = TRUE)),
      upsert = FALSE
    )

    if (success$modifiedCount > 0) {
      updated_count <<- updated_count + 1
    }

    utils::setTxtProgressBar(pb, index)
  })

  close(pb)
  message(sprintf("Updating completed. Modified documents: %d", updated_count))
}
