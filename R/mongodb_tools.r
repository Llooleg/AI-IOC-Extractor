#' Create a MongoDB connection
#'
#' @param host MongoDB host name or IP.
#' @param user Username for authentication.
#' @param pass Password for authentication.
#' @param db Database name (default: "cybersecurity_articles").
#' @param collection Collection name (default: "metadata").
#'
#' @return A mongolite::mongo connection object.
#' @export
mongodb_connection <- function(
  host,
  user,
  pass,
  db = "cybersecurity_articles",
  collection = "metadata"
) {
  if (!nzchar(host) || !nzchar(user) || !nzchar(pass)) {
    stop("Missing MongoDB credentials. Provide host, user, pass.", call. = FALSE)
  }

  url <- sprintf(
    "mongodb://%s:%s@%s:27017/%s?authSource=admin",
    user, pass, host, db
  )

  mongolite::mongo(
    collection = collection,
    db         = db,
    url        = url
  )
}
#' Load and insert JSON data into MongoDB
#'
#' @param file_path Path to JSON file.
#' @param collection_obj A mongolite::mongo collection connection.
#'
#' @return Invisibly returns number of inserted records.
#' @export
mongodb_load_json <- function(file_path, collection_obj) {

  # ---- sanity checks ---------------------------------------------------------
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path, call. = FALSE)
  }
  if (!inherits(collection_obj, "mongo")) {
    stop("collection_obj must be a mongolite::mongo connection.", call. = FALSE)
  }

  # ---- load JSON -------------------------------------------------------------
  raw_data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  if (!is.list(raw_data)) {
    stop("JSON structure must be a list of records.", call. = FALSE)
  }

  # ---- transform -------------------------------------------------------------
  processed <- purrr::map(raw_data, function(item) {

    # authors/categories split by "|"
    if (!is.null(item$authors)) {
      item$authors <- trimws(unlist(strsplit(item$authors, "\\|")))
    }

    if (!is.null(item$categories)) {
      item$categories <- trimws(unlist(strsplit(item$categories, "\\|")))
    }

    # dates
    parse_date <- function(x) {
      if (is.null(x) || is.na(x)) return(NA)
      as.POSIXct(x, format = "%Y-%m-%d", tz = "UTC")
    }

    item$date       <- parse_date(item$date)
    item$datestamp  <- parse_date(item$datestamp)
    item$extracted_at <- Sys.time()

    item
  })

  # ---- convert to JSON strings ----------------------------------------------
  json_strings <- vapply(
    processed,
    FUN = function(x) jsonlite::toJSON(x, auto_unbox = TRUE),
    FUN.VALUE = character(1)
  )

  # ---- insert ---------------------------------------------------------------
  collection_obj$insert(json_strings)

  message(sprintf("Inserted %d records into MongoDB", length(json_strings)))

  invisible(length(json_strings))
}
