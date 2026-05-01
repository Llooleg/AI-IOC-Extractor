#' Harvest arXiv OAI-PMH Records for a Category
#'
#' @param category OAI category, e.g. "cs.CR"
#' @param output_file Path to output JSON file
#' @param verbose Print progress messages
#'
#' @return Invisibly returns a data frame with harvested metadata
#' @export
#'
harvest_arxiv_category <- function(category = "cs.CR",
                                   output_file = "arxiv.json",
                                   verbose = TRUE) {

  base_url <- "https://export.arxiv.org/oai2"

  say <- function(...) if (verbose) message(...)

  # Load previous data
  if (file.exists(output_file)) {
    existing_df <- jsonlite::fromJSON(output_file)
    from_date   <- max(as.Date(existing_df$datestamp), na.rm = TRUE)
    existing_ids <- existing_df$id
    say(sprintf("Existing data: %d records. Fetching from %s", nrow(existing_df), from_date))
  } else {
    existing_df  <- NULL
    existing_ids <- character(0)
    from_date    <- NULL
    say("No existing data. Full harvest.")
  }

  resumption_token <- NULL
  new_records <- list()
  batch_num <- 0

  repeat {
    batch_num <- batch_num + 1
    say(sprintf("Batch %d | New so far: %d", batch_num, length(new_records)))

    # Build query
    if (is.null(resumption_token)) {
      query <- list(
        verb = "ListRecords",
        metadataPrefix = "oai_dc",
        set = category
      )
      if (!is.null(from_date))
        query$from <- format(from_date, "%Y-%m-%d")

      resp <- httr::GET(base_url, query = query)
    } else {
      resp <- httr::GET(base_url, query = list(
        verb = "ListRecords",
        resumptionToken = resumption_token
      ))
    }

    # Handle retry
    if (httr::status_code(resp) == 503) {
      retry <- as.integer(httr::headers(resp)[["retry-after"]])
      retry <- ifelse(is.na(retry), 30, retry)
      say(sprintf("503. Waiting %d sec...", retry + 5))
      Sys.sleep(retry + 5)
      next
    }

    if (httr::status_code(resp) != 200) {
      say(sprintf("Unexpected status %d. Stopping.", httr::status_code(resp)))
      break
    }

    xml <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))

    # If there are no new records
    err <- xml2::xml_find_first(xml, ".//*[local-name()='error']")
    if (!is.na(err) && xml2::xml_attr(err, "code") == "noRecordsMatch") {
      say("No new records. Up to date.")
      return(invisible(existing_df))
    }

    batch_raw <- parse_oai_records(xml)
    batch_new <- Filter(function(r) !r$id %in% existing_ids, batch_raw)
    new_records <- c(new_records, batch_new)

    say(sprintf("  Batch parsed: %d, new: %d",
                length(batch_raw), length(batch_new)))

    # Resumption token
    token <- xml2::xml_find_first(xml, ".//*[local-name()='resumptionToken']")
    resumption_token <- if (!is.na(token)) trimws(xml2::xml_text(token)) else NULL
    if (identical(resumption_token, "")) resumption_token <- NULL

    # Periodic checkpoint
    if (batch_num %% 2 == 0 && length(new_records) > 0) {
      combined <- dplyr::bind_rows(
        existing_df,
        dplyr::bind_rows(new_records)
      )
      jsonlite::write_json(combined, output_file, pretty = TRUE)
      say(sprintf("Checkpoint saved: %d total", nrow(combined)))
    }

    # Done
    if (is.null(resumption_token)) {
      say("Harvest complete.")
      break
    }

    Sys.sleep(10)
  }

  if (length(new_records) == 0) {
    say("No new records.")
    return(invisible(existing_df))
  }

  new_df <- dplyr::bind_rows(new_records)

  final_df <- if (!is.null(existing_df)) {
    dplyr::bind_rows(existing_df, new_df) |>
      dplyr::distinct(id, .keep_all = TRUE) |>
      dplyr::arrange(dplyr::desc(datestamp))
  } else new_df

  jsonlite::write_json(final_df, output_file, pretty = TRUE)

  say(sprintf("Done. %d total records (%d new). Saved to %s",
              nrow(final_df), nrow(new_df), output_file))

  invisible(final_df)
}
