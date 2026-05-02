#' Download PDFs from arXiv for a Given Category
#'
#' @param download_path Directory where PDFs will be saved.
#' @param query arXiv query string (default: "cat:\"cs.CR\"").
#' @param total_limit Max number of papers to fetch (search limit).
#' @param batch_size How many papers to fetch per API call.
#' @param rate_limit Requests per second allowed for the PDF downloads.
#' @param retries Number of retry attempts for failed downloads.
#' @param verbose Whether to print progress information.
#' @param log_file Name of the CSV log stored inside the download directory.
#'
#' @return Invisibly returns a data frame with log results.
#' @export
download_arxiv_papers <- function(
  download_path,
  query        = 'cat:"cs.CR"',
  total_limit  = 10,
  batch_size   = 100,
  rate_limit   = 3,
  retries      = 5,
  log_file     = "download_log.csv",
  verbose      = TRUE
) {
  # ---- sanity checks ---------------------------------------------------------

  if (missing(download_path) || !nzchar(download_path)) {
    stop("download_path must be provided.", call. = FALSE)
  }

  if (!dir.exists(download_path)) {
    dir.create(download_path, recursive = TRUE)
  }

  say <- function(...) if (verbose) message(...)

  # ---- log handling ----------------------------------------------------------

  log_path <- file.path(download_path, log_file)

  if (file.exists(log_path)) {
    log_df <- utils::read.csv(log_path, stringsAsFactors = FALSE)
  } else {
    log_df <- data.frame(
      arxiv_id  = character(),
      file      = character(),
      status    = character(),
      timestamp = character(),
      stringsAsFactors = FALSE
    )
  }

  already_done <- log_df$arxiv_id[log_df$status == "ok"]

  # ---- harvest metadata ------------------------------------------------------

  say("Fetching metadata from arXiv.")
  all_results <- list()
  start <- 0

  repeat {
    results <- tryCatch(
      aRxiv::arxiv_search(
        query     = query,
        start     = start,
        limit     = batch_size,
        sort_by   = "submitted",
        ascending = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(results) || nrow(results) == 0) break

    all_results <- append(all_results, list(results))
    start <- start + nrow(results)

    say(sprintf("Fetched %d metadata records.", start))

    if (nrow(results) < batch_size) break
    if (start >= total_limit) break
    Sys.sleep(1 / rate_limit)
  }

  if (length(all_results) == 0) {
    say("No results from arXiv.")
    return(invisible(log_df))
  }

  papers <- do.call(rbind, all_results)
  say(sprintf("Found %d total papers. %d already downloaded.",
              nrow(papers), length(already_done)))

  to_download <- papers[!papers$id %in% already_done, ]
  n <- nrow(to_download)
  say(sprintf("Need to download %d PDFs.", n))

  # ---- progress bar ----------------------------------------------------------

  show_pb <- verbose && interactive()
  if (show_pb) {
    pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  }

  # ---- download loop ---------------------------------------------------------

  for (i in seq_len(n)) {

    row     <- to_download[i, ]
    safe_id <- gsub("/", "_", row$id)
    fname   <- paste0(safe_id, ".pdf")
    fpath   <- file.path(download_path, fname)
    pdf_url <- paste0("https://export.arxiv.org/pdf/", row$id, ".pdf")

    req <- httr2::request(pdf_url) |>
      httr2::req_user_agent(
        "Academic_Downloader_Bot/1.0 (mailto:your.email@example.com)"
      ) |>
      httr2::req_retry(
        max_tries   = retries,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(429, 503)
        }
      ) |>
      httr2::req_throttle(rate = rate_limit)

    resp <- tryCatch(
      httr2::req_perform(req, path = fpath),
      error = function(e) {
        say(sprintf("[!] Permanent failure for %s: %s", row$id, e$message))
        NULL
      }
    )

    status <- if (!is.null(resp) && file.exists(fpath)) "ok" else "failed"

    entry <- data.frame(
      arxiv_id  = row$id,
      file      = fname,
      status    = status,
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )

    # append to CSV safely
    write.table(
      entry,
      log_path,
      append     = TRUE,
      sep        = ",",
      col.names  = !file.exists(log_path),
      row.names  = FALSE
    )

    if (show_pb) utils::setTxtProgressBar(pb, i)
  }

  if (show_pb) close(pb)

  say("Done.")

  invisible(utils::read.csv(log_path, stringsAsFactors = FALSE))
}
