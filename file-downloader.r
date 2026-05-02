library(aRxiv)
library(httr2)

query <- 'cat:"cs.CR"'
batch_size <- 100
ratelimit <- 3
retries <- 5
logger <- "download_log.csv"
total <- 1000

cat("Enter path to download files: ")
download_path <- readline()
if (!dir.exists(download_path)) dir.create(download_path, recursive = TRUE)

all_results <- list()
start       <- 0
log_path <- file.path(download_path, "download_log.csv")
if (file.exists(log_path)) {
  log_df <- read.csv(log_path, stringsAsFactors = FALSE)
} else {
  log_df <- data.frame(arxiv_id = character(), 
                       file = character(), 
                       status = character(), 
                       stringsAsFactors = FALSE)
}
already_done <- log_df$arxiv_id[log_df$status == "ok"]
repeat {
  results <- tryCatch(
    arxiv_search(query = query, start = start, limit = batch_size,
                 sort_by = "submitted", ascending = FALSE),
    error = function(e) NULL
  )
  if (is.null(results) || nrow(results) == 0) break
  all_results <- append(all_results, list(results))
  start <- start + nrow(results)
  cat(sprintf("  Fetched %d papers so far...\r", start))
  if (nrow(results) < batch_size) break
  if (start >= total_limit) break
  Sys.sleep(1 / ratelimit)
}
papers <- do.call(rbind, all_results)
cat(sprintf("\nFound %d papers total. %d already downloaded. Starting...\n",
            nrow(papers), length(already_done)))
to_download <- papers[!papers$id %in% already_done, ]
total       <- nrow(to_download)

pb <- txtProgressBar(min = 0, max = nrow(to_download), style = 3)

for (i in seq_len(nrow(to_download))) {
  row     <- to_download[i, ]
  safe_id <- gsub("/", "_", row$id)
  fname   <- paste0(safe_id, ".pdf")
  fpath   <- file.path(download_path, fname)
  pdf_url <- paste0("https://export.arxiv.org/pdf/", row$id, ".pdf")
  
  req <- request(pdf_url) %>%
  req_user_agent("Academic_Downloader_Bot/1.0 (mailto:helgilebed@gmail.com)") %>%
    req_retry(
      max_tries = 5,
      is_transient = \(resp) resp_status(resp) %in% c(429, 503)
    ) %>%
    req_throttle(rate = 3)


  resp <- tryCatch({
    req_perform(req, path = fpath) 
  }, error = function(e) {
    message("\n[!] Permanent failure for ", row$id, ": ", e$message)
    NULL
  })

  status <- if (!is.null(resp) && file.exists(fpath)) "ok" else "failed"
  new_entry <- data.frame(
    arxiv_id = row$id,
    file = fname,
    status = status,
    timestamp = Sys.time()
  )
  write.table(
    new_entry,
    log_path,
    append = TRUE,
    sep = ",",
    col.names = !file.exists(log_path),
    row.names = FALSE
  )
  setTxtProgressBar(pb, i)
}
close(pb)
