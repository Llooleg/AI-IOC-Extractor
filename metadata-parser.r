library(aRxiv)
library(jsonlite)
my_category <- "cs.CR"
total_count <- arxiv_count(query = paste0("cat:", my_category))
max_papers <- min(total_count, 5000)
batch_size <- 100
all_results <- data.frame()
cat("Enter path to download files: ")
download_path <- readline()
full_path <- file.path(download_path, "metadata.json")
if (!dir.exists(download_path)) dir.create(download_path, recursive = TRUE)
if (file.exists(full_path)) {
  existing_metadata <- jsonlite::fromJSON(full_path)
  already_saved_count <- nrow(existing_metadata)

  cat("We already have", already_saved_count, "papers.\n")

  fresh_data <- arxiv_search(
    query = paste0("cat:", my_category),
    start = already_saved_count,
    limit = 10,
    sort_by = "submitted",
    ascending = FALSE
  )

  new_papers <- fresh_data[!(fresh_data$id %in% existing_metadata$id), ]

  if (nrow(new_papers) > 0) {
    message(sprintf("Adding %d new entries...", nrow(new_papers)))

    combined <- rbind(existing_metadata, new_papers)

    write_json(combined, full_path, pretty = TRUE)

    cat("File updated. Current row count:", nrow(combined), "\n")
    message("Metadata updated. Exiting.")
    q(save = "no", status = 0)
  } else {
    message("Data is up to date. Exiting.")
    q(save = "no", status = 0)
  }
}

pb <- txtProgressBar(min = 0, max = max_papers, style = 3)

for (i in seq(0, max_papers - 1, by = batch_size)) {
  batch <- arxiv_search(
    query = paste0("cat:", my_category),
    start = i,
    limit = batch_size,
    sort_by = "submitted",
    ascending = FALSE
  )
  all_results <- rbind(all_results, batch)
  setTxtProgressBar(pb, i + nrow(batch))
  Sys.sleep(1)
}

close(pb)

json_data <- toJSON(all_results, pretty = TRUE)
write(json_data, file.path(download_path, "metadata.json"))
data <- fromJSON(file.path(download_path, "metadata.json"))
