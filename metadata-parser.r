library(aRxiv)
library(jsonlite)

my_category <- "cs.CR"
total_count <- arxiv_count(query = paste0("cat:", my_category))
max_papers <- min(total_count, 10000)
batch_size <- 100
all_results <- data.frame()

cat("Enter path to download files: ")
download_path <- readline()
full_path <- file.path(download_path, "metadata.json")
temp_path <- file.path(download_path, "metadata_partial.json")  # our insurance policy

if (!dir.exists(download_path)) dir.create(download_path, recursive = TRUE)

# ── Helper: save whatever we have so far, no questions asked ──────────────────
save_progress <- function(df, path) {
  tryCatch({
    write(toJSON(df, pretty = TRUE), path)
  }, error = function(e) {
    warning("Tried to save progress, failed miserably: ", conditionMessage(e))
  })
}

# ── Resume logic ──────────────────────────────────────────────────────────────
if (file.exists(full_path)) {
  existing_metadata <- jsonlite::fromJSON(full_path)
  already_saved_count <- nrow(existing_metadata)
  cat("We already have", already_saved_count, "papers.\n")
  difference <- total_count - already_saved_count

  if (difference > 0) {
    cat("Fetching", difference, "missing papers.\n")
    all_results <- existing_metadata 
    pb <- txtProgressBar(min = 0, max = difference, style = 3)

    for (i in seq(0, difference - 1, by = batch_size)) {
      tryCatch({
        batch <- arxiv_search(
          query = paste0("cat:", my_category),
          start = already_saved_count + i,
          limit = batch_size,
          sort_by = "submitted",
          ascending = FALSE
        )
        all_results <- rbind(all_results, batch)
        setTxtProgressBar(pb, i + nrow(batch))

        save_progress(all_results, temp_path)
        Sys.sleep(1)

      }, error = function(e) {
        message("\nSomething broke at batch starting ", i, ": ", conditionMessage(e))
        message("Partial results saved to: ", temp_path_)

      })
      Sys.sleep(1)
    }

    close(pb)
    write(toJSON(all_results, pretty = TRUE), full_path)
    if (file.exists(temp_path)) file.remove(temp_path)
    message("Metadata updated. ", nrow(all_results), " papers total.")
    q(save = "no", status = 0)
  } else {
    message("Nothing new to fetch")
    q(save = "no", status = 0)
  }
}

# ── Fresh download ────────────────────────────────────────────────────────────
cat("Fresh start. Fetching up to", max_papers, "papers...\n")
pb <- txtProgressBar(min = 0, max = max_papers, style = 3)

for (i in seq(0, max_papers - 1, by = batch_size)) {
  tryCatch({
    batch <- arxiv_search(
      query = paste0("cat:", my_category),
      start = i,
      limit = batch_size,
      sort_by = "submitted",
      ascending = FALSE
    )
    all_results <- rbind(all_results, batch)
    setTxtProgressBar(pb, i + nrow(batch))

    # Save partial progress after every batch
    save_progress(all_results, temp_path)

  }, error = function(e) {
    message("\nCrashed at batch starting ", i, ": ", conditionMessage(e))
    message("Saved ", nrow(all_results), " papers to: ", temp_path)
  })
  Sys.sleep(1)
}

close(pb)
write(toJSON(all_results, pretty = TRUE), full_path)
if (file.exists(temp_path)) file.remove(temp_path)
cat("Done.", nrow(all_results), "papers saved to", full_path, "\n")

data <- fromJSON(full_path)