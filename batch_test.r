
library(aRxiv)
library(jsonlite)
batch_size <- 100
my_category <- "cs.CR"
already_saved_count <- 10000
batch <- arxiv_search(
          query = paste0("cat:", my_category),
          start = already_saved_count,
          limit = batch_size,
          sort_by = "submitted",
          ascending = FALSE
        )
print(batch)