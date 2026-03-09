library(aRxiv)
library(jsonlite)
my_category <- "cs.CR"
max_papers <- 50
# batch_size <- 100
total_count <- arxiv_count(query = paste0("cat:", my_category))
message(paste("Total papers in", my_category, ":", total_count))


metadata <- arxiv_search(
  query = paste0("cat:", my_category),
  limit = max_papers,
  sort_by = "submitted",
  ascending = FALSE
)


print(toJSON(head(metadata)))

write.csv(metadata, "arxiv_metadata.csv", row.names = FALSE)