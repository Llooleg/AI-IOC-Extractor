library(jsonlite)


tfidif <- read_json("D:/train-ml/full_tagged_classical.json", 
                   simplifyVector = FALSE)
llm  <- read_json("D:/train-ml/tagged.json",
                   simplifyVector = FALSE)

llm_map   <- setNames(llm,   sapply(llm,   `[[`, "id"))
tfidf_map <- setNames(tfidif, sapply(tfidif, `[[`, "id"))

common_ids <- intersect(names(llm_map), names(tfidf_map))

tag_overlap <- sapply(common_ids, function(id) {
  t1 <- unlist(llm_map[[id]]$tag)
  t2 <- unlist(tfidf_map[[id]]$tags)
  length(intersect(t1, t2)) / length(union(t1, t2))  # Jaccard
})

ctx_match <- sapply(common_ids, function(id) {
  c1 <- unlist(llm_map[[id]]$context_tags)
  c2 <- unlist(tfidf_map[[id]]$context_tags)
  any(c1 %in% c2)
})

message(sprintf("Jaccard по тегам (среднее):     %.3f", mean(tag_overlap)))
message(sprintf("Context совпал хотя бы 1 тег:  %.1f%%", mean(ctx_match) * 100))

worst <- common_ids[order(tag_overlap)][1:20]
for (id in worst) {
  cat(sprintf("\n%s\n  LLM:   %s\n  TFIDF: %s\n",
              id,
              paste(unlist(llm_map[[id]]$tag), collapse=" | "),
              paste(unlist(tfidf_map[[id]]$tags), collapse=" | ")))
}