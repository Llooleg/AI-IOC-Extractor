library(jsonlite)
library(dplyr)
library(ggplot2)

# --- Загрузка ---
tfidf_raw <- read_json("D:/train-ml/full_tagged_classical.json", simplifyVector = FALSE)
llm_raw   <- read_json("D:/train-ml/full_tagged_context_n_tags.json", simplifyVector = FALSE)

llm_map   <- setNames(llm_raw,   sapply(llm_raw,   `[[`, "id"))
tfidf_map <- setNames(tfidf_raw, sapply(tfidf_raw, `[[`, "id"))

# --- Нормализация: строка или список → вектор токенов ---
parse_tags <- function(x) {
  raw <- unlist(x)
  if (is.null(raw) || length(raw) == 0) return(character(0))
  # Разбиваем по запятой (на случай "tag1, tag2" в одной строке)
  tags <- unlist(strsplit(raw, ",\\s*"))
  # Нормализация: lower, trim, дефисы/подчёркивания → пробел
  tags <- trimws(tolower(tags))
  tags <- gsub("[-_]", " ", tags)
  tags <- gsub("\\s+", " ", tags)
  unique(tags[nzchar(tags)])
}

jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

# --- Только ID с тегами в обоих источниках ---
common_ids <- intersect(names(llm_map), names(tfidf_map))

valid_ids <- common_ids[sapply(common_ids, function(id) {
  t1 <- parse_tags(llm_map[[id]]$tags)
  t2 <- parse_tags(tfidf_map[[id]]$tags)
  length(t1) > 0 && length(t2) > 0
})]

message(sprintf("Всего общих ID: %d", length(common_ids)))
message(sprintf("С тегами в обоих: %d", length(valid_ids)))

# --- Считаем метрики ---
results_df <- tibble(id = valid_ids) %>%
  rowwise() %>%
  mutate(
    llm_tags_vec   = list(parse_tags(llm_map[[id]]$tags)),
    tfidf_tags_vec = list(parse_tags(tfidf_map[[id]]$tags)),
    llm_ctx_vec    = list(parse_tags(llm_map[[id]]$context_tags)),
    tfidf_ctx_vec  = list(parse_tags(tfidf_map[[id]]$context_tags)),

    # Jaccard по основным тегам
    jaccard_tags   = jaccard(llm_tags_vec, tfidf_tags_vec),
    # Jaccard по context_tags
    jaccard_ctx    = jaccard(llm_ctx_vec,  tfidf_ctx_vec),
    # Хотя бы одно совпадение в context
    ctx_any_match  = any(llm_ctx_vec %in% tfidf_ctx_vec),

    llm_tags   = paste(llm_tags_vec,   collapse = ", "),
    tfidf_tags = paste(tfidf_tags_vec, collapse = ", "),
    llm_ctx    = paste(llm_ctx_vec,    collapse = ", "),
    tfidf_ctx  = paste(tfidf_ctx_vec,  collapse = ", ")
  ) %>%
  ungroup()

# --- Сводная статистика ---
message(sprintf("\n=== ОСНОВНЫЕ ТЕГИ ==="))
message(sprintf("Jaccard среднее:     %.3f", mean(results_df$jaccard_tags, na.rm=TRUE)))
message(sprintf("Jaccard медиана:     %.3f", median(results_df$jaccard_tags, na.rm=TRUE)))
message(sprintf("Совпадение > 0:      %.1f%%", mean(results_df$jaccard_tags > 0, na.rm=TRUE) * 100))
message(sprintf("Полное совпадение:   %.1f%%", mean(results_df$jaccard_tags == 1, na.rm=TRUE) * 100))

message(sprintf("\n=== CONTEXT TAGS ==="))
message(sprintf("Jaccard среднее:     %.3f", mean(results_df$jaccard_ctx, na.rm=TRUE)))
message(sprintf("Хотя бы 1 общий:     %.1f%%", mean(results_df$ctx_any_match, na.rm=TRUE) * 100))

# --- Худшие и лучшие случаи ---
cat("\n--- Худшие совпадения (tags) ---\n")
results_df %>% arrange(jaccard_tags) %>%
  select(id, jaccard_tags, llm_tags, tfidf_tags) %>%
  head(10) %>% print()

cat("\n--- Лучшие совпадения (tags) ---\n")
results_df %>% arrange(desc(jaccard_tags)) %>%
  select(id, jaccard_tags, llm_tags, tfidf_tags) %>%
  head(10) %>% print()

# --- Графики ---

# 1. Гистограмма Jaccard по тегам
ggplot(results_df, aes(x = jaccard_tags)) +
  geom_histogram(binwidth = 0.1, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  labs(title = "Jaccard — основные теги", x = "Jaccard Similarity", y = "Документов") +
  theme_minimal()
ggsave("jaccard_tags.png", width = 8, height = 5)

# 2. Гистограмма Jaccard по context_tags
ggplot(results_df, aes(x = jaccard_ctx)) +
  geom_histogram(binwidth = 0.1, fill = "#a37fb3", color = "#e9ecef", alpha = 0.9) +
  labs(title = "Jaccard — context tags", x = "Jaccard Similarity", y = "Документов") +
  theme_minimal()
ggsave("jaccard_ctx.png", width = 8, height = 5)

# 3. Scatter: теги vs context — видно общую картину сразу
ggplot(results_df, aes(x = jaccard_tags, y = jaccard_ctx)) +
  geom_point(alpha = 0.3, color = "#69b3a2") +
  geom_smooth(method = "lm", color = "tomato", se = FALSE) +
  labs(title = "Теги vs Context tags", x = "Jaccard (tags)", y = "Jaccard (context)") +
  theme_minimal()
ggsave("jaccard_scatter.png", width = 7, height = 6)