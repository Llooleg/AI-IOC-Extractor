library(jsonlite)
library(dplyr)
library(ggplot2)

# --- Загрузка ---
tfidf_raw <- read_json("D:/train-ml/full_tagged_classical.json", simplifyVector = FALSE)
llm_raw   <- read_json("D:/train-ml/full_tagged_context_n_tags.json", simplifyVector = FALSE)

llm_map   <- setNames(llm_raw,   sapply(llm_raw,   `[[`, "id"))
tfidf_map <- setNames(tfidf_raw, sapply(tfidf_raw, `[[`, "id"))

# --- Нормализация ---
parse_tags <- function(x) {
  raw <- unlist(x)
  if (is.null(raw) || length(raw) == 0) return(character(0))
  tags <- unlist(strsplit(raw, ",\\s*"))
  tags <- trimws(tolower(tags))
  tags <- gsub("[-_/]", " ", tags)          # дефисы, подчёркивания, слэши → пробел
  tags <- gsub("\\s*\\(.*?\\)\\s*", " ", tags)  # убираем скобки с содержимым: (AES) → ""
  tags <- gsub("\\s*\\(.*",         " ", tags)  # незакрытые скобки: (sgx → ""
  tags <- gsub("[^a-zа-яё0-9 ]",    "",  tags)  # всё остальное кроме букв/цифр/пробела
  tags <- gsub("\\s+", " ", tags)
  tags <- trimws(tags)
  unique(tags[nzchar(tags)])
}

jaccard <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  length(intersect(a, b)) / length(union(a, b))
}

# --- parent_map с той же нормализацией ---
TAG_TREE <- list(
  "Криптография" = c(
    "Симметричное шифрование (AES, блочные шифры)",
    "Асимметричное шифрование, PKI",
    "Постквантовая криптография",
    "Zero-knowledge proofs",
    "Гомоморфное шифрование",
    "Протоколы MPC"
  ),
  "Безопасность ML-систем" = c(
    "Adversarial attacks",
    "Отравление данных (data poisoning)",
    "Differential privacy",
    "Federated learning + privacy",
    "Membership inference attacks",
    "Model inversion attacks"
  ),
  "Сетевая безопасность" = c(
    "Обнаружение вторжений (IDS/IPS)",
    "DDoS, сетевые атаки",
    "Firewall, VPN, SD-WAN"
  ),
  "Анализ уязвимостей" = c(
    "Fuzzing, статический анализ",
    "Эксплойты, CVE-анализ",
    "Side-channel attacks"
  ),
  "Методы атак" = c(
    "Prompt injection / LLM-атаки",
    "Социальная инженерия, фишинг"
  ),
  "Приватность" = c(
    "Анонимизация данных",
    "GDPR / соответствие нормативам",
    "Приватность в IoT"
  ),
  "Блокчейн и криптовалюты" = c(
    "Безопасность смарт-контрактов и DeFi",
    "Консенсус-протоколы"
  ),
  "Системная безопасность" = c(
    "TEE / доверенное исполнение (SGX, TrustZone)",
    "ОС-безопасность, гипервизоры",
    "Hardware security"
  ),
  "Встроенные и киберфизические системы" = c(
    "IoT / CPS / автономные системы",
    "Промышленные системы (ICS/SCADA)",
    "Биометрия"
  )
)

raw_parent_map <- setNames(
  rep(names(TAG_TREE), sapply(TAG_TREE, length)),
  unlist(TAG_TREE)
)
raw_parent_map[names(TAG_TREE)] <- names(TAG_TREE)

# Нормализуем ключи тем же parse_tags
parent_map_norm <- setNames(
  as.character(raw_parent_map),
  sapply(names(raw_parent_map), function(k) parse_tags(k)[1])
)

lookup_parent <- function(tag) {
  unname(parent_map_norm[tag])
}

# --- Валидные ID ---
common_ids <- intersect(names(llm_map), names(tfidf_map))

valid_ids <- common_ids[sapply(common_ids, function(id) {
  t1 <- parse_tags(llm_map[[id]]$tags)
  t2 <- parse_tags(tfidf_map[[id]]$tags)
  length(t1) > 0 && length(t2) > 0
})]

message(sprintf("Всего общих ID: %d", length(common_ids)))
message(sprintf("С тегами в обоих: %d", length(valid_ids)))

# --- Метрики ---
results_df <- tibble(id = valid_ids) %>%
  rowwise() %>%
  mutate(
    llm_tags_vec   = list(parse_tags(llm_map[[id]]$tags)),
    tfidf_tags_vec = list(parse_tags(tfidf_map[[id]]$tags)),
    llm_ctx_vec    = list(parse_tags(llm_map[[id]]$context_tags)),
    tfidf_ctx_vec  = list(parse_tags(tfidf_map[[id]]$context_tags)),

    jaccard_tags  = jaccard(llm_tags_vec, tfidf_tags_vec),
    jaccard_ctx   = jaccard(llm_ctx_vec,  tfidf_ctx_vec),
    ctx_any_match = any(llm_ctx_vec %in% tfidf_ctx_vec),

    llm_tag1     = llm_tags_vec[1],
    tfidf_tag1   = tfidf_tags_vec[1],
    llm_parent   = lookup_parent(llm_tag1),
    tfidf_parent = lookup_parent(tfidf_tag1),
    parent_match = isTRUE(llm_parent == tfidf_parent),

    llm_tags   = paste(llm_tags_vec,   collapse = ", "),
    tfidf_tags = paste(tfidf_tags_vec, collapse = ", "),
    llm_ctx    = paste(llm_ctx_vec,    collapse = ", "),
    tfidf_ctx  = paste(tfidf_ctx_vec,  collapse = ", ")
  ) %>%
  ungroup()

# --- Статистика ---
message("\n=== ОСНОВНЫЕ ТЕГИ ===")
message(sprintf("Jaccard среднее:            %.3f", mean(results_df$jaccard_tags, na.rm=TRUE)))
message(sprintf("Jaccard медиана:            %.3f", median(results_df$jaccard_tags, na.rm=TRUE)))
message(sprintf("Совпадение > 0:             %.1f%%", mean(results_df$jaccard_tags > 0, na.rm=TRUE) * 100))
message(sprintf("Полное совпадение:          %.1f%%", mean(results_df$jaccard_tags == 1, na.rm=TRUE) * 100))

message("\n=== CONTEXT TAGS ===")
message(sprintf("Jaccard среднее:            %.3f", mean(results_df$jaccard_ctx, na.rm=TRUE)))
message(sprintf("Хотя бы 1 общий:            %.1f%%", mean(results_df$ctx_any_match, na.rm=TRUE) * 100))

message("\n=== РОДИТЕЛЬСКИЕ КАТЕГОРИИ ===")
message(sprintf("Совпадение:                 %.1f%%", mean(results_df$parent_match, na.rm=TRUE) * 100))
message(sprintf("LLM вне таксономии:         %.1f%%", mean(is.na(results_df$llm_parent)) * 100))
message(sprintf("TF-IDF вне таксономии:      %.1f%%", mean(is.na(results_df$tfidf_parent)) * 100))

# --- Графики ---
ggplot(results_df, aes(x = jaccard_tags)) +
  geom_histogram(binwidth = 0.1, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  labs(title = "Jaccard — основные теги", x = "Jaccard Similarity", y = "Документов") +
  theme_minimal()
ggsave("jaccard_tags.png", width = 8, height = 5)

ggplot(results_df, aes(x = jaccard_ctx)) +
  geom_histogram(binwidth = 0.1, fill = "#a37fb3", color = "#e9ecef", alpha = 0.9) +
  labs(title = "Jaccard — context tags", x = "Jaccard Similarity", y = "Документов") +
  theme_minimal()
ggsave("jaccard_ctx.png", width = 8, height = 5)

ggplot(results_df, aes(x = jaccard_tags, y = jaccard_ctx)) +
  geom_point(alpha = 0.3, color = "#69b3a2") +
  geom_smooth(method = "lm", color = "tomato", se = FALSE) +
  labs(title = "Теги vs Context tags", x = "Jaccard (tags)", y = "Jaccard (context)") +
  theme_minimal()
ggsave("jaccard_scatter.png", width = 7, height = 6)

confusion <- results_df %>%
  filter(!is.na(llm_parent), !is.na(tfidf_parent)) %>%
  count(llm_parent, tfidf_parent)

ggplot(confusion, aes(x = tfidf_parent, y = llm_parent, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), size = 3, color = "white") +
  scale_fill_gradient(low = "#2d2d2d", high = "#69b3a2") +
  labs(title = "Где методы расходятся", x = "TF-IDF категория", y = "LLM категория") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("confusion_parents.png", width = 10, height = 8)