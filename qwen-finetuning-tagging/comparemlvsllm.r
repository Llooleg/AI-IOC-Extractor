library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Загрузка ---
tfidf_raw <- read_json("D:/train-ml/full_tagged_classical.json",      simplifyVector = FALSE)
llm_raw   <- read_json("D:/train-ml/full_tagged_context_n_tags.json", simplifyVector = FALSE)

llm_map   <- setNames(llm_raw,   sapply(llm_raw,   `[[`, "id"))
tfidf_map <- setNames(tfidf_raw, sapply(tfidf_raw, `[[`, "id"))

# --- Нормализация тегов ---
parse_tags <- function(x) {
  raw <- unlist(x)
  if (is.null(raw) || length(raw) == 0) return(character(0))
  tags <- unlist(strsplit(raw, ",\\s*"))
  tags <- trimws(tolower(tags))
  tags <- gsub("[-_/]", " ", tags)
  tags <- gsub("\\s*\\(.*?\\)\\s*", " ", tags)
  tags <- gsub("\\s*\\(.*",         " ", tags)
  tags <- gsub("[^a-zа-яё0-9 ]",    "",  tags)
  tags <- gsub("\\s+", " ", tags)
  tags <- trimws(tags)
  unique(tags[nzchar(tags)])
}

# --- Таксономия ---
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
# Категории сами себе родители
raw_parent_map[names(TAG_TREE)] <- names(TAG_TREE)

parent_map_norm <- setNames(
  as.character(raw_parent_map),
  sapply(names(raw_parent_map), function(k) parse_tags(k)[1])
)

# Нормализованные имена категорий (для проверки)
category_names_norm <- sapply(names(TAG_TREE), function(k) parse_tags(k)[1])

# --- Привести теги к уровню родительских категорий ---
to_parent_level <- function(tags, pmap) {
  result <- sapply(tags, function(t) {
    p <- pmap[t]
    if (!is.na(p)) p else NA_character_   # тег вне таксономии → NA
  }, USE.NAMES = FALSE)
  unique(tolower(result[!is.na(result)]))
}

# --- Метрики ---
precision_fn <- function(pred, ref) {
  if (length(pred) == 0) return(NA_real_)
  length(intersect(pred, ref)) / length(pred)
}

recall_fn <- function(pred, ref) {
  if (length(ref) == 0) return(NA_real_)
  length(intersect(pred, ref)) / length(ref)
}

f1_fn <- function(pred, ref) {
  p <- precision_fn(pred, ref)
  r <- recall_fn(pred, ref)
  if (is.na(p) || is.na(r) || (p + r) == 0) return(0)
  2 * p * r / (p + r)
}

# --- Валидные ID ---
common_ids <- intersect(names(llm_map), names(tfidf_map))

valid_ids <- common_ids[sapply(common_ids, function(id) {
  t1 <- to_parent_level(parse_tags(llm_map[[id]]$tags),   parent_map_norm)
  t2 <- to_parent_level(parse_tags(tfidf_map[[id]]$tags), parent_map_norm)
  length(t1) > 0 && length(t2) > 0
})]

message(sprintf("Всего общих ID: %d", length(common_ids)))
message(sprintf("После нормализации к категориям: %d", length(valid_ids)))
# --- Новые метрики ---

# Есть ли хотя бы одно совпадение (бинарный "зачёт")
any_hit_fn <- function(pred, ref) {
  as.integer(length(intersect(pred, ref)) > 0)
}

# Partial credit: доля совпавших тегов от МЕНЬШЕГО из двух наборов
# (не наказывает за "лишние" теги в большем наборе)
partial_credit_fn <- function(pred, ref) {
  n_inter <- length(intersect(pred, ref))
  denom   <- min(length(pred), length(ref))
  if (denom == 0) return(NA_real_)
  n_inter / denom
}

# Recall LLM→TF-IDF: сколько LLM-тегов нашлось в TF-IDF
# (твой кейс: 3 у LLM, 1 у TF-IDF, совпал 1 → recall = 1/3, но any_hit = 1)
recall_llm_fn <- function(llm, tfidf) {
  if (length(llm) == 0) return(NA_real_)
  length(intersect(llm, tfidf)) / length(llm)
}
# --- Основные метрики ---
results_df <- tibble(id = valid_ids) %>%
  rowwise() %>%
  mutate(
    llm_raw   = list(parse_tags(llm_map[[id]]$tags)),
    tfidf_raw = list(parse_tags(tfidf_map[[id]]$tags)),

    # Оба приведены к родительским категориям
    llm_cats   = list(to_parent_level(llm_raw,   parent_map_norm)),
    tfidf_cats = list(to_parent_level(tfidf_raw, parent_map_norm)),

    # Количество категорий у каждого
    n_llm   = length(llm_cats),
    n_tfidf = length(tfidf_cats),

    # Метрики согласия (симметричные — просто насколько совпадают)
    precision = precision_fn(llm_cats, tfidf_cats),
    recall    = recall_fn(llm_cats,    tfidf_cats),
    f1        = f1_fn(llm_cats,        tfidf_cats),

    # Jaccard оставим для справки — теперь он корректен, т.к. уровни совпадают
    jaccard   = length(intersect(llm_cats, tfidf_cats)) /
                length(union(llm_cats, tfidf_cats)),

    # Первая категория каждого — для confusion matrix
    llm_top   = llm_cats[1],
    tfidf_top = tfidf_cats[1],
    any_hit       = any_hit_fn(llm_cats, tfidf_cats),
    partial_credit = partial_credit_fn(llm_cats, tfidf_cats),
    recall_llm    = recall_llm_fn(llm_cats, tfidf_cats),
    llm_str   = paste(llm_cats,   collapse = ", "),
    tfidf_str = paste(tfidf_cats, collapse = ", ")
  ) %>%
  ungroup()

# --- Статистика ---
message("\n=== СОГЛАСИЕ LLM vs TF-IDF (уровень категорий) ===")
message(sprintf("Документов в анализе:       %d", nrow(results_df)))
message(sprintf("Среднее кат. у LLM:         %.2f", mean(results_df$n_llm)))
message(sprintf("Среднее кат. у TF-IDF:      %.2f", mean(results_df$n_tfidf)))
message(sprintf(""))
message(sprintf("Precision среднее:          %.3f", mean(results_df$precision, na.rm=TRUE)))
message(sprintf("Recall среднее:             %.3f", mean(results_df$recall,    na.rm=TRUE)))
message(sprintf("F1 среднее:                 %.3f", mean(results_df$f1,        na.rm=TRUE)))
message(sprintf("F1 медиана:                 %.3f", median(results_df$f1,      na.rm=TRUE)))
message(sprintf("Jaccard среднее:            %.3f", mean(results_df$jaccard,   na.rm=TRUE)))
message(sprintf(""))
message(sprintf("Полное совпадение (F1=1):   %.1f%%", mean(results_df$f1 == 1,  na.rm=TRUE) * 100))
message(sprintf("Нет совпадений  (F1=0):     %.1f%%", mean(results_df$f1 == 0,  na.rm=TRUE) * 100))

message("\n=== ДИАГНОСТИКА ===")
message(sprintf("LLM даёт больше категорий:  %.1f%%",
  mean(results_df$n_llm > results_df$n_tfidf) * 100))
message(sprintf("TF-IDF даёт больше:         %.1f%%",
  mean(results_df$n_tfidf > results_df$n_llm) * 100))
message(sprintf("Одинаково:                  %.1f%%",
  mean(results_df$n_llm == results_df$n_tfidf) * 100))
message(sprintf("Any-hit (хоть 1 совпадение): %.1f%%",
  mean(results_df$any_hit, na.rm = TRUE) * 100))
message(sprintf("Partial credit среднее:      %.3f",
  mean(results_df$partial_credit, na.rm = TRUE)))
message(sprintf("Recall LLM→TF-IDF среднее:  %.3f",
  mean(results_df$recall_llm, na.rm = TRUE)))
# --- Графики ---

# 1. F1 распределение
ggplot(results_df, aes(x = f1)) +
  geom_histogram(binwidth = 0.1, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Согласие LLM и TF-IDF по категориям (F1)",
       subtitle = "1.0 = полное совпадение, 0 = полное расхождение",
       x = "F1", y = "Документов") +
  theme_minimal()
ggsave("f1_agreement.png", width = 8, height = 5)

# 2. Precision vs Recall
ggplot(results_df, aes(x = recall, y = precision)) +
  geom_point(alpha = 0.2, color = "#69b3a2") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Precision vs Recall",
       subtitle = "Выше диагонали = LLM 'уже' (меньше категорий), ниже = 'шире'",
       x = "Recall (TF-IDF покрытие)", y = "Precision (LLM точность)") +
  theme_minimal()
ggsave("precision_recall.png", width = 7, height = 6)

# 3. Количество категорий: LLM vs TF-IDF
results_df %>%
  select(n_llm, n_tfidf) %>%
  pivot_longer(everything(), names_to = "method", values_to = "n_cats") %>%
  mutate(method = recode(method, n_llm = "LLM", n_tfidf = "TF-IDF")) %>%
  ggplot(aes(x = n_cats, fill = method)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_fill_manual(values = c("LLM" = "#69b3a2", "TF-IDF" = "#a37fb3")) +
  labs(title = "Сколько категорий назначает каждый метод",
       x = "Количество категорий", y = "Документов", fill = NULL) +
  theme_minimal()
ggsave("n_categories.png", width = 8, height = 5)

# 4. Confusion matrix

# Разворачиваем все категории каждого документа в пары
confusion_full <- results_df %>%
  select(id, llm_cats, tfidf_cats) %>%
  rowwise() %>%
  mutate(
    # Все комбинации тегов для документа
    pairs = list(expand.grid(
      llm   = llm_cats,
      tfidf = tfidf_cats,
      stringsAsFactors = FALSE
    ))
  ) %>%
  ungroup() %>%
  select(pairs) %>%
  tidyr::unnest(pairs)

confusion_agg <- confusion_full %>%
  count(llm, tfidf) %>%
  group_by(tfidf) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(confusion_agg, aes(x = tfidf, y = llm, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%d\n%.0f%%", n, pct * 100)),
            size = 2.8, color = "white") +
  scale_fill_gradient(low = "#2d2d2d", high = "#69b3a2",
                      labels = scales::percent) +
  labs(
    title    = "Где методы расходятся (все категории)",
    subtitle = "Все пары LLM×TF-IDF тегов; % от столбца TF-IDF",
    x = "TF-IDF категория", y = "LLM категория", fill = "Доля"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("confusion_matrix.png", width = 10, height = 7)

# 5. Jaccard vs F1 (для справки — должны коррелировать)
ggplot(results_df, aes(x = jaccard, y = f1)) +
  geom_point(alpha = 0.2, color = "#5b9bd5") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Jaccard vs F1 (после нормализации)",
       subtitle = "Должны совпадать когда n_llm == n_tfidf",
       x = "Jaccard", y = "F1") +
  theme_minimal()
ggsave("jaccard_vs_f1.png", width = 7, height = 6)
