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
normalize_key <- function(x) {
  x <- trimws(tolower(x))
  x <- gsub("[-_]", " ", x)
  x <- gsub("\\s+", " ", x)
  # Убираем неполные скобки (артефакт обрезки строки)
  x <- gsub("\\s*\\(.*", "", x)  # всё после "(" 
  trimws(x)
}

parent_map_norm <- setNames(
  as.character(parent_map),
  normalize_key(names(parent_map))
)

lookup_tag <- function(tag) {
  t <- normalize_key(tag)
  unname(parent_map_norm[t])
}

results_df <- results_df %>%
  rowwise() %>%
  mutate(
    llm_tag1     = parse_tags(llm_map[[id]]$tags)[1],
    tfidf_tag1   = parse_tags(tfidf_map[[id]]$tags)[1],
    llm_parent   = lookup_tag(llm_tag1),
    tfidf_parent = lookup_tag(tfidf_tag1),
    parent_match = isTRUE(llm_parent == tfidf_parent)
  ) %>%
  ungroup()

cat(sprintf("Совпадение родительской категории: %.1f%%\n",
            mean(results_df$parent_match, na.rm=TRUE) * 100))
cat(sprintf("LLM теги не в таксономии: %.1f%%\n",
            mean(is.na(results_df$llm_parent)) * 100))
cat(sprintf("TF-IDF теги не в таксономии: %.1f%%\n",
            mean(is.na(results_df$tfidf_parent)) * 100))
cat(sprintf("LLM теги не в таксономии: %.1f%%\n",
            mean(is.na(results_df$llm_parent)) * 100))
cat(sprintf("TF-IDF теги не в таксономии: %.1f%%\n",
            mean(is.na(results_df$tfidf_parent)) * 100))

