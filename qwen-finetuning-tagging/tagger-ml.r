# =====================================================================
# retag_nulls.R
# Резервный TF-IDF классификатор для статей с тегом "null"
# Берёт уже обработанный датасет, переклассифицирует только null-записи
# =====================================================================

library(jsonlite)
library(text2vec)
library(Matrix)

# ── настройки ────────────────────────────────────────────────────────
# Входной файл — результат работы первого классификатора
INPUT_FILE  <- "D:train-ml/full_tagged_context_n_tags.json"
# Выходной файл — тот же датасет, но с заполненными null-тегами
OUTPUT_FILE <- "D:train-ml/full_tagged_final1.json"

NULL_TAG <- "Криптография"

# ── keyword-словари (те же что в train-ml.r) ─────────────────────────
TAG_KEYWORDS <- list(
  "Симметричное шифрование (AES, блочные шифры)" =
    "AES block cipher symmetric encryption DES ChaCha20 mode ECB CBC GCM schedule",
  "Асимметричное шифрование, PKI" =
    "RSA public  infrastructure asymmetric elliptic curve certificate PKI TLS",
  "Постквантовая криптография" =
    "post-quantum lattice NTRU Kyber Dilithium NIST quantum resistant cryptography",
  "Zero-knowledge proofs" =
    "zero knowledge proof ZKP zk-SNARK zk-STARK verifiable computation commitment scheme",
  "Гомоморфное шифрование" =
    "homomorphic encryption FHE CKKS BFV BGV computation on encrypted data",
  "Протоколы MPC" =
    "multiparty computation MPC secret sharing garbled circuit oblivious transfer",
  "Adversarial attacks" =
    "adversarial example perturbation robustness attack neural network evasion",
  "Отравление данных (data poisoning)" =
    "data poisoning backdoor trojan training data manipulation label flipping",
  "Differential privacy" =
    "differential privacy noise mechanism epsilon delta privacy guarantee",
  "Federated learning + privacy" =
    "federated learning privacy distributed training aggregation gradient",
  "Membership inference attacks" =
    "membership inference attack shadow model training data privacy leakage",
  "Model inversion attacks" =
    "model inversion reconstruction attack privacy gradient leakage",
  "Обнаружение вторжений (IDS/IPS)" =
    "intrusion detection system IDS IPS anomaly network traffic classification",
  "DDoS, сетевые атаки" =
    "DDoS distributed denial of service flooding botnet amplification network attack",
  "Firewall, VPN, SD-WAN" =
    "firewall VPN SD-WAN network policy filtering tunnel access control",
  "Fuzzing, статический анализ" =
    "fuzzing fuzz testing static analysis code vulnerability taint AFL",
  "Эксплойты, CVE-анализ" =
    "exploit CVE vulnerability buffer overflow RCE privilege escalation patch",
  "Side-channel attacks" =
    "side channel timing power analysis cache covert channel Spectre Meltdown",
  "Prompt injection / LLM-атаки" =
    "prompt injection LLM jailbreak adversarial prompt language model attack",
  "Социальная инженерия, фишинг" =
    "phishing social engineering spear phishing email fraud impersonation",
  "Анонимизация данных" =
    "anonymization k-anonymity l-diversity data de-identification pseudonymization",
  "GDPR / соответствие нормативам" =
    "GDPR compliance regulation privacy law data protection CCPA policy",
  "Приватность в IoT" =
    "IoT privacy sensor data collection smart device privacy home",
  "Безопасность смарт-контрактов и DeFi" =
    "smart contract Solidity DeFi reentrancy vulnerability blockchain audit Ethereum",
  "Консенсус-протоколы" =
    "consensus protocol proof of work stake Byzantine fault tolerant blockchain",
  "TEE / доверенное исполнение (SGX, TrustZone)" =
    "trusted execution environment SGX TrustZone enclave TEE remote attestation",
  "ОС-безопасность, гипервизоры" =
    "operating system kernel hypervisor VM sandbox privilege separation security",
  "Hardware security" =
    "hardware security TPM secure boot FPGA tamper physical unclonable function",
  "IoT / CPS / автономные системы" =
    "IoT cyber physical system autonomous embedded firmware attack CPS",
  "Промышленные системы (ICS/SCADA)" =
    "industrial control system SCADA ICS PLC MODBUS critical infrastructure",
  "Биометрия" =
    "biometric fingerprint face recognition iris speaker authentication spoofing"
)

CONTEXT_KEYWORDS <- list(
  "attack"    = "attack propose novel adversarial exploit offensive new method",
  "defense"   = "defense mitigation protection countermeasure secure system prevent",
  "analysis"  = "analyze evaluate measurement study existing protocol system CVE",
  "survey"    = "survey review systematization literature overview state of the art",
  "formal"    = "formal proof theorem model verification mathematical security reduction",
  "benchmark" = "benchmark comparison evaluate performance baseline dataset metric",
  "framework" = "framework architecture system tool platform propose design build",
  "other"     = "discuss perspective position opinion challenge open problem"
)

# ── вспомогательная: проверить, является ли запись "null" ─────────────
is_null_tagged <- function(entry) {
  tags <- entry$tags
  # Поддерживаем разные форматы хранения null-тега:
  # ["null"], [null], [], NULL, или единственный тег == NULL_TAG
  if (is.null(tags) || length(tags) == 0) return(TRUE)
  if (length(tags) == 1 && tolower(as.character(tags[[1]])) == tolower(NULL_TAG)) return(TRUE)
  return(FALSE)
}

# ── TF-IDF engine (без изменений из оригинала) ────────────────────────
build_vectorizer <- function(all_docs) {
  it    <- itoken(all_docs, preprocessor = tolower,
                  tokenizer = word_tokenizer, progressbar = FALSE)
  vocab <- create_vocabulary(it, stopwords = stopwords::stopwords("en"))
  vocab <- prune_vocabulary(vocab, term_count_min = 1L)
  vocab_vectorizer(vocab)
}

transform_docs <- function(docs, vectorizer, tfidf_model) {
  it  <- itoken(docs, preprocessor = tolower,
                tokenizer = word_tokenizer, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  transform(dtm, tfidf_model)
}

# ── main ──────────────────────────────────────────────────────────────
main <- function() {
  message("Грузим обработанный датасет...")
  articles <- fromJSON(INPUT_FILE, simplifyVector = FALSE)

  # fromJSON может вернуть список или data.frame — нормализуем
  if (is.data.frame(articles)) {
    articles <- lapply(seq_len(nrow(articles)), function(i) as.list(articles[i, ]))
  }
  message("  Всего записей: ", length(articles))

  # ── фильтрация null-записей ───────────────────────────────────────
  null_idx <- which(sapply(articles, is_null_tagged))
  message(sprintf("  Из них с тегом null: %d (%.1f%%)",
                  length(null_idx),
                  100 * length(null_idx) / length(articles)))

  if (length(null_idx) == 0) {
    message("Нечего переклассифицировать. Иди пей кофе.")
    return(invisible(NULL))
  }

  null_articles <- articles[null_idx]

  null_abstracts <- sapply(null_articles, function(a)
    trimws(if (!is.null(a$abstract)) a$abstract else ""))

  nonempty <- nchar(null_abstracts) > 0
  message(sprintf("  Пустых абстрактов среди null: %d", sum(!nonempty)))

  if (sum(nonempty) == 0) {
    message("Все null-записи с пустыми абстрактами. Классифицировать нечего, поздравляю.")
    return(invisible(NULL))
  }

  # ── строим TF-IDF на ВСЁМ корпусе ────────────────────────────────
  # IDF считается по всему корпусу — иначе веса будут другими
  # и косинусное сходство с keyword-векторами поедет
  all_abstracts <- sapply(articles, function(a)
    trimws(if (!is.null(a$abstract)) a$abstract else ""))

  all_keyword_docs   <- c(unlist(TAG_KEYWORDS), unlist(CONTEXT_KEYWORDS))
  all_docs_for_vocab <- c(all_abstracts[nchar(all_abstracts) > 0], all_keyword_docs)

  message("Строим TF-IDF словарь на полном корпусе...")
  vectorizer  <- build_vectorizer(all_docs_for_vocab)

  # Фитим модель
  it_all      <- itoken(all_docs_for_vocab, preprocessor = tolower,
                        tokenizer = word_tokenizer, progressbar = FALSE)
  dtm_all     <- create_dtm(it_all, vectorizer)
  tfidf_model <- TfIdf$new()
  fit_transform(dtm_all, tfidf_model)  # side-effect: модель fitted

  message("Трансформируем keyword-векторы...")
  tag_names     <- names(TAG_KEYWORDS)
  context_names <- names(CONTEXT_KEYWORDS)
  tag_vecs      <- transform_docs(unlist(TAG_KEYWORDS),     vectorizer, tfidf_model)
  context_vecs  <- transform_docs(unlist(CONTEXT_KEYWORDS), vectorizer, tfidf_model)

  tag_norms     <- sqrt(rowSums(tag_vecs^2))
  context_norms <- sqrt(rowSums(context_vecs^2))

  message("Трансформируем null-абстракты батчем...")
  abs_matrix <- transform_docs(null_abstracts[nonempty], vectorizer, tfidf_model)
  abs_norms  <- sqrt(rowSums(abs_matrix^2))

  tag_scores     <- as.matrix(abs_matrix %*% t(tag_vecs)) /
                     outer(abs_norms, tag_norms + 1e-10)
  context_scores <- as.matrix(abs_matrix %*% t(context_vecs)) /
                     outer(abs_norms, context_norms + 1e-10)

  colnames(tag_scores)     <- tag_names
  colnames(context_scores) <- context_names

  message("Патчим null-записи...")
  nonempty_null_idx <- null_idx[nonempty]   # индексы в исходном articles[]
  replaced_count <- 0

  for (i in seq_len(nrow(tag_scores))) {
    ts <- tag_scores[i, ]
    cs <- context_scores[i, ]

    tag_order <- order(ts, decreasing = TRUE)
    top_tags  <- tag_names[tag_order[ts[tag_order] > 0.05]]
    if (length(top_tags) == 0) top_tags <- tag_names[tag_order[1]]
    if (length(top_tags) > 3)  top_tags <- top_tags[1:3]

    ctx_order <- order(cs, decreasing = TRUE)
    top_ctx   <- context_names[ctx_order[1]]
    if (!is.na(cs[ctx_order[1]]) && cs[ctx_order[1]] > 0 &&
        !is.na(cs[ctx_order[2]]) && cs[ctx_order[2]] > 0.7 * cs[ctx_order[1]])
      top_ctx <- c(top_ctx, context_names[ctx_order[2]])

    orig_idx <- nonempty_null_idx[i]
    articles[[orig_idx]]$tags         <- top_tags
    articles[[orig_idx]]$context_tags <- top_ctx
    articles[[orig_idx]]$tagged_by    <- "tfidf_fallback"   # маркер метода
    replaced_count <- replaced_count + 1
  }

  empty_null_idx <- null_idx[!nonempty]
  for (idx in empty_null_idx) {
    id <- articles[[idx]]$id
    message(sprintf("  Пропущено (пустой абстракт): %s", id))
  }

  dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
  write(toJSON(articles, auto_unbox = TRUE, pretty = TRUE), OUTPUT_FILE)
  message(sprintf(
    "\nГотово! Переклассифицировано %d из %d null-записей -> %s",
    replaced_count, length(null_idx), OUTPUT_FILE
  ))
}

main()