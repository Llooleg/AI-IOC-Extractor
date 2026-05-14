# =====================================================================
# tagger_classical.R
# TF-IDF + LogisticRegression (через Python/reticulate) или
# чисто на R через text2vec + glmnet
# =====================================================================

library(jsonlite)
library(text2vec)
library(glmnet)
library(Matrix)

# ── настройки ────────────────────────────────────────────────────────
INPUT_FILE  <- "D:train-ml/metadata.json"
OUTPUT_FILE <- "D:train-ml/full_tagged_classical.json"

# ── иерархия тегов (та же, что у тебя) ──────────────────────────────
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

CONTEXT_LABELS <- c("attack", "defense", "analysis", "survey",
                    "formal", "benchmark", "framework", "other")

# ── keyword-словари для каждого класса ──────────────────────────────
# TF-IDF без обучающих меток — это просто fancy счётчик слов.
# Нам нужны либо размеченные данные, либо keyword-prior.
# Используем второй подход: каждый тег = набор сигнальных слов.
# Модель: cosine similarity между TF-IDF вектором абстракта
# и TF-IDF вектором keyword-документа для каждого класса.
# Просто, интерпретируемо, работает без разметки.

TAG_KEYWORDS <- list(
  # ── Криптография ──
  "Симметричное шифрование (AES, блочные шифры)" =
    "AES block cipher symmetric encryption DES ChaCha20 mode ECB CBC GCM key schedule",
  "Асимметричное шифрование, PKI" =
    "RSA public key infrastructure asymmetric elliptic curve certificate PKI TLS",
  "Постквантовая криптография" =
    "post-quantum lattice NTRU Kyber Dilithium NIST quantum resistant cryptography",
  "Zero-knowledge proofs" =
    "zero knowledge proof ZKP zk-SNARK zk-STARK verifiable computation commitment scheme",
  "Гомоморфное шифрование" =
    "homomorphic encryption FHE CKKS BFV BGV computation on encrypted data",
  "Протоколы MPC" =
    "multiparty computation MPC secret sharing garbled circuit oblivious transfer",

  # ── Безопасность ML-систем ──
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

  # ── Сетевая безопасность ──
  "Обнаружение вторжений (IDS/IPS)" =
    "intrusion detection system IDS IPS anomaly network traffic classification",
  "DDoS, сетевые атаки" =
    "DDoS distributed denial of service flooding botnet amplification network attack",
  "Firewall, VPN, SD-WAN" =
    "firewall VPN SD-WAN network policy filtering tunnel access control",

  # ── Анализ уязвимостей ──
  "Fuzzing, статический анализ" =
    "fuzzing fuzz testing static analysis code vulnerability taint AFL",
  "Эксплойты, CVE-анализ" =
    "exploit CVE vulnerability buffer overflow RCE privilege escalation patch",
  "Side-channel attacks" =
    "side channel timing power analysis cache covert channel Spectre Meltdown",

  # ── Методы атак ──
  "Prompt injection / LLM-атаки" =
    "prompt injection LLM jailbreak adversarial prompt language model attack",
  "Социальная инженерия, фишинг" =
    "phishing social engineering spear phishing email fraud impersonation",

  # ── Приватность ──
  "Анонимизация данных" =
    "anonymization k-anonymity l-diversity data de-identification pseudonymization",
  "GDPR / соответствие нормативам" =
    "GDPR compliance regulation privacy law data protection CCPA policy",
  "Приватность в IoT" =
    "IoT privacy sensor data collection smart device privacy home",

  # ── Блокчейн ──
  "Безопасность смарт-контрактов и DeFi" =
    "smart contract Solidity DeFi reentrancy vulnerability blockchain audit Ethereum",
  "Консенсус-протоколы" =
    "consensus protocol proof of work stake Byzantine fault tolerant blockchain",

  # ── Системная безопасность ──
  "TEE / доверенное исполнение (SGX, TrustZone)" =
    "trusted execution environment SGX TrustZone enclave TEE remote attestation",
  "ОС-безопасность, гипервизоры" =
    "operating system kernel hypervisor VM sandbox privilege separation security",
  "Hardware security" =
    "hardware security TPM secure boot FPGA tamper physical unclonable function",

  # ── Встроенные системы ──
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

# ── TF-IDF similarity engine ─────────────────────────────────────────

build_vectorizer <- function(all_docs) {
  it    <- itoken(all_docs, preprocessor = tolower,
                  tokenizer = word_tokenizer, progressbar = FALSE)
  vocab <- create_vocabulary(it, stopwords = stopwords::stopwords("en"))
  vocab <- prune_vocabulary(vocab, term_count_min = 1L)
  vocab_vectorizer(vocab)
}

tfidf_matrix <- function(docs, vectorizer) {
  it  <- itoken(docs, preprocessor = tolower,
                tokenizer = word_tokenizer, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  tfidf <- TfIdf$new()
  fit_transform(dtm, tfidf)
}

cosine_sim_vec <- function(query_vec, ref_matrix) {
  # query_vec: sparse 1xN, ref_matrix: KxN
  # возвращает вектор длины K
  num   <- as.numeric(query_vec %*% t(ref_matrix))
  dq    <- sqrt(sum(query_vec^2))
  dr    <- sqrt(rowSums(ref_matrix^2))
  num / (dq * dr + 1e-10)
}

classify_abstract <- function(abstract, tag_vecs, context_vecs,
                               vectorizer, tfidf_model,
                               tag_names, context_names,
                               top_n_tags = 3, threshold = 0.05) {
  it  <- itoken(list(abstract), preprocessor = tolower,
                tokenizer = word_tokenizer, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  vec <- transform(dtm, tfidf_model)

  tag_scores     <- cosine_sim_vec(vec, tag_vecs)
  context_scores <- cosine_sim_vec(vec, context_vecs)

  # теги: берём топ-N выше порога
  tag_order <- order(tag_scores, decreasing = TRUE)
  top_tags  <- tag_names[tag_order[tag_scores[tag_order] > threshold]]
  if (length(top_tags) == 0) top_tags <- tag_names[tag_order[1]]
  if (length(top_tags) > top_n_tags) top_tags <- top_tags[1:top_n_tags]

  # context: топ-1, возможно топ-2 если второй близко
  ctx_order <- order(context_scores, decreasing = TRUE)
  top_ctx   <- context_names[ctx_order[1]]
  if (context_scores[ctx_order[2]] > 0.7 * context_scores[ctx_order[1]]) {
    top_ctx <- c(top_ctx, context_names[ctx_order[2]])
  }

  list(tags = top_tags, context_tags = top_ctx)
}

# ── main ─────────────────────────────────────────────────────────────

main <- function() {
  message("Грузим статьи...")
  articles <- fromJSON(INPUT_FILE, simplifyVector = FALSE)
  message("  Статей: ", length(articles))

  all_keyword_docs <- c(unlist(TAG_KEYWORDS), unlist(CONTEXT_KEYWORDS))
  abstracts_all    <- sapply(articles, function(a)
    trimws(if (!is.null(a$abstract)) a$abstract else ""))

message("Строим TF-IDF словарь...")
all_docs_for_vocab <- c(abstracts_all[nchar(abstracts_all) > 0], all_keyword_docs)
vectorizer <- build_vectorizer(all_docs_for_vocab)

# Фитим модель ОДИН раз на всём корпусе
it_all  <- itoken(all_docs_for_vocab, preprocessor = tolower,
                  tokenizer = word_tokenizer, progressbar = FALSE)
dtm_all <- create_dtm(it_all, vectorizer)
tfidf_model <- TfIdf$new()

# fit_transform на всём корпусе — фитит модель И возвращает матрицу (которую выбрасываем)
fit_transform(dtm_all, tfidf_model)  # side-effect: модель теперь fitted

transform_docs <- function(docs) {
  it  <- itoken(docs, preprocessor = tolower,
                tokenizer = word_tokenizer, progressbar = FALSE)
  dtm <- create_dtm(it, vectorizer)
  transform(dtm, tfidf_model)  # просто transform, не fit_transform
}
  message("Трансформируем keyword-векторы...")
  tag_names     <- names(TAG_KEYWORDS)
  context_names <- names(CONTEXT_KEYWORDS)
  tag_vecs      <- transform_docs(unlist(TAG_KEYWORDS))
  context_vecs  <- transform_docs(unlist(CONTEXT_KEYWORDS))

  # нормализуем reference-векторы один раз
  tag_norms     <- sqrt(rowSums(tag_vecs^2))
  context_norms <- sqrt(rowSums(context_vecs^2))

  existing <- if (file.exists(OUTPUT_FILE)) {
    data <- fromJSON(OUTPUT_FILE, simplifyVector = FALSE)
    setNames(data, sapply(data, `[[`, "id"))
  } else list()
  message("  Уже обработано: ", length(existing))

  to_process <- Filter(function(a) !(as.character(a$id) %in% names(existing)),
                       articles)
  message("  Осталось: ", length(to_process), "\n")

  if (length(to_process) == 0) {
    message("Всё уже обработано.")
    return(invisible(NULL))
  }

  ids       <- sapply(to_process, function(a) as.character(a$id))
  abstracts <- sapply(to_process, function(a)
    trimws(if (!is.null(a$abstract)) a$abstract else ""))

  nonempty  <- nchar(abstracts) > 0
  message(sprintf("  Пустых абстрактов: %d", sum(!nonempty)))

  # ── батчевая трансформация ────────────────────────────────────────
  message("Трансформируем абстракты батчем...")
  abs_matrix <- transform_docs(abstracts[nonempty])

  # cosine similarity разом: [n_abstracts x n_tags]
  abs_norms <- sqrt(rowSums(abs_matrix^2))

  tag_scores     <- as.matrix(abs_matrix %*% t(tag_vecs)) /
                     outer(abs_norms, tag_norms + 1e-10)
  context_scores <- as.matrix(abs_matrix %*% t(context_vecs)) /
                     outer(abs_norms, context_norms + 1e-10)

  colnames(tag_scores)     <- tag_names
  colnames(context_scores) <- context_names

  # ── сборка результатов ────────────────────────────────────────────
  message("Собираем результаты...")
  results <- existing

  nonempty_ids       <- ids[nonempty]
  nonempty_abstracts <- abstracts[nonempty]

  for (i in seq_len(nrow(tag_scores))) {
    ts  <- tag_scores[i, ]
    cs  <- context_scores[i, ]

    # теги: топ-3 выше порога
    tag_order <- order(ts, decreasing = TRUE)
    top_tags  <- tag_names[tag_order[ts[tag_order] > 0.05]]
    if (length(top_tags) == 0) top_tags <- tag_names[tag_order[1]]
    if (length(top_tags) > 3)  top_tags <- top_tags[1:3]

    # context: топ-1, второй если близко к первому
    ctx_order <- order(cs, decreasing = TRUE)
    top_ctx   <- context_names[ctx_order[1]]
    if (!is.na(cs[ctx_order[1]]) && cs[ctx_order[1]] > 0 &&
    !is.na(cs[ctx_order[2]]) && cs[ctx_order[2]] > 0.7 * cs[ctx_order[1]])
    top_ctx <- c(top_ctx, context_names[ctx_order[2]])

    article_id <- nonempty_ids[i]
    results[[article_id]] <- list(
      id           = article_id,
      tags         = top_tags,
      context_tags = top_ctx,
      abstract     = nonempty_abstracts[i]
    )
  }

  # пустые абстракты логируем
  for (id in ids[!nonempty])
    message(sprintf("  %s — пустой абстракт", id))

  # ── сохраняем ─────────────────────────────────────────────────────
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)
  write(toJSON(unname(results), auto_unbox = TRUE, pretty = TRUE), OUTPUT_FILE)
  message(sprintf("\nГотово! %d записей -> %s", length(results), OUTPUT_FILE))
}

main()