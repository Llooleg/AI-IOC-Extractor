library(httr)
library(jsonlite)
library(future)
library(furrr)

# settings

LM_STUDIO_URL  <- "http://localhost:1234/v1/chat/completions"
MODEL_NAME     <- "tokenizer_qwen3-8b.Q6_K"
INPUT_FILE     <- "E:/metadata.json"
OUTPUT_FILE    <- "E:/final_tagger/full_tagged_context_n_tags.json"
BATCH_SIZE     <- 8
MAX_RETRIES    <- 3
N_WORKERS      <- 8

# иерархия тегов

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

PARENT_TAGS <- names(TAG_TREE)
CHILD_TAGS  <- unlist(TAG_TREE, use.names = FALSE)
ALL_TAGS    <- c(PARENT_TAGS, CHILD_TAGS)

ALLOWED_CONTEXT_TAGS <- c(
  "attack", "defense", "analysis", "survey",
  "formal", "benchmark", "framework", "other"
)

# промпты

build_tags_prompt <- function(abstract, tag_tree) {
  tree_str <- paste(
    mapply(function(parent, children) {
      children_str <- paste(paste0("    - ", children), collapse = "\n")
      paste0("  ", parent, ":\n", children_str)
    }, names(tag_tree), tag_tree, SIMPLIFY = TRUE, USE.NAMES = FALSE),
    collapse = "\n"
  )
  paste0(
    "/no_think\n",
    "You are a scientific article classifier specializing in cybersecurity.\n\n",
    "TASK: Assign 1 to 4 tags to the article abstract from the hierarchy below.\n\n",
    "RULES:\n",
    "- You MUST return at least 1 tag, never return empty.\n",
    "- Prefer specific subtags (indented) over parent categories.\n",
    "- Use a parent category only if no subtag fits.\n",
    "- Separate multiple tags with | (pipe).\n",
    "- Return ONLY tag names, nothing else.\n\n",
    "TAG HIERARCHY:\n", tree_str, "\n\n",
    "Abstract:\n", abstract, "\n\n",
    "Tags:"
  )
}

build_type_prompt <- function(abstract) {
  types_str <- paste(
    "- attack    : proposes a new attack or exploit",
    "- defense   : proposes a defense, mitigation, or protection method",
    "- analysis  : analyzes an existing system, protocol, or CVE",
    "- survey    : literature review or systematization of knowledge",
    "- formal    : mathematical proof or formal model",
    "- benchmark : comparison of methods or performance evaluation",
    "- framework : proposes an architecture, system, or tool",
    "- other     : does not fit any of the above",
    sep = "\n"
  )
  paste0(
    "/no_think\n",
    "You are a scientific article classifier specializing in cybersecurity.\n\n",
    "TASK: Classify what this article DOES (not what it is about).\n",
    "Choose 1 or 2 types from the list below.\n\n",
    "RULES:\n",
    "- You MUST return at least 1 type, never return empty.\n",
    "- Separate two types with | (pipe).\n",
    "- Return ONLY type names from the list, nothing else.\n\n",
    "TYPES:\n", types_str, "\n\n",
    "Abstract:\n", abstract, "\n\n",
    "Type:"
  )
}

# запрос

lm_request <- function(prompt, max_tokens = 80,
                       url = "http://localhost:1234/v1/chat/completions",
                       model = "tokenizer_qwen3-8b.Q6_K",
                       max_retries = 3) {
  payload <- list(
    model       = model,
    messages    = list(list(role = "user", content = prompt)),
    temperature = 0.0,
    max_tokens  = max_tokens
  )
  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      resp <- httr::POST(
        url,
        body   = jsonlite::toJSON(payload, auto_unbox = TRUE),
        encode = "raw",
        httr::add_headers("Content-Type" = "application/json"),
        httr::timeout(60)
      )
      trimws(httr::content(resp, "parsed")$choices[[1]]$message$content)
    }, error = function(e) {
      Sys.sleep(2)
      NA
    })
    if (!identical(result, NA)) return(result)
  }
  return("")
}

# парсинг

parse_tags <- function(raw, all_tags) {
  parts   <- trimws(strsplit(raw, "\\|")[[1]])
  parts   <- parts[nchar(parts) > 0]
  matched <- c()
  for (part in parts) {
    exact <- all_tags[tolower(all_tags) == tolower(part)]
    if (length(exact) > 0) { matched <- c(matched, exact[1]); next }
    soft <- all_tags[sapply(all_tags, function(t) grepl(tolower(t), tolower(part), fixed = TRUE))]
    if (length(soft) > 0) matched <- c(matched, soft[1])
  }
  matched <- unique(matched)
  if (length(matched) > 4) matched <- matched[1:4]
  if (length(matched) == 0) return("Криптография")
  return(matched)
}

parse_context_tags <- function(raw, allowed) {
  parts   <- trimws(strsplit(raw, "\\|")[[1]])
  parts   <- parts[nchar(parts) > 0]
  matched <- allowed[tolower(allowed) %in% tolower(parts)]
  matched <- unique(matched)
  if (length(matched) > 2) matched <- matched[1:2]
  if (length(matched) == 0) return("other")
  return(matched)
}

# обработка 1 статьи

process_article <- function(article, lm_url, model_name, max_retries,
                            tag_tree, all_tags, allowed_context) {
  article_id <- as.character(article$id)
  abstract   <- trimws(if (!is.null(article$abstract)) article$abstract else "")

  if (nchar(abstract) == 0) {
    return(list(id = article_id, skipped = TRUE))
  }

  raw_tags    <- lm_request(build_tags_prompt(abstract, tag_tree),
                            max_tokens = 80, url = lm_url,
                            model = model_name, max_retries = max_retries)
  raw_context <- lm_request(build_type_prompt(abstract),
                            max_tokens = 30, url = lm_url,
                            model = model_name, max_retries = max_retries)

  tags    <- parse_tags(raw_tags, all_tags)
  context <- parse_context_tags(raw_context, allowed_context)

  list(
    id           = article_id,
    tags         = tags,
    context_tags = context,
    abstract     = abstract,
    skipped      = FALSE
  )
}

# загрузка сохранение

load_existing <- function(path) {
  if (!file.exists(path)) return(list())
  tryCatch({
    txt <- trimws(paste(readLines(path, warn = FALSE), collapse = "\n"))
    if (nchar(txt) == 0) return(list())
    data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    setNames(data, sapply(data, function(x) x$id))
  }, error = function(e) list())
}

save_results <- function(results, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  output <- unname(lapply(results, function(x) {
    x$skipped <- NULL
    x
  }))
  write(jsonlite::toJSON(output, auto_unbox = TRUE, pretty = TRUE), path)
}

# mainc

main <- function() {
  # Запускаем параллельные воркеры
  plan(multisession, workers = N_WORKERS)
  message("Параллельных воркеров: ", N_WORKERS)

  message("Загружаем статьи из ", INPUT_FILE, "...")
  articles <- jsonlite::fromJSON(INPUT_FILE, simplifyVector = FALSE)
  total    <- length(articles)
  message("  Всего статей: ", total)

  existing   <- load_existing(OUTPUT_FILE)
  message("  Уже обработано: ", length(existing), "\n")

  results    <- existing
  to_process <- Filter(function(a) !(as.character(a$id) %in% names(results)), articles)
  left       <- length(to_process)
  message("Осталось: ", left, "\n")

  # Разбиваем на батчи по N_WORKERS
  chunks     <- split(to_process, ceiling(seq_along(to_process) / BATCH_SIZE))
  n_chunks   <- length(chunks)

  for (ci in seq_along(chunks)) {
    chunk <- chunks[[ci]]

    # Параллельная обработка батча
    batch_results <- future_map(chunk, function(article) {
      process_article(
        article      = article,
        lm_url       = LM_STUDIO_URL,
        model_name   = MODEL_NAME,
        max_retries  = MAX_RETRIES,
        tag_tree     = TAG_TREE,
        all_tags     = ALL_TAGS,
        allowed_context = ALLOWED_CONTEXT_TAGS
      )
    }, .options = furrr_options(seed = TRUE))

    # Собираем результаты
    for (res in batch_results) {
      if (!res$skipped) {
        results[[res$id]] <- res
        message(sprintf("  %s | context_tags: %s | tags: %s",
                        res$id,
                        paste(res$context_tags, collapse = " | "),
                        paste(res$tags, collapse = " | ")))
      } else {
        message(sprintf("  %s — пустой абстракт", res$id))
      }
    }

    # Сохраняем каждые 10 батчей
    if (ci %% 10 == 0 || ci == n_chunks) {
      save_results(results, OUTPUT_FILE)
      message(sprintf("[%d/%d чанков] Сохранено %d записей\n", ci, n_chunks, length(results)))
    }
  }

  plan(sequential)  # возвращаем обычный режим
  message(sprintf("Готово! Обработано: %d, сохранено в: %s", length(results), OUTPUT_FILE))
}

main()
