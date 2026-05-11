library(pdftools)
library(stringr)
library(jsonlite)
library(progress)
library(fastTextR)

#Параметры
PDF_FOLDER   <- "E:/files"
OUTPUT_JSONL <- "E:/files2/qwen_dataset.jsonl"
FASTTEXT_MODEL <- "E:/cc.ru.300.bin"

MAX_CHARS <- 12000L
MIN_CHARS <- 200L
TOP_K     <- 3L

SYSTEM_PROMPT <- paste0(
  "Ты эксперт по кибербезопасности. ",
  "Определи 1–5 наиболее подходящих тегов по тексту статьи. ",
  "Отвечай строго тегами через | без пояснений."
)

TAGS <- c(
  "Симметричное шифрование (AES, блочные шифры)",
  "Асимметричное шифрование, PKI",
  "Постквантовая криптография",
  "Zero-knowledge proofs / доказательства с нулевым разглашением",
  "Гомоморфное шифрование",
  "Протоколы многостороннего вычисления (MPC)",

  "Обнаружение вторжений (IDS/IPS)",
  "DDoS, сетевые атаки",
  "Firewall, VPN, SD-WAN",

  "Adversarial attacks",
  "Отравление данных (data poisoning)",
  "Differential privacy",
  "Federated learning + privacy",

  "Fuzzing, статический анализ",
  "Side-channel attacks (утечки через кэш, питание и т.д.)",
  "Эксплойты, CVE-анализ",
  "Prompt injection / LLM-атаки",

  "Анонимизация данных",
  "GDPR / соответствие нормативам",
  "Приватность в IoT",

  "Smart contract security",
  "Консенсус-протоколы",
  "DeFi-атаки",

  "TEE / доверенное исполнение (SGX, TrustZone)",
  "ОС-безопасность, гипервизоры",
  "Hardware security",

  "IoT / CPS / автономные системы",
  "Промышленные системы (ICS/SCADA)",
  "Биометрия"
)

#Загрузка fastText-модели
message("Загрузка fastText модели...")
ft_model <- ft_load(FASTTEXT_MODEL)

#Функция получения вектора
get_sentence_vec <- function(txt, model) {
  #Лишние пробелы
  txt <- tolower(str_squish(txt))
  words <- strsplit(txt, "\\s+")[[1]]
  vecs <- lapply(words, function(w) {
    v <- ft_word_vec(model, w)
    if (is.null(v) || length(v) == 0) NULL else as.numeric(v)
  })
  vecs <- vecs[!sapply(vecs, is.null)]
  if (length(vecs) == 0) return(rep(0, ft_word_vec(model, "криптография") %>% length())) # fallback
  #Усредняем
  mat <- do.call(rbind, vecs)
  colMeans(mat)
}

#Извлечение текста из PDF
extract_text <- function(path) {
  tryCatch({
    txt <- pdf_text(path) |> paste(collapse = " ")
    txt <- str_squish(txt)
    if (nchar(txt) < MIN_CHARS) return(NULL)
    str_sub(txt, 1, MAX_CHARS)
  }, error = function(e) NULL)
}

#Эмбеддинги тегов
message("Вычисление эмбеддингов тегов...")
tag_emb <- t(sapply(TAGS, get_sentence_vec, model = ft_model))

#Подбора тегов
get_tags <- function(text) {
  text_vec <- get_sentence_vec(text, ft_model)
  #Косинусное сходство между одним вектором и матрицей
  sim <- as.numeric(text_vec %*% t(tag_emb)) /
    (sqrt(sum(text_vec^2)) * sqrt(rowSums(tag_emb^2)))
  top_idx <- order(sim, decreasing = TRUE)[1:min(TOP_K, length(TAGS))]
  TAGS[top_idx]
}


pdf_files <- list.files(PDF_FOLDER, pattern = "\\.pdf$", full.names = FALSE)
message("Найдено PDF-файлов: ", length(pdf_files))

written <- 0
skipped <- 0

if (!dir.exists(dirname(OUTPUT_JSONL))) dir.create(dirname(OUTPUT_JSONL), recursive = TRUE)

pb <- progress_bar$new(
  format = "  Обработка [:bar] :percent :eta",
  total = length(pdf_files), clear = FALSE, width = 60
)

conn <- file(OUTPUT_JSONL, "w", encoding = "UTF-8")

for (file in pdf_files) {
  pb$tick()
  path <- file.path(PDF_FOLDER, file)
  text <- extract_text(path)
  if (is.null(text)) {
    skipped <- skipped + 1
    next
  }
  tags <- get_tags(text)
  sample <- list(
    messages = list(
      list(role = "system",    content = SYSTEM_PROMPT),
      list(role = "user",      content = text),
      list(role = "assistant", content = paste(tags, collapse = "|"))
    )
  )
  writeLines(jsonlite::toJSON(sample, auto_unbox = TRUE, force = TRUE), conn)
  written <- written + 1
}

close(conn)

message("\nГОТОВО")
message("Записано: ", written)
message("Пропущено: ", skipped)
message("Результат: ", OUTPUT_JSONL)