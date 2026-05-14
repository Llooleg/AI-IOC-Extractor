---
title: "AI IOC Extractor"
subtitle: "Структурируем профессиональные знания для эффективной защиты"
author: "Безлепкин Л.А., Демидов Б.Ю., Лебедев О.Н., Настепанин И.Д., Плохих Г.А."
date: "2026-03-14"
---


## Задача

Мы разрабатываем **специализированное веб-приложение** для работы с контентом по теме информационной безопасности, которое включает:

- **Структурированную базу данных** статей, исследований и аналитических материалов
- **Интеллектуальную систему тегирования** на основе нейросетевых алгоритмов
- **Семантический поиск**, понимающий контекст и смысл запросов
- **Инструменты аналитики** для выявления трендов и связей между угрозами


## Актуальность

Количество кибератак растёт с каждым днём.

В условиях санкционного давления критически важна собственная инфраструктура знаний по ИБ.

Еженедельно публикуется:

- **~800** новых уязвимостей в базе CVE

- **~500** статей в профильных журналах

- **>5000** постов в профессиональных блогах


## Решаемые проблемы


Разрозненные источники информации.

Сложность анализа публикаций.

Незнание технического английского языка.


## Наши решения

Централизованная база знаний

Анализирующая ИИ система

Перевод на основе ИИ, чат для уточнения терминов.


## СТРУКТУРА ПРОЕКТА

**3 ключевых этапа:**

1. **Сбор данных** 
2. **Обучение ИИ, разработка приложения**
3. **Поддержка жизни приложения**

## ЭТАП 1: СБОР ДАННЫХ

На этом этапе нам необходимо подготовить информационную базу для обучения ИИ и пополнения библиотеки.

Для сбора будем использовать язык R и соответствующий пакет aRxiv,а в качестве СУБД - Mongodb.

**Источник:** arXiv.org

Для парсинга мы воспользуемся следующими библиотеками:

- **httr** - для запросов к API
- **xml2** - для парсинга результатов
- **dplyr** - для обработки данных
-  **jsonlite** - для "упаковки" результатов в JSON

Функция **parse_oai_records** парсит полученные записи. Она принимает XML, полученный от *arxiv*, и для каждой записи вытаскивает необходимую информацию:

- **авторы**
- **дата**
- **заголовок**
- **id**
- и другие

```
parse_oai_records <- function(xml_content) {
  ns <- c(oai = "http://www.openarchives.org/OAI/2.0/",
          dc  = "http://purl.org/dc/elements/1.1/")
  
  records <- xml_find_all(xml_content, ".//oai:record", ns)
  
  lapply(records, function(rec) {
    get_field <- function(xpath) {
      node <- xml_find_first(rec, xpath, ns)
      if (is.na(node)) NA_character_ else xml_text(node)
    }
    get_all <- function(xpath) {
      nodes <- xml_find_all(rec, xpath, ns)
      if (length(nodes) == 0) NA_character_ 
      else paste(xml_text(nodes), collapse = " | ")
    }
    
    list(
      id          = get_field(".//oai:identifier"),
      datestamp   = get_field(".//oai:datestamp"),
      title       = get_field(".//dc:title"),
      authors     = get_all(".//dc:creator"),
      abstract    = get_field(".//dc:description"),
      date        = get_field(".//dc:date"),
      categories  = get_all(".//dc:subject")
    )
  })
}
```

Функция **harvest_arxiv_category** осуществляет запросы к API *arxiv*. Если есть файл с метаданными, то берем максимальную дату, указанную там, и используем её как точку отсчета", составляя запросы с неё. В противном случае составляем запросы "с нуля".

```
harvest_arxiv_category <- function(category = "cs.CR", output_file = 
"arxiv_cs_cr.rds") {
  base_url        <- "https://export.arxiv.org/oai2"
  resumption_token <- NULL
  all_records     <- list()
  batch_num       <- 0
  repeat {
    batch_num <- batch_num + 1
    cat(sprintf("Fetching batch %d | Total records so far: %d\n", 
    batch_num, length(all_records)))
    if (is.null(resumption_token)) {
      response <- GET(base_url, query = list(
        verb            = "ListRecords",
        metadataPrefix  = "oai_dc",
        set             = category))
      print(response)
    } else {
      response <- GET(base_url, query = list(
        verb             = "ListRecords",
        resumptionToken  = resumption_token
      ))
      print(response)
    }
    if (status_code(response) == 503) {
      retry_after <- as.integer(headers(response)$`retry-after`) 
      retry_after <- if (is.na(retry_after)) 30 else retry_after
      cat(sprintf("503 received. Waiting %d seconds...\n", 
      retry_after))
      Sys.sleep(retry_after + 5)
      next
    }
    print(response)
```

Затем начинаем сбор данных. Сервер будет отдавать данные *"пачками"* по ~1000 записей в формате XML за запрос. Для пагинации по записям используется resumption token, который мы получаем от сервера. В коде  предусмотрена обработка ошибок, rate limiting, дедупликация. Каждые 2 *"пачки"* идёт запись в JSON-файл. 

```
harvest_arxiv_category <- function(category = "cs.CR", output_file = 
"arxiv_cs_cr.rds") {
  base_url        <- "https://export.arxiv.org/oai2"
  resumption_token <- NULL
  all_records     <- list()
  batch_num       <- 0
  repeat {
    batch_num <- batch_num + 1
    cat(sprintf("Fetching batch %d | Total records so far: %d\n", 
    batch_num, length(all_records)))
    if (is.null(resumption_token)) {
      response <- GET(base_url, query = list(
        verb            = "ListRecords",
        metadataPrefix  = "oai_dc",
        set             = category))
      print(response)
    } else {
      response <- GET(base_url, query = list(
        verb             = "ListRecords",
        resumptionToken  = resumption_token
      ))
      print(response)
    }
    if (status_code(response) == 503) {
      retry_after <- as.integer(headers(response)$`retry-after`) 
      retry_after <- if (is.na(retry_after)) 30 else retry_after
      cat(sprintf("503 received. Waiting %d seconds...\n", 
      retry_after))
      Sys.sleep(retry_after + 5)
      next
    }
    print(response)
```

**Иные защитные меры для предотвращения ошибок и запись в файл:**

```
if (status_code(response) == 503) {
      retry_after <- as.integer(headers(response)$`retry-after`) 
      retry_after <- if (is.na(retry_after)) 30 else retry_after
      cat(sprintf("503 received. Waiting %d seconds...\n", retry_after))
      Sys.sleep(retry_after + 5)
      next
    }
    print(response)
    if (status_code(response) != 200) {
      cat(sprintf("Unexpected status %d. Stopping.\n", status_code(response)))
      break
    }
    xml_content     <- read_xml(content(response, as = "text", encoding = "UTF-8"))
    records         <- parse_oai_records(xml_content)
    all_records     <- c(all_records, records)
    token_node      <- xml_find_first(xml_content, 
                                      ".//*[local-name()='resumptionToken']")
    resumption_token <- if (!is.na(token_node)) xml_text(token_node) else NULL
    if (batch_num %% 2 == 0) {
      saveRDS(all_records, output_file)
      cat(sprintf("Checkpoint saved: %d records\n", length(all_records)))
       write_json(df, file.path(dirname(output_file), "metadata.json"), pretty = TRUE)

    }
    if (is.null(resumption_token) || nchar(trimws(resumption_token)) == 0) {
      cat("No resumption token. Harvest complete.\n")
      break
    }
    df <- bind_rows(all_records)
    Sys.sleep(20) 
  }
```

## ЭТАП 2: Обучение ИИ, разработка приложения 

На этом этапе мы займемся fine-tuning'ом нейросети и созданием веб-приложения, соединяющего ИИ-поиск с базой данных.

**Нейросеть:** Нейросеть Qwen 2.5.

**Технологии:** H2O llm studio, LoRa, Shiny

Задача — взять 1000 PDF статей по кибербезопасности и автоматически присвоить каждой теги, не используя LLM и не размечая вручную. Это решение первой и самой трудоёмкой проблемы: откуда взять обучающие данные.

Подход основан на семантическом сходстве. Текст статьи и названия тегов переводятся в векторное пространство с помощью модели all-MiniLM-L6-v2, после чего измеряется косинусное расстояние между вектором статьи и векторами каждого тега. Три ближайших тега по смыслу и становятся метками.

Технические решения:

- **PyMuPDF (fitz)** — извлечение текста из PDF

- **sentence-transformers/all-MiniLM-L6-v2** — лёгкая быстрая модель для эмбеддингов

- **cosine_similarity** — метрика близости векторов

- **Формат JSONL** — стандарт для файнтюнинга

```
model = SentenceTransformer("sentence-transformers/"
"all-MiniLM-L6-v2")
tag_emb = model.encode(TAGS, normalize_embeddings=True)
def extract_text(path):
    try:
        doc = fitz.open(path)
        text = ""
        for page in doc:
            text += page.get_text()
        text = re.sub(r"\s+", " ", text).strip()
        if len(text) < MIN_CHARS:
            return None
        return text[:MAX_CHARS]
    except Exception:
        return None
def get_tags(text):
    emb = model.encode([text], normalize_embeddings=True)
    sims = cosine_similarity(emb, tag_emb)[0]
    idx = sims.argsort()[-TOP_K:][::-1]
    return [TAGS[i] for i in idx]
```

**Результат** — файл qwen_dataset.jsonl, где каждая запись - это готовый диалог в формате system/user/assistant, пригодный для файнтюнинга.

```
pdf_files = [
    f for f in os.listdir(PDF_FOLDER)
    if f.endswith(".pdf")
]
print("PDF FOUND:", len(pdf_files))
written = 0
skipped = 0
with open(OUTPUT_JSONL, "w", encoding="utf-8") as f:
    for file in tqdm(pdf_files):
        path = os.path.join(PDF_FOLDER, file)
        text = extract_text(path)
        if text is None:
            skipped += 1
            continue
        tags = get_tags(text)
        sample = {
            "messages": [
                {
                    "role": "system",
                    "content": SYSTEM_PROMPT
                },
                {
                    "role": "user",
                    "content": text
                },
                {
                    "role": "assistant",
                    "content": "|".join(tags)
                }
            ]
        }
        f.write(json.dumps(sample, ensure_ascii=False) + "\n")
        written += 1
```

На этом этапе берётся предобученная модель **Qwen3-8B** и дообучается на размеченном датасете из 1000 статей. **Цель** — научить модель определять теги по тексту статьи в рамках строго заданного словаря из 29 категорий.

Для обучения используется **LoRA (Low-Rank Adaptation)** — метод который не меняет все веса модели, а добавляет небольшие адаптерные матрицы к ключевым слоям. Это позволяет обучать 8-миллиардную модель на обычном потребительском GPU вместо кластера. Фреймворк **Unsloth** оптимизирует обучение под конкретное железо — ускоряет forward/backward pass и снижает потребление VRAM примерно в 2 раза по сравнению с обычным HuggingFace.

Ключевые параметры обучения:

- **15 эпох** — датасет маленький (1000 примеров), нужно много проходов

- **Cosine learning rate scheduler** — плавное снижение LR к концу обучения

- **Gradient checkpointing** — экономия VRAM за счёт пересчёта активаций

```
tokenizer = get_chat_template(tokenizer, 
chat_template="qwen-2.5")
dataset = load_dataset("json", data_files=DATASET_PATH,
split="train")
dataset = standardize_sharegpt(dataset)
def formatting_func(examples):
    texts = [
        tokenizer.apply_chat_template(
            convo,
            tokenize              = False,
            add_generation_prompt = False
        )
        for convo in examples["messages"]
    ]
    return {"text": texts}
dataset = dataset.map(formatting_func, batched=True)
trainer = SFTTrainer(...)
trainer.train()
```

Дообученная модель через LM Studio API обрабатывает все 47 390 абстрактов и присваивает каждой статье два типа меток.

**Первый** - тематические теги по двухуровневой иерархии из 9 родительских категорий и 31 подкатегории. Модель выбирает от 1 до 4 тегов, предпочитая конкретные подкатегории общим. Если подкатегория не подходит - ставится родительская.

**Второй** - тип исследования, отвечающий на вопрос что именно делает статья: предлагает атаку, защиту, формальную модель, обзор литературы или что-то иное. Это второе измерение классификации позволяет фильтровать базу не только по теме, но и по характеру вклада.

Технические решения:

- **future + furrr** - параллельные сессии R для одновременных HTTP запросов

- **Двухуровневая иерархия тегов в промпте** — модель видит структуру и выбирает осознанно

- **/no_think** - отключение reasoning mode Qwen3

- **Checkpoint система** - load_existing при старте подхватывает уже готовые результаты

- **Двойной матчинг** - точное совпадение, затем мягкое через grepl

- **Запасные теги** - никогда не возвращает null

```
lm_request <- function(prompt, max_tokens = 80,
                       url = "http://localhost:1234/v1/chat/"
                       "completions",
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
      trimws(httr::content(resp, "parsed")$choices[[1]]
      $message$content)
    }, error = function(e) {
      Sys.sleep(2)
      NA
    })
    if (!identical(result, NA)) return(result)
  }
  return("")
}
```

Для ускорения обработки используется параллельное выполнение через future + furrr - 8 воркеров отправляют запросы  одновременно, соответствуя числу параллельных слотов LM Studio.

```
process_article <- function(article, lm_url, model_name,
 max_retries, tag_tree, all_tags, allowed_context) {
  article_id <- as.character(article$id)
  abstract   <- trimws(if (!is.null(article$abstract))
  article$abstract else "")
  if (nchar(abstract) == 0) {
    return(list(id = article_id, skipped = TRUE))
  }
  raw_tags    <- lm_request(build_tags_prompt(
    abstract, tag_tree),
                            max_tokens = 80, url = lm_url,
                            model = model_name, max_retries
                             = max_retries)
  raw_context <- lm_request(build_type_prompt(abstract),
                            max_tokens = 30, url = lm_url,
                            model = model_name, max_retries
                             = max_retries)

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
```

Результат сохраняется в JSON каждые 10 батчей с поддержкой продолжения с места остановки.

```
load_existing <- function(path) {
  if (!file.exists(path)) return(list())
  tryCatch({
    txt <- trimws(paste(readLines(path, warn = FALSE), 
    collapse = "\n"))
    if (nchar(txt) == 0) return(list())
    data <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    setNames(data, sapply(data, function(x) x$id))
  }, error = function(e) list())
}
save_results <- function(results, path) {
  dir.create(dirname(path), recursive = TRUE, 
  showWarnings = FALSE)
  output <- unname(lapply(results, function(x) {
    x$skipped <- NULL
    x
  }))
  write(jsonlite::toJSON(output, auto_unbox = TRUE
  , pretty = TRUE), path)
}
```

## ЭТАП 3: Поддержка жизни приложения

На этом этапе мы будем регулярно обновлять базу данных новыми публикациями и поддерживать актуальность ИИ-модели.

Интерфейс построен на базе пакета **bslib**, который заменяет стандартный дизайн Shiny на современные компоненты **Bootstrap 5**. Приложение использует многостраничную навигацию (page_navbar). В отличие от старых подходов Shiny, где страницы перерисовывались сервером (renderUI), здесь все вкладки загружаются в DOM сразу, а переключение между ними происходит мгновенно на стороне клиента.

Основные вкладки:

- **Главная (home)**: Дашборд для ручного обновления базы данных.

- **Библиотека (articles)**: Основная таблица со списком статей.

- **Разделы (sections)**: Инструмент для сборки комбинаций тегов и категорий.

- **AI (chat)**: Глобальный чат с ИИ-ассистентом.

- **Детали статьи (article_detail)**: Скрытая вкладка, которая динамически активируется при клике на конкретную статью в "Библиотеке".

Взаимодействие с базой данных вынесено в изолированную функцию **fetch_data()**. Для подключения используется пакет mongolite. Подключение происходит безопасно: учетные данные не зашиты в код, а подтягиваются из переменных окружения. Функция использует механизм **on.exit(db$disconnect())**, что гарантирует автоматическое закрытие соединения с БД при любом исходе (даже при ошибках), предотвращая утечку соединений.

```
fetch_data <- function() {
  db <- mongolite::mongo(
    collection = "metadata",
    db = "cybersecurity_articles",
    url = sprintf(
      "mongodb://%s:%s@%s:27017/cybersecurity?
      authSource=admin",
      Sys.getenv("MONGO_USER"),
      Sys.getenv("MONGO_PASS"),
      Sys.getenv("MONGO_HOST")
    )
  )
  on.exit(db$disconnect())

  data <- db$find("{}")
  if (nrow(data) > 0) {
    data <- data %>% mutate(date = as.Date(date))
  }
  return(data)
}
```

Приложение объединяет Категории и Теги в единый справочник "Разделы". Поскольку **MongoDB** часто отдает списки в смешанном формате (где-то пустые NULL, где-то строки), перед фильтрацией применяется функция **sanitize_list_col**, приводящая массивы к единому строковому виду. Вкладка "Разделы" позволяет выбрать несколько элементов (логическое ИЛИ). Реактивный блок **filtered_data** вычисляет match_score для каждой статьи:

```
match_scores <- sapply(1:nrow(data), function(i) {
        score <- 0
        cat_list <- if ("categories" %in% names(data) && 
        !is.null(data$categories[[i]])) unlist(data$
        categories[[i]]) 
        else character(0)
        tag_list <- if ("tag" %in% names(data) 
        && !is.null(data$tag[[i]])) unlist(data$tag[[i]])
        else character(0)

        for (j in 1:nrow(filts)) {
          if (filts$type[j] == "Категория" && 
          (filts$value[j] %in% cat_list)) score <- score + 1
          if (filts$type[j] == "Тег" && 
          (filts$value[j] %in% tag_list)) score <- score + 1
        }
        score
      })

      data$match_score <- match_scores
      data <- data %>%
        filter(match_score > 0) %>%
        arrange(desc(match_score), desc(date))
```

Благодаря этому алгоритму в верху списка всегда оказываются статьи, наиболее полно удовлетворяющие сложному запросу пользователя.

Shiny по умолчанию однопоточен. Если бы мы выполняли HTTP-запросы к ИИ синхронно, весь интерфейс приложения "зависал" бы для всех пользователей на время генерации ответа. Для решения этой проблемы архитектура приложения использует связку пакетов future и promises (plan(multisession)). Это позволяет делегировать сетевые запросы к ИИ в отдельные фоновые процессы.

Глобальный чат использует HTTP клиент **httr2**. При отправке сообщения создается фоновая задача(**future**), которая "обещает" (**promise**) вернуть ответ. Когда ответ приходит, срабатывает оператор %...>%, который обновляет UI, не прерывая работу основного потока R.

```
future_promise <- future(
      {
        tryCatch({
            resp <- httr2::request(Sys.getenv(
              "AI_SERVICE_URL")) %>%
              httr2::req_body_json(list(
                message = user_text)) %>%
              httr2::req_timeout(60) %>%
              httr2::req_perform() %>%
              httr2::resp_body_json()
            resp$choices[[1]]$message$content
          },
          error = function(e) {
            paste("Ошибка при обращении к AI-сервису:",
             e$message)
          })
      },
      seed = TRUE
    )
    future_promise %...>% (function(ai_response) {
      global_chat_data$history <- append(
        global_chat_data$history, 
      list(list(role = "ai", text = ai_response)))
      global_chat_data$is_loading <- FALSE
    }) %...!% (function(error) {
      global_chat_data$history <- append(
        global_chat_data$history, 
      list(list(role = "ai", text = "Произошла системная 
      ошибка при обращении к AI.")))
      global_chat_data$is_loading <- FALSE
    })
```

В карточке статьи есть возможность начать диалог по конкретному тексту (аннотации). При инициализации чата (SYSTEM_INIT_ANALYSIS) текст статьи скрыто отправляется в ИИ-сервис для предварительного анализа. Логика обращения вынесена в функцию-посредник ask_article_ai. В текущей версии она содержит симуляцию задержки, но готова к интеграции с реальным локальным портом.

Приложение упаковывается в Docker с установкой необходимых библиотек (например, zlib1g-dev для mongolite и libuv1-dev для асинхронности). 

На самом приложение скрыто за Nginx, который выступает в роли Reverse Proxy, обеспечивая терминацию SSL (HTTPS) и проброс WebSockets, необходимых для непрерывного коннекта между браузером и Shiny сервером.

```
version: '3.8'
services:
  mongodb:
    image: mongo:latest
    container_name: mongodb
    restart: always
    environment:
      MONGO_INITDB_ROOT_USERNAME: …
      MONGO_INITDB_ROOT_PASSWORD: …
    ports:
      - "127.0.0.1:27017:27017"
    volumes:
      - mongo_data:/data/db
  mongo-express:
    image: mongo-express:latest
    container_name: mongo-express
    restart: always
    ports:
      - "127.0.0.1:8081:8081"
```
## Ссылки

### EG Team

**GitHub**: https://github.com/Llooleg/AI-IOC-Extractor<br>
**Telegram**: https://t.me/+KLvdo-ZnRCpmNDc6

</div>


# Спасибо за внимание!

Готовы ответить на ваши вопросы.