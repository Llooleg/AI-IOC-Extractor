# JSON-схемы инструментов для xAI (OpenAI function calling формат).
# Каждая схема описывает имя, назначение и параметры инструмента.

TOOL_DEFINITIONS <- list(
  list(
    type = "function",
    "function" = list(
      name = "search_articles",
      description = paste0(
        "Поиск статей по кибербезопасности в базе данных по ключевым словам. ",
        "Ищет совпадения в заголовке и аннотации статьи. ",
        "Используй когда пользователь хочет найти статьи по теме или ключевому слову."
      ),
      parameters = list(
        type = "object",
        properties = list(
          query = list(
            type        = "string",
            description = "Ключевые слова для поиска (например: 'ransomware', 'APT attack', 'zero-day')"
          ),
          limit = list(
            type        = "integer",
            description = "Максимальное количество возвращаемых статей (1-50, по умолчанию 10)",
            default     = 10L
          )
        ),
        required = list("query")
      )
    )
  ),
  list(
    type = "function",
    "function" = list(
      name = "filter_by_tags",
      description = paste0(
        "Фильтрация статей по тегам, категориям или контекстным тегам. ",
        "Используй когда пользователь хочет найти статьи определённой категории или с конкретными тегами. ",
        "Можно указывать несколько значений одновременно."
      ),
      parameters = list(
        type = "object",
        properties = list(
          tags = list(
            type        = "array",
            items       = list(type = "string"),
            description = "Список тегов для фильтрации (например: ['malware', 'phishing'])"
          ),
          categories = list(
            type        = "array",
            items       = list(type = "string"),
            description = "Список категорий (например: ['cs.CR', 'cs.AI'])"
          ),
          context_tags = list(
            type        = "array",
            items       = list(type = "string"),
            description = "Список контекстных тегов"
          ),
          limit = list(
            type        = "integer",
            description = "Максимальное количество результатов (1-50, по умолчанию 10)",
            default     = 10L
          )
        ),
        required = list()
      )
    )
  ),
  list(
    type = "function",
    "function" = list(
      name = "get_article_by_id",
      description = paste0(
        "Получить полные данные конкретной статьи по её ID. ",
        "Возвращает заголовок, аннотацию, авторов, дату, теги и категории. ",
        "Используй когда нужна подробная информация о конкретной статье."
      ),
      parameters = list(
        type = "object",
        properties = list(
          article_id = list(
            type        = "string",
            description = "Уникальный идентификатор статьи"
          )
        ),
        required = list("article_id")
      )
    )
  ),
  list(
    type = "function",
    "function" = list(
      name = "get_statistics",
      description = paste0(
        "Получить статистику по базе данных: общее количество статей, ",
        "топ теги и категории, диапазон дат. ",
        "Используй когда пользователь спрашивает о содержимом или объёме базы данных."
      ),
      parameters = list(
        type       = "object",
        properties = setNames(list(), character(0)),
        required   = list()
      )
    )
  ),
  list(
    type = "function",
    "function" = list(
      name = "get_articles_by_date",
      description = paste0(
        "Получить статьи за определённый период времени. ",
        "Используй когда пользователь хочет найти статьи за конкретный год, месяц или период."
      ),
      parameters = list(
        type = "object",
        properties = list(
          start_date = list(
            type        = "string",
            description = "Начальная дата в формате YYYY-MM-DD (например: '2024-01-01')"
          ),
          end_date = list(
            type        = "string",
            description = "Конечная дата в формате YYYY-MM-DD (например: '2024-12-31')"
          ),
          limit = list(
            type        = "integer",
            description = "Максимальное количество результатов (1-50, по умолчанию 15)",
            default     = 15L
          )
        ),
        required = list("start_date", "end_date")
      )
    )
  ),
  list(
    type = "function",
    "function" = list(
      name = "get_tags_and_categories",
      description = paste0(
        "Получить список всех доступных тегов, категорий и контекстных тегов в базе данных ",
        "с частотой их использования. ",
        "Используй когда пользователь спрашивает о доступных темах или хочет узнать, ",
        "какие теги/категории есть в системе."
      ),
      parameters = list(
        type = "object",
        properties = list(
          limit = list(
            type        = "integer",
            description = "Максимальное количество элементов каждого типа (5-100, по умолчанию 30)",
            default     = 30L
          )
        ),
        required = list()
      )
    )
  )
)
