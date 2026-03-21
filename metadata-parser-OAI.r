library(httr)
library(xml2)
library(dplyr)
library(jsonlite)

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
      if (length(nodes) == 0) NA_character_ else paste(xml_text(nodes), collapse = " | ")
    }
    
    list(
      id         = get_field(".//oai:identifier"),
      datestamp  = get_field(".//oai:datestamp"),
      title      = get_field(".//dc:title"),
      authors    = get_all(".//dc:creator"),
      abstract   = get_field(".//dc:description"),
      date       = get_field(".//dc:date"),
      categories = get_all(".//dc:subject")
    )
  })
}

harvest_arxiv_category <- function(category = "cs.CR", output_file = "arxiv_cs_cr.json") {
  base_url <- "https://export.arxiv.org/oai2"
  
  if (file.exists(output_file)) {
    existing_df <- fromJSON(output_file)
    # Берём максимальный datestamp как точку отсчёта
    from_date <- max(as.Date(existing_df$datestamp), na.rm = TRUE)
    cat(sprintf("Existing data found: %d records. Fetching from %s\n", 
                nrow(existing_df), from_date))
    existing_ids <- existing_df$id
  } else {
    existing_df  <- NULL
    from_date    <- NULL
    existing_ids <- character(0)
    cat("No existing data. Full harvest.\n")
  }
  
  resumption_token <- NULL
  new_records      <- list()
  batch_num        <- 0
  
  repeat {
    batch_num <- batch_num + 1
    cat(sprintf("Batch %d | New records so far: %d\n", batch_num, length(new_records)))
    
    if (is.null(resumption_token)) {
      query_params <- list(
        verb           = "ListRecords",
        metadataPrefix = "oai_dc",
        set            = category
      )
      # Добавляем from только если есть существующие данные
      if (!is.null(from_date)) {
        query_params$from <- format(from_date, "%Y-%m-%d")
      }
      response <- GET(base_url, query = query_params)
    } else {
      response <- GET(base_url, query = list(
        verb            = "ListRecords",
        resumptionToken = resumption_token
      ))
    }
    
    if (status_code(response) == 503) {
      retry_after <- as.integer(headers(response)$`retry-after`)
      retry_after <- if (is.na(retry_after)) 30 else retry_after
      cat(sprintf("503. Waiting %d sec...\n", retry_after + 5))
      Sys.sleep(retry_after + 5)
      next
    }
    
    if (status_code(response) != 200) {
      cat(sprintf("Unexpected status %d. Stopping.\n", status_code(response)))
      break
    }
    
    xml_content <- read_xml(content(response, as = "text", encoding = "UTF-8"))
    
    # Проверяем на noRecordsMatch (когда новых записей нет вообще)
    error_node <- xml_find_first(xml_content, ".//*[local-name()='error']")
    if (!is.na(error_node) && xml_attr(error_node, "code") == "noRecordsMatch") {
      cat("No new records since last harvest. Already up to date.\n")
      return(invisible(existing_df))
    }
    
    batch_records <- parse_oai_records(xml_content)
    
    # Дедупликация: отсеиваем уже известные id
    batch_new <- Filter(function(r) !r$id %in% existing_ids, batch_records)
    new_records <- c(new_records, batch_new)
    cat(sprintf("  Batch: %d records, %d new\n", length(batch_records), length(batch_new)))
    
    token_node       <- xml_find_first(xml_content, ".//*[local-name()='resumptionToken']")
    resumption_token <- if (!is.na(token_node) && nchar(trimws(xml_text(token_node))) > 0)
                          xml_text(token_node) else NULL
    
    # Чекпоинт каждые 2 батча
    if (batch_num %% 2 == 0 && length(new_records) > 0) {
      combined <- bind_rows(c(
        if (!is.null(existing_df)) lapply(seq_len(nrow(existing_df)), function(i) as.list(existing_df[i,])) else list(),
        new_records
      ))
      write_json(combined, output_file, pretty = TRUE)
      cat(sprintf("Checkpoint: %d total records\n", nrow(combined)))
    }
    
    if (is.null(resumption_token)) {
      cat("Harvest complete.\n")
      break
    }
    
    Sys.sleep(10)
  }
  
  if (length(new_records) == 0) {
    cat("No new records found.\n")
    return(invisible(existing_df))
  }
  
  new_df <- bind_rows(new_records)
  
  final_df <- if (!is.null(existing_df)) {
    bind_rows(existing_df, new_df) |>
      distinct(id, .keep_all = TRUE) |>   # на всякий случай
      arrange(desc(datestamp))
  } else {
    new_df
  }
  
  write_json(final_df, output_file, pretty = TRUE)
  cat(sprintf("Done. %d total records (%d new) saved to %s\n",
              nrow(final_df), nrow(new_df), output_file))
  
  return(invisible(final_df))
}

df <- harvest_arxiv_category("cs:cs:CR", "metadata.json")