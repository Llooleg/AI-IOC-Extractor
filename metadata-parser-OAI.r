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

harvest_arxiv_category <- function(category = "cs.CR", output_file = "arxiv_cs_cr.rds") {
  base_url        <- "https://export.arxiv.org/oai2"
  resumption_token <- NULL
  all_records     <- list()
  batch_num       <- 0
  
  repeat {
    batch_num <- batch_num + 1
    cat(sprintf("Fetching batch %d | Total records so far: %d\n", batch_num, length(all_records)))
    
    if (is.null(resumption_token)) {
      response <- GET(base_url, query = list(
        verb            = "ListRecords",
        metadataPrefix  = "oai_dc",
        set             = category
      ))
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
  
  df <- bind_rows(all_records)
  saveRDS(df, output_file)
  cat(sprintf("Done. %d total records saved to %s\n", nrow(df), output_file))
  write_json(df, file.path(dirname(output_file), "metadata.json"), pretty = TRUE)
  return(df)
}

df <- harvest_arxiv_category("cs:cs:CR")