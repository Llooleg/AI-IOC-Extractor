#' Parse OAI-PMH Records
#'
#' @param xml_content XML document from arXiv OAI-PMH result
#' @return A list of parsed records
#' @keywords internal
parse_oai_records <- function(xml_content) {

  ns <- c(
    oai = "http://www.openarchives.org/OAI/2.0/",
    dc  = "http://purl.org/dc/elements/1.1/"
  )

  records <- xml2::xml_find_all(xml_content, ".//oai:record", ns)

  lapply(records, function(rec) {

    get_field <- function(xpath) {
      node <- xml2::xml_find_first(rec, xpath, ns)
      if (is.na(node)) NA_character_ else xml2::xml_text(node)
    }

    get_all <- function(xpath) {
      nodes <- xml2::xml_find_all(rec, xpath, ns)
      if (length(nodes) == 0) NA_character_
      else paste(xml2::xml_text(nodes), collapse = " | ")
    }

    list(
      id         = get_field(".//oai:identifier"),
      datestamp  = get_field(".//oai:datestamp"),
      title      = get_field(".//dc:title"),
      authors    = get_all(".//dc:creator"),
      abstract   = get_field(".//dc:description"),
      date       = get_field(".//dc:date"),
      categories = get_all(".//dc:subject"),
      url        = get_field(".//dc:identifier[starts-with(text(), 'http')]")
    )
  })
}
