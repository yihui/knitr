#' Extract R code from a knitted R Markdown HTML file.
#'
#' @name html_to_r
#' @author Steve Condylios
#' @usage html_to_r(rmarkdown_html, padding, inc_out)
#' @param rmarkdown_html The  HTML output  of a knitted Rmd file
#' @param padding Specifies what goes between the last character of a code block and the
#'     first character of the next code block. Defaults to two newlines (which gives
#'     the visual appearance of one newline between code blocks).
#' @param inc_out \code{TRUE}/\code{FALSE} as to whether to include output of code chucks. Defaults
#'     to \code{TRUE}.
#'
#' @return A character vector of length 1 containing the R code extracted from the
#'     R Markdown HTML file.
#'
#' @export
#'
#' @examples
#' cat((rmarkdown_html <- "<html><body>Intro
#' ```R\n1 * 1\n```\nmore text\n```r\n2 * 2\n```
#' some more text\n```\n3 * 3\n```\nThe End.
#' </body></html>\n"))
#'
#' html_to_r(rmarkdown_html)
#'
#' cat(html_to_r(rmarkdown_html))
#'
#' @importFrom stringr str_match_all str_replace_all



html_to_r <- function(rmarkdown_html, padding, inc_out) {

  if(missing(padding)) { padding = "\n\n" }
  if(missing(inc_out)) { inc_out = TRUE }

  extract_body <- function(rmarkdown_html) {
    # light-weight replacement for html_nodes()
    as.character(xml2::xml_find_all(xml2::read_html(rmarkdown_html), ".//body"))
  }

  body <- extract_body(rmarkdown_html)

  remove_body_tags <- function(body) {
    # light-weight replacement for html_text()
    inner <- substr(body, 7, nchar(body))
    substr(inner, 1, nchar(inner)-6)
  }

  inner <- remove_body_tags(body)

  # only include |``` if output is to be included
  chunks <- if(inc_out == TRUE) {
    str_match_all(inner, "(```R|```r|```)((.|\\s)*?)```")
  }else{
    str_match_all(inner, "(```R|```r)((.|\\s)*?)```")
  }


  clean_chunks <- function(chunks) {

    # remove first char
    neat_chunks <- substr(chunks[[1]][,3] , 2, nchar(chunks[[1]][,3]))

    # last first char
    substr(neat_chunks , 1, nchar(neat_chunks) - 1)
  }

  neat_chunks <- clean_chunks(chunks)


  replace_character_entities <- function(char_entity){
    xml2::xml_text(xml2::read_html(paste0("<x>", char_entity, "</x>")))
  }

  neat_chunks <- unname(sapply(neat_chunks, replace_character_entities))

  paste0(neat_chunks, collapse=padding)
}









