
# Functions in this file are derived from utils::txtProgressBar(),
# author: R Core Team and contributors worldwide

knit_progress <- function (
  max = 1
  , title
  , message
  , style = 1
) {

  .val <- 0
  .text <- character(1L)
  .killed <- FALSE
  .nb <- 0L
  .pc <- -1L
  char = "."

  width <- floor(getOption("width") * 2/5)
  text_width <- getOption("width") - width - 8


  up <- function(value, text, carriage_return = "\r") {

    nb <- round(width * value/max)
    pc <- round(100 * value/max)

    if (nb == .nb && pc == .pc && text == .text) return()
    if (!is.finite(value) || value > max) stop("knitr::knit_progress() 'value' is ", value)

    if(isTRUE(knitr::opts_knit$get("verbose")) && text != .text) carriage_return <- "\n"

    .text <<- text
    .val <<- value


    cat(
      carriage_return
      , strtrim(encodeString(text, width = text_width), width = text_width)
      , "|"
      , rep.int(char, nb)
      , rep.int(" ", width - nb)
      , sprintf("| %3d%%", pc)
      , sep = ""
    )
    flush.console()
    .nb <<- nb
    .pc <<- pc
  }
  getVal <- function() .val
  getText <- function() .text
  kill <- function() if (!.killed) {
    cat("\n")
    flush.console()
    .killed <<- TRUE
  }

  up(value = 0, text = "Knitting...", carriage_return = "\n")
  structure(list(getVal = getVal, getText = getText, up = up, kill = kill), class = "txtProgressBar")
}


set_knit_progress <- function(x, ...) {
  UseMethod("set_knit_progress")
}

set_knit_progress.character <- function(x, ...) {
  x <- paste0(x, ..., collapse = "")

  pb <- getOption("knitr.knit_progress")
  if(is.null(pb)) return()
  oldval <- pb$getVal()
  oldtext <- pb$getText()
  pb$up(value = oldval, text = x)
  invisible(oldval)
}

set_knit_progress.numeric <- function(x, ...) {
  pb <- getOption("knitr.knit_progress")
  if(is.null(pb)) return()
  oldval <- pb$getVal()
  oldtext <- pb$getText()
  pb$up(value = x, text = oldtext)
  invisible(oldval)
}
