#' Spin goat's hair into wool
#'
#' This function takes a specially formatted R script and converts it to a
#' literate programming document. By default normal text (documentation) should
#' be written after the roxygen comment (\code{#'}) and code chunk options are
#' written after \code{#+} or \code{# \%\%} or \code{#-} or \code{# ----} or
#' any of these combinations replacing \code{#} with \code{--}.
#'
#' Obviously the goat's hair is the original R script, and the wool is the
#' literate programming document (ready to be knitted).
#' @param hair Path to the R script. The script must be encoded in UTF-8 if it
#'   contains multibyte characters.
#' @param knit Logical; whether to compile the document after conversion.
#' @param report Logical; whether to generate a report for \file{Rmd},
#'   \file{Rnw} and \file{Rtex} output. Ignored if \code{knit = FALSE}.
#' @param text A character vector of code, as an alternative way to provide the
#'   R source. If \code{text} is not \code{NULL}, \code{hair} will be ignored.
#' @param envir Environment for \code{\link{knit}()} to evaluate the code.
#' @param format Character; the output format. The default is R Markdown.
#' @param doc A regular expression to identify the documentation lines; by
#'   default it follows the roxygen convention, but it can be customized, e.g.
#'   if you want to use \code{##} to denote documentation, you can use
#'   \code{'^##\\\\s*'}.
#' @param inline A regular expression to identify inline R expressions; by
#'   default, code of the form \code{\{\{code\}\}} on its own line is treated as
#'   an inline expression.
#' @param comment A pair of regular expressions for the start and end delimiters
#'   of comments; the lines between a start and an end delimiter will be
#'   ignored. By default, the delimiters are \verb{/*} at the beginning of a
#'   line, and \verb{*/} at the end, following the convention of C comments.
#' @param precious logical: whether intermediate files (e.g., \code{.Rmd} files
#'   when \code{format} is \code{"Rmd"}) should be preserved. The default is
#'   \code{FALSE} if \code{knit} is \code{TRUE} and the input is a file.
#' @author Yihui Xie, with the original idea from Richard FitzJohn (who named it
#'   as \code{sowsear()} which meant to make a silk purse out of a sow's ear)
#' @return If \code{text} is \code{NULL}, the path of the final output document,
#'   otherwise the content of the output.
#' @note If the output format is \code{Rnw} and no document class is specified
#'   in roxygen comments, this function will automatically add the
#'   \code{article} class to the LaTeX document so that it is complete and can
#'   be compiled. You can always specify the document class and other LaTeX
#'   settings in roxygen comments manually.
#'
#'   When the output format is \code{Rmd}, it is compiled to HTML via
#'   \code{\link{knit2html}()}, which uses R Markdown v1 instead of v2. If you
#'   want to use the latter, you should call
#'   \code{rmarkdown::\link[rmarkdown]{render}()} instead. Similarly, if the
#'   output format is \code{qmd}, you need to render the output with Quarto.
#' @export
#' @seealso \code{\link{stitch}} (feed a template with an R script)
#' @references \url{https://yihui.org/knitr/demo/stitch/}
spin = function(
  hair, knit = TRUE, report = TRUE, text = NULL, envir = parent.frame(),
  format = c('Rmd', 'Rnw', 'Rhtml', 'Rtex', 'Rrst', 'qmd'),
  doc = "^#+'[ ]?", inline = '^[{][{](.+)[}][}][ ]*$',
  comment = c("^[# ]*/[*]", "^.*[*]/ *$"), precious = !knit && is.null(text)
) {

  format = match.arg(format)
  x = if (nosrc <- is.null(text)) read_utf8(hair) else split_lines(text)
  stopifnot(length(comment) == 2L)
  c1 = grep(comment[1], x); c2 = grep(comment[2], x)
  if (length(c1) != length(c2))
    stop('comments must be put in pairs of start and end delimiters')
  # remove comments
  if (length(c1)) x = x[-unique(unlist(mapply(seq, c1, c2, SIMPLIFY = FALSE)))]

  # remove multiline string literals and symbols (note that this ignores lines with spaces at their
  # beginnings, assuming doc and inline regex don't match these lines anyway)
  parsed_data = getParseData(parse(text = x, keep.source = TRUE))
  is_matchable = seq_along(x) %in% unique(parsed_data[parsed_data$col1 == 1, 'line1'])

  # .Rmd/.qmd need to be treated specially
  is_md = grepl('^[Rq]md$', format)
  p = if (is_md) .fmt.rmd(x) else .fmt.pat[[tolower(format)]]

  # turn {{expr}} into inline expressions, e.g. `r expr` or \Sexpr{expr}
  if (any(i <- is_matchable & grepl(inline, x))) x[i] = gsub(inline, p[4], x[i])

  r = rle((is_matchable & grepl(doc, x)) | i)  # inline expressions are treated as doc instead of code
  n = length(r$lengths); txt = vector('list', n); idx = c(0L, cumsum(r$lengths))

  for (i in seq_len(n)) {
    block = x[seq(idx[i] + 1L, idx[i + 1])]
    txt[[i]] = if (r$values[i]) {
      # normal text; just strip #'
      sub(doc, '', block)
    } else {
      # R code; #+/- indicates chunk options
      block = strip_white(block) # rm white lines in beginning and end
      if (!length(block)) next

      rc <- '^(#|--)+(\\+|-|\\s+%%| ----+| @knitr)'
      opt = grep(rc, block)
      # pipe comments (#|) should start a code chunk if they are not preceded by
      # chunk opening tokens
      if (format == 'qmd') {
        j = setdiff(pipe_comment_start(block), opt + 1)
        # add the token '# %%' before the starting pipe comment
        block[j] = paste0('# %%\n', block[j])
        opt = c(opt, j)
      }

      if (length(opt)) {
        opts = gsub(paste0(rc, '(-*\\s*$|\n.*)'), '', block[opt])
        opts = paste0(ifelse(opts == '', '', ' '), opts)
        # add chunk headers with options (special case: '# %%\n#| ...')
        block[opt] = paste0(
          p[1L], opts, p[2L],
          ifelse(grepl('\n', block[opt]), gsub('.*?(\n.+)', '\\1', block[opt]), '')
        )
        # close each chunk if there are multiple chunks in this block
        if (any(opt > 1)) {
          j = opt[opt > 1]
          block[j] = paste(p[3L], block[j], sep = '\n')
        }
      }
      if (!startsWith(block[1L], p[1L])) {
        block = c(paste0(p[1L], p[2L]), block)
      }
      c('', block, p[3L], '')
    }
  }

  txt = unlist(txt)
  is_tex = grepl('^R(nw|tex)$', format)
  # make it a complete TeX document if document class not specified
  if (report && is_tex && !any(grepl('^\\s*\\\\documentclass', txt))) {
    txt = c('\\documentclass{article}', '\\begin{document}', txt, '\\end{document}')
  }
  if (nosrc) {
    outsrc = with_ext(hair, format)
    write_utf8(txt, outsrc)
    txt = NULL
  } else outsrc = NULL
  if (!knit) return(txt %n% outsrc)

  out = if (report) {
    if (is_md) {
      knit2html(outsrc, text = txt, envir = envir)
    } else if (!is.null(outsrc) && is_tex) {
      knit2pdf(outsrc, envir = envir)
    }
  } else knit(outsrc, text = txt, envir = envir)

  if (!precious && !is.null(outsrc)) file.remove(outsrc)
  invisible(out)
}

.fmt.pat = list(
  rnw = c('<<', '>>=', '@', '\\\\Sexpr{\\1}'),
  rhtml = c('<!--begin.rcode', '', 'end.rcode-->', '<!--rinline \\1 -->'),
  rtex = c('% begin.rcode', '', '% end.rcode', '\\\\rinline{\\1}'),
  rrst = c('.. {r', '}', '.. ..', ':r:`\\1`')
)

# determine how many backticks we need to wrap code blocks and inline code
.fmt.rmd = function(x) {
  x = one_string(x)
  l = attr(gregexpr('`+', x)[[1]], 'match.length')
  l = max(l, 0)
  if (length(l) > 0) {
    i = strrep('`', l + 1)
    b = strrep('`', max(l + 1, 3))
  } else {
    i = '`'
    b = '```'
  }
  c(paste0(b, '{r'), '}', b, paste0(i, 'r \\1 ', i))
}

# find the position of the starting `#|` in a consecutive block of `#|` comments
pipe_comment_start = function(x) {
  i = startsWith(x, '#| ')
  r = rle(i)
  l = r$lengths
  j = cumsum(l) - l + 1
  j[r$values]
}

#' Spin a child R script
#'
#' This function is similar to \code{\link{knit_child}()} but is used in R
#' scripts instead. When the main R script is not called via
#' \code{\link{spin}()}, this function simply executes the child script via
#' \code{\link{sys.source}()}, otherwise it calls \code{\link{spin}()} to spin
#' the child script into a source document, and uses \code{\link{knit_child}()}
#' to compile it. You can call this function in R code, or using the syntax of
#' inline R expressions in \code{\link{spin}()} (e.g.
#' \code{{{knitr::spin_child('script.R')}}}).
#' @param input Filename of the input R script.
#' @param format Passed to \code{format} in \code{spin()}. If not
#'   provided, it will be guessed from the current knitting process.
#' @return A character string of the knitted R script.
#' @export
spin_child = function(input, format) {
  if (!isTRUE(getOption('knitr.in.progress')))
    return(sys.source(input, parent.frame()))
  fmt = if (missing(format)) {
    if (is.null(fmt <- out_format()))
      stop('spin_child() must be called in a knitting process')
    .spin.fmt = c(
      'latex' = 'Rnw', 'sweave' = 'Rnw', 'listings' = 'Rnw',
      'html' = 'Rhtml', 'markdown' = 'Rmd'
    )
    if (is.na(fmt <- .spin.fmt[fmt]))
      stop('the document format ', fmt, ' is not supported yet')
    fmt
  } else format
  asis_output(knit_child(
    text = spin(text = read_utf8(input), knit = FALSE, report = FALSE, format = fmt),
    quiet = TRUE
  ))
}
