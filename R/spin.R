#' Spin goat's hair into wool
#'
#' This function takes a specially formatted R script and converts it to a
#' literate programming document. By default normal text (documentation) should
#' be written after the roxygen comment (`#'`) and code chunk options are
#' written after `#|` or `#+` or `# \%\%` or `# ----`.
#'
#' Obviously the goat's hair is the original R script, and the wool is the
#' literate programming document (ready to be knitted).
#' @param hair Path to the R script. The script must be encoded in UTF-8 if it
#'   contains multibyte characters.
#' @param knit Whether to compile the document after conversion.
#' @param report Whether to generate a report for \file{Rmd}, \file{Rnw}, and
#'   \file{Rtex} output. Ignored if `knit = FALSE`.
#' @param text A character vector of code, as an alternative way to provide the
#'   R source. If `text` is not `NULL`, `hair` will be ignored.
#' @param envir Environment for [knit()] to evaluate the code.
#' @param format Character; the output format. The default is R Markdown.
#' @param doc A regular expression to identify the documentation lines; by
#'   default it follows the roxygen convention, but it can be customized, e.g.
#'   if you want to use `##` to denote documentation, you can use
#'   `'^##\\\\s*'`.
#' @param inline A regular expression to identify inline R expressions; by
#'   default, code of the form `{{code}}` on its own line is treated as
#'   an inline expression.
#' @param comment A pair of regular expressions for the start and end delimiters
#'   of comments; the lines between a start and an end delimiter will be
#'   ignored. By default, the delimiters are \verb{/*} at the beginning of a
#'   line, and \verb{*/} at the end, following the convention of C comments.
#' @param precious Whether to preserve intermediate files (e.g., `.Rmd`
#'   files when `format` is `"Rmd"`). The default is `FALSE` if
#'   `knit` is `TRUE` and the input is a file.
#' @param engine The default language engine for the code chunks, e.g., `'r'` or
#'   `'python'`. This is useful for spinning a script written in another
#'   language (e.g., a Python `.py` file) so that all chunks use that engine
#'   (via the chunk fence, e.g. \verb{```\{python\}}) without setting
#'   `#+ engine=...` on every chunk. It only applies to the Markdown output
#'   formats (`Rmd` and `qmd`). By default, the engine is guessed from the input
#'   file's extension (e.g., `.py` implies `'python'`, and other extensions
#'   imply `'r'`). A chunk that sets its own `engine` option overrides this
#'   default.
#' @author Yihui Xie, with the original idea from Richard FitzJohn (who named it
#'   as `sowsear()` which meant to make a silk purse out of a sow's ear)
#' @return If `text` is `NULL`, the path of the final output document,
#'   otherwise the content of the output.
#' @note If the output format is `Rnw` and no document class is specified
#'   in roxygen comments, this function will automatically add the
#'   `article` class to the LaTeX document so that it is complete and can
#'   be compiled. You can always specify the document class and other LaTeX
#'   settings in roxygen comments manually.
#'
#'   When the output format is `Rmd`, it is compiled to HTML via
#'   [knit2html()], which uses R Markdown v1 instead of v2. If you
#'   want to use the latter, you should call
#'   [rmarkdown::render()] instead. Similarly, if the
#'   output format is `qmd`, you need to render the output with Quarto.
#' @export
#' @seealso [stitch()] (feed a template with an R script)
#' @references <https://yihui.org/knitr/demo/stitch/>
spin = function(
  hair, knit = TRUE, report = TRUE, text = NULL, envir = parent.frame(),
  format = c('Rmd', 'Rnw', 'Rhtml', 'Rtex', 'Rrst', 'qmd'),
  doc = "^#+'[ ]?", inline = '^[{][{](.+)[}][}][ ]*$',
  comment = c("^[# ]*/[*]", "^.*[*]/ *$"), precious = !knit && is.null(text),
  engine = NULL
) {

  format = match.arg(format)
  x = if (nosrc <- is.null(text)) read_utf8(hair) else split_lines(text)
  if (is.null(engine)) engine = if (nosrc) spin_engine(file_ext(hair)) else 'r'
  stopifnot(length(comment) == 2L)
  c1 = grep(comment[1], x); c2 = grep(comment[2], x)
  check_comments(c1, c2)
  # remove comments
  if (length(c1)) x = x[-unique(unlist(mapply(seq, c1, c2, SIMPLIFY = FALSE)))]

  # remove multiline string literals and symbols (note that this ignores lines with spaces at their
  # beginnings, assuming doc and inline regex don't match these lines anyway)
  is_matchable = function(x) {
    # if code can't be parsed, assume all lines are matchable
    p = tryCatch(parse(text = x, keep.source = TRUE), error = function(e) NULL)
    n = length(x)
    if (is.null(p)) return(rep(TRUE, n))
    d = getParseData(p)
    seq_len(n) %in% d[d$col1 == 1, 'line1']
  }
  matchable = is_matchable(x)

  # .Rmd/.qmd need to be treated specially
  is_md = grepl('^[Rq]md$', format)
  p = if (is_md) .fmt.rmd(x, engine) else .fmt.pat[[tolower(format)]]

  # a chunk header (fence + options); vectorized over opts as a block may
  # contain multiple chunk starts
  chunk_header = function(opts = '') {
    opts = trimws(opts)
    paste0(p[1], ifelse(nzchar(opts), paste0(' ', opts), ''), p[2])
  }

  # turn {{expr}} into inline expressions, e.g. `r expr` or \Sexpr{expr}
  if (any(i <- matchable & grepl(inline, x))) x[i] = gsub(inline, p[4], x[i])

  r = rle((matchable & grepl(doc, x)) | i)  # inline expressions are treated as doc instead of code
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

      rc = '^(#|--)+(\\+| %%| ----+| @knitr)(.*?)\\s*-*\\s*$'
      j1 = grep(rc, block)
      # pipe comments (#|) should start a code chunk if they are not preceded by
      # chunk opening tokens
      j2 = setdiff(pipe_comment_start(block), j1 + 1)

      if (length(j3 <- c(j1, j2))) {
        # the chunk options/label; the `----` token (from purl(documentation =
        # 2)) leaves no space before the label (e.g. `## ----label`), unlike the
        # `#+` token, so trim and re-insert a single space to avoid producing an
        # invalid header like ```{rlabel} that would be parsed as an engine name
        # on the next round-trip (#2014)
        opts = trimws(gsub(rc, '\\3', block[j1]))
        block[j1] = chunk_header(opts)
        block[j2] = paste0(chunk_header(), '\n', block[j2])

        # close each chunk if there are multiple chunks in this block
        if (any(j3 > 1)) {
          j = j3[j3 > 1]
          block[j] = paste0(p[3], '\n', block[j])
        }
      }
      if (!startsWith(block[1L], p[1L])) {
        block = c(chunk_header(), block)
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
.fmt.rmd = function(x, engine = 'r') {
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
  # the fence takes the chunk engine; inline expressions are always R
  c(paste0(b, '{', engine), '}', b, paste0(i, 'r \\1 ', i))
}

# language engine for a script's file extension (default 'r')
spin_engine = function(ext) {
  switch(tolower(ext), py = 'python', jl = 'julia', sh = 'bash', 'r')
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
#' This function is similar to [knit_child()] but is used in R
#' scripts instead. When the main R script is not called via
#' [spin()], this function simply executes the child script via
#' [sys.source()], otherwise it calls [spin()] to spin
#' the child script into a source document, and uses [knit_child()]
#' to compile it. You can call this function in R code, or using the syntax of
#' inline R expressions in [spin()] (e.g.
#' `{{knitr::spin_child('script.R')}}`).
#' @param input Filename of the input R script.
#' @param format Passed to `format` in `spin()`. If not
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

# Check that comment delimiters are correctly paired and ordered. c1/c2 are the
# line numbers of the start (# /*) and end (# */) delimiters, respectively. It is
# not enough to compare length(c1) and length(c2): the delimiters must also be
# interleaved correctly (a start must precede its end), otherwise the lines
# between mis-ordered delimiters would be silently dropped. When any delimiter is
# unmatched, signal an error reporting the offending line numbers.
check_comments = function(c1, c2) {
  # merge and sort delimiters by line number, remembering whether each is a start
  # (TRUE) or end (FALSE); ties (same line matched by both) resolve start-first
  i = order(c(c1, c2), c(rep(0, length(c1)), rep(1, length(c2))))
  lines = c(c1, c2)[i]; starts = c(rep(TRUE, length(c1)), rep(FALSE, length(c2)))[i]
  notes = character(); open = NA
  for (j in seq_along(lines)) {
    if (starts[j]) {
      # a new start before the previous one was closed: previous start is unmatched
      if (!is.na(open)) notes = c(notes, sprintf('  * started on line %d; no end delimiter', open))
      open = lines[j]
    } else {
      if (is.na(open)) {
        notes = c(notes, sprintf('  * no starting delimiter; ended on line %d', lines[j]))
      } else open = NA  # matched a start with this end
    }
  }
  if (!is.na(open)) notes = c(notes, sprintf('  * started on line %d; no end delimiter', open))
  if (length(notes)) stop2(one_string(c(
    'comments must be put in pairs of start and end delimiters.', notes
  )))
}

