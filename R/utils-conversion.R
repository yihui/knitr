#' A wrapper for rst2pdf
#'
#' Convert reST to PDF using \command{rst2pdf} (which converts from rst to PDF
#' using the ReportLab open-source library).
#' @param input The input rst file.
#' @param command Character string giving the path of the
#'   \command{rst2pdf} program. If the program is not in your PATH, the full path has to be
#'   given here.
#' @param options Extra command line options, e.g., `'-v'`.
#' @author Alex Zvoleff and Yihui Xie
#' @return An input file \file{*.rst} will produce \file{*.pdf} and this output
#'   filename is returned if the conversion was successful.
#' @export
#' @seealso [knit2pdf()]
#' @references <https://github.com/rst2pdf/rst2pdf>
rst2pdf = function(input, command = 'rst2pdf', options = '') {
  out = with_ext(input, 'pdf')
  system2(command, paste(shQuote(input), '-o', shQuote(out), options))
  if (file.exists(out)) out else stop('conversion by rst2pdf failed!')
}

#' Convert various input files to various output files using `knit()` and
#' Pandoc
#'
#' Knits the input file and compiles to an output format using Pandoc.
#' @inheritParams knit
#' @param to Character string giving the Pandoc output format to use.
#' @param pandoc_wrapper An R function used to call Pandoc. If `NULL` (the
#'   default), [rmarkdown::pandoc_convert()] will be used
#'   if \pkg{rmarkdown} is installed, otherwise [pandoc()].
#' @param ... Options to be passed to the `pandoc_wrapper` function.
#' @param encoding Ignored (always assumes UTF-8).
#' @author Trevor L. Davis
#' @return Returns the output of the `pandoc_wrapper` function.
#' @export
knit2pandoc = function(
  input, output = NULL, tangle = FALSE, text = NULL, quiet = FALSE,
  envir = parent.frame(), to = 'html', pandoc_wrapper = NULL, ..., encoding = 'UTF-8'
) {
  knit_output = knit(input, output, tangle, text, quiet, envir)
  if (!is.null(pandoc_wrapper)) return(pandoc_wrapper(knit_output, to, ...))
  if (!has_package('rmarkdown')) return(pandoc(knit_output, to, ...))
  output = gsub(paste0(file_ext(knit_output), '$'), to, knit_output)
  rmarkdown::pandoc_convert(knit_output, to, output = output, ...)
}

#' Convert Rnw or Rrst files to PDF
#'
#' Knit the input Rnw or Rrst document, and compile to PDF using
#' [tinytex::latexmk()] or [rst2pdf()].
#' @inheritParams knit
#' @param compiler A character string giving the LaTeX engine used to compile
#'   the tex document to PDF. For an Rrst file, setting `compiler` to
#'   `'rst2pdf'` will use [rst2pdf()] to compile the rst file to
#'   PDF using the ReportLab open-source library. For an Rtyp file, setting
#'   `compiler` to `'typst'` will use the \command{typst} command-line
#'   tool to compile the typ file to PDF.
#' @param ... Options to be passed to [tinytex::latexmk()]
#'   or [rst2pdf()].
#' @author Ramnath Vaidyanathan, Alex Zvoleff and Yihui Xie
#' @return The filename of the PDF file.
#' @note The `output` argument specifies the output filename to be passed
#'   to the PDF compiler (e.g. a tex document) instead of the PDF filename.
#' @export
#' @examples #' compile with xelatex
#' ## knit2pdf(..., compiler = 'xelatex')
#'
#' #' compile a reST file with rst2pdf
#' ## knit2pdf(..., compiler = 'rst2pdf')
#'
#' #' compile an Rtyp file with typst
#' ## knit2pdf(..., compiler = 'typst')
knit2pdf = function(
  input, output = NULL, compiler = NULL, envir = parent.frame(), quiet = FALSE, ...
) {
  out = knit(input, output = output, envir = envir, quiet = quiet)
  owd = setwd(dirname(out)); on.exit(setwd(owd))
  if (is.null(compiler)) {
    compiler = if (grepl('\\.rst$', out)) 'rst2pdf' else
      if (grepl('\\.typ$', out)) 'typst' else 'pdflatex'
  }
  if (identical(compiler, 'rst2pdf')) {
    if (tolower(file_ext(out)) != 'rst')
      stop('for rst2pdf compiler input must be a .rst file')
    rst2pdf(basename(out), ...)
  } else if (identical(compiler, 'typst')) {
    if (tolower(file_ext(out)) != 'typ')
      stop('for typst compiler input must be a .typ file')
    f = basename(out); o = with_ext(f, 'pdf')
    status = system2('typst', c('compile', shQuote(f), shQuote(o)))
    if (!file.exists(o)) stop(
      'compilation by typst failed (exit status ', status, '); ',
      'make sure typst is installed and available in your PATH'
    )
  } else {
    tinytex::latexmk(basename(out), engine = compiler, ...)
  }
  with_ext(out, 'pdf')
}

#' Convert an \file{Rnw} document to PDF
#'
#' Call [knit()] to compile the \file{.Rnw} input to \file{.tex},
#' and then [tinytex::latexmk()] to convert \file{.tex} to
#' \file{.pdf}.
#'
#' This function is similar to [knit2pdf()], with the following differences:
#' \enumerate{
#'   \item The default compiler is "xelatex" instead of "pdflatex".
#'   \item `output` uses the file extension ".pdf" instead of ".tex".
#'   \item Before knitting, it tries to remove the `output` file and will throw a clear error if the file cannot be removed.
#'   \item `output` could be under any dir, not necessarily the same directory as `input`.
#'   \item It cleans up intermediate files by default, including the ".tex" file.
#'   \item It stops knitting when any error occurs (by setting the chunk option `error = FALSE`).
#' }
#' @inheritParams knit
#' @param output Path of the PDF output file. By default, it uses the same name
#'   as the `input`, but changes the file extension to ".pdf".
#' @param compiler,... The LaTeX engine and other arguments to be passed to
#'   [tinytex::latexmk()]. The default compiler is
#'   `xelatex`.
#' @param clean If `TRUE`, the intermediate files will be removed.
#' @param error If `FALSE`, knitting stops when any error occurs.
#' @return The `output` file path.
#' @export
rnw2pdf = function(
  input, output = with_ext(input, 'pdf'), compiler = 'xelatex',
  envir = parent.frame(), quiet = FALSE, clean = TRUE, error = FALSE, ...
) {
  # On Windows, when tweaking the content, users may forget to close the PDF
  # file (thus can't be written). Since knitting may take quite some time, it's
  # better to check the write permission of the output file in advance.
  if (file_exists(output) && !file.remove(output)) stop(
    "The file '", output, "' cannot be removed (may be locked by a PDF reader)."
  )
  old = opts_chunk$set(error = error)
  on.exit(opts_chunk$set(old), add = TRUE)
  file_tex = knit(input, envir = envir, quiet = quiet)
  if (clean) on.exit(file.remove(file_tex), add = TRUE)
  file_pdf = tinytex::latexmk(file_tex, engine = compiler, clean = clean, ...)
  if (!xfun::same_path(output, file_pdf)) file.rename(file_pdf, output)
  output
}

# Render Markdown to a standalone HTML document via litedown. The markdown
# package's mark_html() is a thin wrapper of litedown::mark() that forces the
# HTML template on (to produce a full document instead of a fragment); we
# replicate that here so knitr no longer depends on the markdown package.
mark_html = function(..., template = TRUE) {
  opts = options(litedown.html.template = template)
  on.exit(options(opts), add = TRUE)
  litedown::mark(...)
}

#' Convert markdown to HTML using knit() and litedown::mark()
#'
#' This is a convenience function to knit the input markdown source and call
#' [litedown::mark()] to convert the result to HTML.
#' @inheritParams knit
#' @param ... Options passed to [litedown::mark()].
#' @param force_v1 Whether to force rendering the input document as an R
#'   Markdown v1 document, even if it is for v2.
#' @export
#' @seealso [knit()], [litedown::mark()]
#' @return If the argument `text` is NULL, a character string (HTML code)
#'   is returned; otherwise the result is written into a file and the filename
#'   is returned.
#' @note This function renders R Markdown v1, which is much less powerful than R
#'   Markdown v2, i.e. the \pkg{rmarkdown} package
#'   (<https://rmarkdown.rstudio.com>). To render R Markdown v2 documents to
#'   HTML, please use `rmarkdown::render()` instead. For a lighter-weight
#'   alternative that handles figure paths robustly, see
#'   [litedown::fuse()].
#' @examples # a minimal example
#' writeLines(c("# hello markdown", '```{r hello-random, echo=TRUE}', 'rnorm(5)', '```'), 'test.Rmd')
#' knit2html('test.Rmd')
#' if (interactive()) browseURL('test.html')
#'
#' unlink(c('test.Rmd', 'test.html', 'test.md'))
knit2html = function(
  input, output = NULL, ..., envir = parent.frame(), text = NULL,
  quiet = FALSE, encoding = 'UTF-8', force_v1 = getOption('knitr.knit2html.force_v1', FALSE)
) {
  if (is_cran_check() && !has_package('litedown'))
    return(vweave_empty(input, .reason = 'litedown'))

  if (!force_v1 && is.null(text)) {
    # test if an Rmd input should be rendered via rmarkdown::render() instead
    res = xfun::yaml_body(read_utf8(input))$yaml[['output']]
    if (is.list(res)) res = names(res)
    rmd_v2 = length(res) > 0 && is.character(res) &&
      !any(grepl('^litedown::', res) | res == 'html') &&
      !any(grepl('^markdown::', res))
    if (rmd_v2) warning2(
      'It seems you should call rmarkdown::render() instead of knitr::knit2html() ',
      'because ', input, ' appears to be an R Markdown v2 document.'
    )
  }
  out = knit(input, text = text, envir = envir, quiet = quiet)
  if (is.null(text)) {
    output = with_ext(if (is.null(output) || is.na(output)) out else output, 'html')
    # mark() resolves relative resource paths (e.g. figures) against the output
    # directory, but knit() writes them next to the input; when the output goes
    # to a different directory, render next to the input first (so resources can
    # be found/embedded), then move the HTML to the output location (#2408)
    if (xfun::same_path(dirname(out), dirname(output))) {
      mark_html(out, output, ...)
    } else {
      html = with_ext(out, 'html')
      on.exit(if (!xfun::same_path(html, output)) file.remove(html), add = TRUE)
      mark_html(out, html, ...)
      file.copy(html, output, overwrite = TRUE)
    }
    invisible(output)
  } else mark_html(text = out, ...)
}

#' Knit an R Markdown document and post it to WordPress
#'
#' This function is a wrapper around the \pkg{RWordPress} package. It compiles
#' an R Markdown document to HTML and post the results to WordPress. Please note
#' that \pkg{RWordPress} has not been updated for several years, which is
#' [not a good sign](https://github.com/yihui/knitr/issues/1866). For
#' blogging with R, you may want to try the \pkg{blogdown} package instead.
#' @param input Filename of the Rmd document.
#' @param title Title of the post.
#' @param ... Other meta information of the post, e.g., `categories =
#'   c('R', 'Stats')` and `mt_keywords = c('knitr', 'wordpress')`, etc.
#' @param shortcode A length-2 logical vector indicating whether to use the
#'   shortcode `[sourcecode lang='lang']`, which can be useful to
#'   WordPress.com users for syntax highlighting of source code and output. The
#'   first element
#'   applies to source code, and the second applies to text output. By default,
#'   both are `FALSE`.
#' @param action Whether to create a new post, update an existing post, or
#'   create a new page.
#' @param postid If `action` is `editPost`, the post id `postid`
#'   must be specified.
#' @param publish Whether to publish the post immediately.
#' @inheritParams knit
#' @export
#' @references <https://yihui.org/knitr/demo/wordpress/>
#' @author William K. Morris, Yihui Xie, and Jared Lander
#' @note This function will convert the encoding of the post and the title to
#'   UTF-8 internally. If you have additional data to send to WordPress (e.g.
#'   keywords and categories), you may have to manually convert them to the
#'   UTF-8 encoding with the \code{\link{iconv}(x, to = 'UTF-8')} function
#'   (especially when using Windows).
#' @examples # see the reference
knit2wp = function(
  input, title = 'A post from knitr', ..., envir = parent.frame(), shortcode = FALSE,
  action = c('newPost', 'editPost', 'newPage'), postid, publish = TRUE
) {
  do.call('library', list(package = 'RWordPress', character.only = TRUE))
  xfun::do_once(
    warning2(
      'This function is based on the RWordPress package, which is no longer actively ',
      'maintained (https://github.com/yihui/knitr/issues/1866). For blogging with R, ',
      'you may try the blogdown package instead.'
    ), 'knitr.knit2wp.warning'
  )
  out = knit(input, envir = envir); on.exit(unlink(out))
  content = read_utf8(out)
  if (missing(title) && length(title2 <- xfun::yaml_body(content)$yaml$title) == 1)
    title = title2
  content = litedown::mark(text = content)
  shortcode = rep(shortcode, length.out = 2L)
  if (shortcode[1]) content = gsub(
    '<pre><code class="([[:alpha:]]+)">(.+?)</code></pre>',
    '[sourcecode language="\\1"]\\2[/sourcecode]', content
  )
  content = gsub(
    '<pre><code( class="no-highlight"|)>(.+?)</code></pre>',
    if (shortcode[2]) '[sourcecode]\\2[/sourcecode]' else '<pre>\\2</pre>', content
  )

  content = enc2utf8(content)
  title = enc2utf8(title)

  # figure out if we are making a newPost or overwriting an existing post
  action = match.arg(action)

  # build a list of arguments to be fed into either newPost or editPost
  # the first argument is the content, which itself is a list containing
  #     description
  #     title
  #     ...
  # then there is the publish argument
  WPargs = list(content = list(description = content, title = title, ...), publish = publish)

  # if we are editing the post, also include the argument for postid
  if (action == "editPost") WPargs = c(postid = postid, WPargs)

  do.call(action, args = WPargs)
}

#' Watch an input file continuously and knit it when it is updated
#'
#' Check the modification time of an input file continuously in an infinite loop.
#' Whenever the time indicates the file has been modified, call a function to
#' recompile the input file.
#'
#' This is actually a general function not necessarily restricted to
#' applications in \pkg{knitr}. You may specify any `compile` function to
#' process the `input` file. To stop the infinite loop, press the
#' `Escape` key or `Ctrl + C` (depending on your editing environment
#' and operating system).
#' @param input An input file path, or a character vector of multiple input file paths.
#' @param compile A function to compile the `input` file. This could be e.g.
#'   [knit()] or [knit2pdf()], depending on the input file
#'   and the output you want.
#' @param interval A time interval to pause in each cycle of the infinite loop.
#' @param ... Other arguments to be passed to the `compile` function.
#' @export
#' @examples # knit_watch('foo.Rnw', knit2pdf)
#'
#' # knit_watch('foo.Rmd', rmarkdown::render)
knit_watch = function(input, compile = knit, interval = 1, ...) {
  mtime = function(...) file.info(...)[, 'mtime']
  last_time = mtime(input)
  updated = function() {
    this_time = mtime(input)
    on.exit(last_time <<- this_time, add = TRUE)
    this_time > last_time
  }
  for (f in input) compile(f, ...)
  while (TRUE) {
    for (f in input[updated()]) compile(f, ...)
    Sys.sleep(interval)
  }
}
