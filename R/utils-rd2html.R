#' Knit package documentation
#'
#' Run examples in a package and insert output into the examples code.
#' @param pkg package name
#' @param links a charactor vector of links to be passed to
#'   \code{\link[tools]{Rd2HTML}}
#' @return All HTML pages corresponding to topics in the package are written
#'   under the current working directory. An \file{index.html} is also written
#'   as a table of content.
#' @examples \dontrun{knit_rd('maps')
#' knit_rd('rpart')
#' knit_rd('ggplot2') # time-consuming!
#' }
#' @export
knit_rd = function(pkg, links = tools::findHTMLlinks()) {
  library(pkg, character.only = TRUE)
  optc = opts_chunk$get(); on.exit(opts_chunk$set(optc))
  file.copy(system.file('misc', c('highlight.css', 'highlight.pack.js', 'R.css'), package = 'knitr'), './')
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    message('knitting documentation of ', p)
    tools::Rd2HTML(pkgRdDB[[p]], f <- tempfile(),
            package = pkg, Links = links, no_links = is.null(links), stages = 'render')
    txt = readLines(f, warn = FALSE)
    if (length(i <- grep('<h3>Examples</h3>', txt)) == 1L &&
      length(grep('</pre>', txt[i:length(txt)]))) {
      i0 = grep('<pre>', txt); i0 = i0[i0 > i][1L] - 1L
      i1 = grep('</pre>', txt); i1 = i1[i1 > i0][1L] + 1L
      tools::Rd2ex(pkgRdDB[[p]], ef <- tempfile())
      ex = readLines(ef, warn = FALSE)
      ex = ex[-(1L:grep('### ** Examples', ex, fixed = TRUE))]
      ex = c('```{r}', ex, '```')
      ex = gsub('^(## Not run:\\s*)', '```{r eval=FALSE}\n\\1', ex)
      ex = gsub('^(## End\\(Not run\\)\\s*)', '\\1\n```{r}', ex)
      opts_chunk$set(fig.path = str_c('figure/', p, '-'), tidy = FALSE)
      ex = knit2html(text = ex, envir = parent.frame(2), fragment.only = TRUE)
      txt = c(txt[1:i0], ex, txt[i1:length(txt)])
      txt = sub('</head>', '
<link rel="stylesheet" href="highlight.css">
<script src="highlight.pack.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
</head>', txt)
    } else message('no examples found for ', p)
    writeLines(txt, str_c(p, '.html'))
  }
  unlink('figure/', recursive = TRUE)
  toc = sprintf('- <a href="%s" target="content">%s</a>', str_c(topics, '.html'), topics)
  toc = c(str_c('# ', pkg), '', toc, '',
          paste('Generated with [knitr](http://yihui.name/knitr) ', packageVersion('knitr')))
  markdown::markdownToHTML(text = paste(toc, collapse = '\n'), output = '00frame_toc.html',
                           title = str_c('R Documentation of ', pkg),
                           options = NULL, extensions = NULL, stylesheet = 'R.css')
  file.copy(file.path(find.package(pkg), 'html', '00Index.html'), '.', overwrite = TRUE)
  # fix image links
  txt = readLines('00Index.html')
  writeLines(gsub('../../../doc/html/', 'http://stat.ethz.ch/R-manual/R-devel/doc/html/',
                  txt, fixed = TRUE), '00Index.html')
  writeLines(sprintf(
'<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">
<html>
<head><title>Documentation of the %s package</title></head>
<frameset cols="15%%,*">
  <frame src="00frame_toc.html">
  <frame src="00Index.html" name="content">
</frameset>
</html>
', pkg), 'index.html')
}
