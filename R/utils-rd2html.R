#' Knit package documentation
#'
#' Run examples in a package and insert output into the examples code.
#' @param pkg package name
#' @return All HTML pages corresponding to topics in the package are written
#'   under the current working directory. An \file{index.html} is also written
#'   as a table of content.
#' @examples \dontrun{knit_rd('maps')
#' knit_rd('rpart')}
#' @export
knit_rd = function(pkg) {
  library(pkg, character.only = TRUE)
  path = find.package(pkg)
  objs = readRDS(file.path(path, 'help', 'aliases.rds'))
  tits = character()
  optc = opts_chunk$get(); on.exit(opts_chunk$set(optc))
  file.copy(system.file('misc', c('highlight.css', 'highlight.pack.js', 'R.css'), package = 'knitr'), './')
  for (p in unique(objs)) {
    message('knitting documentation of ', p)
    hf = utils:::.getHelpFile(file.path(path, 'help', p))
    tools::Rd2HTML(hf, f <- tempfile(), package = pkg)
    txt = readLines(f, warn = FALSE)
    tits[p] = gsub('(.*<h2>)([^<]*)(</h2>.*)', '\\2', paste(txt, collapse = '\n'))
    if (length(i <- grep('<h3>Examples</h3>', txt)) == 1L &&
      length(grep('</pre>', txt[i:length(txt)]))) {
      i0 = grep('<pre>', txt); i0 = i0[i0 > i][1L] - 1L
      i1 = grep('</pre>', txt); i1 = i1[i1 > i0][1L] + 1L
      tools::Rd2ex(hf, ef <- tempfile())
      ex = readLines(ef, warn = FALSE)
      ex = ex[-(1L:grep('### ** Examples', ex, fixed = TRUE))]
      ex = c('```{r}', ex, '```')
      ex = gsub('^(## Not run:\\s*)', '```{r eval=FALSE}\n\\1', ex)
      ex = gsub('^(## End\\(Not run\\)\\s*)', '\\1\n```{r}', ex)
      opts_chunk$set(fig.path = str_c('figure/', p), tidy = FALSE)
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
  tits = str_trim(tits)
  toc = sprintf('- [%s](%s): %s', names(objs), str_c(objs, '.html'), tits[objs])
  toc = c(str_c('# Help Pages of ', pkg), '', toc, '',
          paste('Generated with [knitr](http://yihui.name/knitr) ', packageVersion('knitr')))
  markdown::markdownToHTML(text = paste(toc, collapse = '\n'), output = 'index.html',
                           title = str_c('R Documentation of ', pkg),
                           fragment.only = TRUE)
}
