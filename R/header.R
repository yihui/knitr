## doc is the output of processed document
insert_header <- function(doc){
  if (is.null(b <- knit_patterns$get('header.begin'))) return(doc)

  fmt = opts_knit$get('out.format')
	switch(fmt, 
		html  = insert_header_html(doc, b),
		latex = insert_header_latex(doc, b),
		doc
	)
}

## Makes latex header with macros required for highlighting, tikz and framed
make_header_latex <- function(){
	h <- "\\usepackage{graphicx, color}"
	h <- paste(c(h, opts_knit$get('header')), collapse = "\n")
	if (opts_knit$get('self.contained')){
		return(h)
	} else {
		writeLines(h, 'knitr.sty')
		return('\\usepackage{knitr}')
	}
}

insert_header_latex <- function(doc, b) {
	h   <- make_header_latex()
	i   <- which(str_detect(doc, b))
	l   <- str_locate(doc[i], b)
	if (length(i) == 1L){
		tmp <- str_sub(doc[i], l[, 1], l[, 2])
		str_sub(doc[i], l[,1], l[,2]) <- str_c(tmp, "\n", h)
	} else if (length(i) == 0L) {
		doc <- str_c(getOption('tikzDocumentDeclaration'), h, .knitEnv$packages,
			"\\begin{document}", doc, "\\end{document}")
	}
	return(doc)
}

make_header_html <- function(){
	h <- opts_knit$get('header')[['highlight']]
	if (opts_knit$get('self.contained')){
		h <- str_c('<style type="text/css">', h, '</style>', collapse = "\n")
		return(h)
	} else {
		writeLines(h, 'knitr.css')
		return('<link rel="stylesheet" href="knitr.css" type="text/css" />')
	}
}

insert_header_html <- function(doc, b) {
	h <- make_header_html()
	i <- which(str_detect(doc, b))
	l <- str_locate(doc[i], b)
	if (length(i) == 1L){
		tmp <- str_sub(doc[i], l[, 1], l[, 2])
		str_sub(doc[i], l[,1], l[,2]) <- str_c(tmp, "\n", h)
	}
	return(doc)
}

#' Set the header information
#' 
#' Some output documents may need appropriate header information, for example,
#' for LaTeX output, we need to write \samp{\\usepackage{tikz}} into the
#' preamble if we use tikz graphics; this function sets the header information
#' to be written into the output.
#' 
#' By default, \pkg{knitr} will set up the header automatically. For example, if
#' the tikz device is used, \pkg{knitr} will add \samp{\\usepackage{tikz}} to
#' the LaTeX preamble, and this is done by setting the header component
#' \code{tikz} to be a character string: \code{set_header(tikz =
#' '\\usepackage{tikz}')}. Similary, when we highlight R code using the
#' \pkg{highlight} package (i.e. the chunk option \code{highlight = TRUE}),
#' \pkg{knitr} will set the \code{highlight} component of the header vector 
#' automatically; if the output type is HTML, this component will be different
#' -- instead of LaTeX commands, it contains CSS definitions.
#' 
#' For power users, all the components can be modified to adapt to a customized
#' type of output. For instance, we can change \code{highlight} to LaTeX
#' definitions of the \pkg{listings} package (and modify the output hooks
#' accordingly), so we can decorate R code using the \pkg{listings} package.
#' @param ... the header components; currently possible components are
#'   \code{highlight}, \code{tikz} and \code{framed}, which contain the
#'   necessary commands to be used in the HTML header or LaTeX preamble; note
#'   HTML output only uses the \code{highlight} component (the other two are
#'   ignored)
#' @return The header vector in \code{opts_knit} is set.
#' @export
#' @examples set_header(tikz = '\\usepackage{tikz}')
#' opts_knit$get('header')
set_header = function(...) {
  h = opts_knit$get('header')
  z = c(...)
  h[names(z)] = z
  opts_knit$set(header = h)
}

