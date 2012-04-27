# SET OF HOOKS FOR RESTRUCTURED TEXT ---

#' @rdname hook_plot
#' @export
hook_plot_rst <- function (x, options) {
  if (options$fig.show == "animate") return(.ani.plot.hook.html(x, options))

  base = opts_knit$get("base.url")
  if (is.null(base)) base = ""
  cap = if (is.null(fig.cap <- options$fig.cap)) {
    sprintf("plot of chunk %s", options$label)
  } else {
    if (options$fig.num == 1L) fig.cap[1] else fig.cap[options$fig.cur]
  }
  # TODO: add all options for figure
  # See http://docutils.sourceforge.net/docs/ref/rst/directives.html#image
  # http://docutils.sourceforge.net/docs/ref/rst/directives.html#figure
  make_directive('figure', str_c(base, .upload.url(x)),
                 c(align = options$fig.align, alt = cap,
                   width = options$out.width, height = options$out.height))
}

render_rst <- function (){
	knit_hooks$restore()
	hook.t = function(x, options){
		make_directive('sourcecode', "r", "", content = x)
	}
	hook.o = function(x, options){
		if (output_asis(x, options)) {
			x
		} else {
			hook.t(x, options)
		}
	}
	hook.i = function(x) {
	  sprintf(if (inherits(x, "AsIs"))  "%s"	 else "`%s`", 
	    .inline.hook(format_sci(x, "html")))
	}
	knit_hooks$set(source = hook.t, warning = hook.t, error = hook.t,
		message = hook.t, output = hook.o, inline = hook.i, plot = hook_plot_rst)
}

#' Insert a reStructuredText directive for sphinx
#' 
#' A reSt directive consists of  a name, arguments, option and some content.
#' A typical reSt directive looks like this:
#' .. <name>:: <arguments>
#'    :<option>: <option values>
#'
#'     content
#'
#' This function accepts these arguments and returns the correctly formatted
#' reStructuredText directive
#  Input
#      make_directive('figure', 'fig.png', align = 'center', alt = 'cap')
#  Output
#  .. figure:: fig.png
#      :align: center
#      :alt: cap
make_directive <- function(name, arg, opt, content = ""){
	l1 <- sprintf("\n.. %s:: %s\n", name, arg)
	l2 <- paste(sprintf(":%s: %s", names(opt), opt), collapse = "\n")
	l  <- c(l1, indent_block(l2), indent_block(content))
	paste(l1, indent_block(l2), "\n\n", indent_block(content), sep = "")
}
