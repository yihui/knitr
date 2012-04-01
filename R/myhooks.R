# SET OF HOOKS FOR RESTRUCTURED TEXT ---
hook_plot_rst <- function (x, options) {
	if (options$fig.show == "animate") {
		.ani.plot.hook.html(x, options)
	}
	else {
		base = opts_knit$get("base.url")
		if (is.null(base)) 
			base = ""
		cap = if (is.null(fig.cap <- options$fig.cap)) {
			sprintf("plot of chunk %s", options$label)
		} else {
			if (options$fig.num == 1L) 
				fig.cap[1]
			else fig.cap[options$fig.cur]
		}
		make_directive('figure', .upload.url(x), 
			list(align = options$fig.align, alt = cap))
	}
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

#' Indents a Block
#' 
#  Input
#     "library(ggplot2)\nqplot(wt, mpg, data = mtcars)"
#  Output
#          library(ggplot2)
#          qplot(wt, mpg, data  = mtcars)
indent_block <- function(block, nspaces = 4){
	.pad   <- paste(rep(" ", nspaces), collapse = "")
	.lines <- readLines(textConnection(block))
	paste(.pad, .lines, collapse = "\n")
}

