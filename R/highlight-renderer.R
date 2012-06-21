## stolen from Romain's highlight package (v0.3.2)

# renderers are responsible for rendering the evidence gathered by the 
# parser and the detective. they need to be able to : 
# 
# - translate the token text into the end format : 
#       e.g. > becomes &gt; in html format
#   this particular job is the job of the "translator" function
#  
# - apply the style decided by the detective, e.g surround the token
#       with "<span>" tags for html. 
#   this is the job of the formatter
#                                  
# - translate a spaces into the end format (space)
# - translate a newline character into the end format (newline)
#
# - generate a header, e.g write <html>, css definitions, <body>
# - generate a footer, e.g write </body></html>

# {{{ renderer interface
renderer <- function( translator, formatter, space, newline, header, footer, ... ){
	structure( list( translator = translator, 
		formatter = formatter, space = space, newline = newline, 
		header = header, footer = footer, ... ), 
		class = "renderer" )
}
# }}}

# {{{ renderer implementations
# {{{ html
formatter_html <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '<span class="%s">%s</span>', styles, tokens ) 
	)
}

translator_html <- function( x, size ){
		x <- gsub( '[&]', "&amp;", x )
		x <- gsub( "[<]", "&lt;", x )
		x <- gsub( "[>]", "&gt;", x )
		x
}

space_html <- function( ){
	" "
}

newline_html <- function( ){
	"\n" 
}

header_html <- function( document, stylesheet){
	if( document ){
		cssfile <- getStyleFile( stylesheet )
		function(){
			c( '<html>\n<head>\n<style type="text/css">\n', 
				if( !is.null(cssfile) ) paste( readLines(cssfile), "\n", sep = "") , 
				'</style>\n</head>\n<body>\n<pre>\n' )
		}
	} else {
		function() "<pre>\n"
	}
}

footer_html <- function( document ){
	if( document ){
		function() "\n</pre>\n</body>\n</html>\n"
	} else{
		function() "\n</pre>\n"
	}
}

renderer_html <- function( document = TRUE, 
	translator = translator_html, formatter = formatter_html, 
	space = space_html, newline = newline_html, 
	header = header_html( document, stylesheet ) , 
	footer = footer_html( document ) ,  
	stylesheet = "default", 
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		stylesheet= stylesheet, 
		... )
}
# }}}

# {{{ latex 
formatter_latex <- function( tokens, styles, ... ){
	ifelse( styles == "", 
		tokens, 
		sprintf( '\\hl%s{%s}', styles, tokens ) 
		) 
}

.translator_latex_maker <- function(){
	f <- function( x, size = LATEX_SIZES ){
		size <- match.arg( size )
		s <- function( rx, rep ){
			x <<- gsub( rx, rep, x, fixed = TRUE )
			x <<- gsub( "hlbox", sprintf( "hl%sbox", size ), x, fixed = TRUE )
		}
		# replacement contain open and close braces and backslash
		# so we use this trick
		
		# this wrap is used so that the replacement are not shown in this
		# file, so that it can be rendered as well
		wrap <- function( x ) {
			sprintf( "%s%s%s", paste(rep("@", 3), collapse=""), x, paste(rep("@", 3), collapse="") )
		}
		s( "\\", wrap("bs") )
		s( "{" , wrap("op") )
		s( "}" , "\\usebox{\\hlboxclosebrace}" )
		s( wrap("op") , "\\usebox{\\hlboxopenbrace}" )
		s( wrap("bs") , "\\usebox{\\hlboxbackslash}" )
		
		s( "<"      , "\\usebox{\\hlboxlessthan}" )
		s( ">"      , "\\usebox{\\hlboxgreaterthan}" )
		s( "$"      , "\\usebox{\\hlboxdollar}" )
		s( "_"      , "\\usebox{\\hlboxunderscore}" )
		s( "&"      , "\\usebox{\\hlboxand}")
		s( "#"      , "\\usebox{\\hlboxhash}" )
		s( "@"      , "\\usebox{\\hlboxat}" )
		s( "%"      , "\\usebox{\\hlboxpercent}" )
		s( "^"      , "\\usebox{\\hlboxhat}" )
		s( "~"      , "\\urltilda{}" )
		s( "'"      , "\\usebox{\\hlboxsinglequote}" )
		s( "`"      , "\\usebox{\\hlboxbacktick}" )
		s( " "      , "{\\ }" )
		s( "\n"     , newline_latex() )
		s( '"', '"{}' )
		x
	}
	formals(f)[[2]] <- LATEX_SIZES
	f
}
translator_latex <- .translator_latex_maker()

space_latex <- function( ){
	"{\\ }"
}

newline_latex <- function( ){
	"\\hspace*{\\fill}\\\\\n\\hlstd{}" 
}

boxes_latex <- function( ){


boxes <- '
\\newsavebox{\\hlboxclosebrace}%
\\newsavebox{\\hlboxopenbrace}%
\\newsavebox{\\hlboxbackslash}%
\\newsavebox{\\hlboxlessthan}%
\\newsavebox{\\hlboxgreaterthan}%
\\newsavebox{\\hlboxdollar}%
\\newsavebox{\\hlboxunderscore}%
\\newsavebox{\\hlboxand}%
\\newsavebox{\\hlboxhash}%
\\newsavebox{\\hlboxat}%
\\newsavebox{\\hlboxpercent}% 
\\newsavebox{\\hlboxhat}%
\\newsavebox{\\hlboxsinglequote}%
\\newsavebox{\\hlboxbacktick}%

\\setbox\\hlboxopenbrace=\\hbox{\\verb.{.}%
\\setbox\\hlboxclosebrace=\\hbox{\\verb.}.}%
\\setbox\\hlboxlessthan=\\hbox{\\verb.<.}%
\\setbox\\hlboxdollar=\\hbox{\\verb.$.}%
\\setbox\\hlboxunderscore=\\hbox{\\verb._.}%
\\setbox\\hlboxand=\\hbox{\\verb.&.}%
\\setbox\\hlboxhash=\\hbox{\\verb.#.}%
\\setbox\\hlboxat=\\hbox{\\verb.@.}%
\\setbox\\hlboxbackslash=\\hbox{\\verb.\\.}%
\\setbox\\hlboxgreaterthan=\\hbox{\\verb.>.}%
\\setbox\\hlboxpercent=\\hbox{\\verb.%.}%
\\setbox\\hlboxhat=\\hbox{\\verb.^.}%
\\setbox\\hlboxsinglequote=\\hbox{\\verb.\'.}%
\\setbox\\hlboxbacktick=\\hbox{\\verb.`.}%
\\setbox\\hlboxhat=\\hbox{\\verb.^.}%
'
	allboxes <- paste( lapply( LATEX_SIZES, function( s ){
		out <- gsub( "hlbox", 	sprintf( "hl%sbox", s ), boxes, fixed = TRUE )
		out <- gsub( "hbox{", sprintf("hbox{\\begin{%s}", s ), out, fixed = TRUE )
		out <- gsub( ".}%", sprintf( ".\\end{%s}}%%", s ) , out, fixed = TRUE )
		out
	} ), collapse = "\n\n" )


paste( "
\\usepackage{color}%
", allboxes, '

\\def\\urltilda{\\kern -.15em\\lower .7ex\\hbox{\\~{}}\\kern .04em}%

\\newcommand{\\hlstd}[1]{\\textcolor[rgb]{0,0,0}{#1}}%
\\newcommand{\\hlnum}[1]{\\textcolor[rgb]{0.16,0.16,1}{#1}}
\\newcommand{\\hlesc}[1]{\\textcolor[rgb]{1,0,1}{#1}}
\\newcommand{\\hlstr}[1]{\\textcolor[rgb]{1,0,0}{#1}}
\\newcommand{\\hldstr}[1]{\\textcolor[rgb]{0.51,0.51,0}{#1}}
\\newcommand{\\hlslc}[1]{\\textcolor[rgb]{0.51,0.51,0.51}{\\it{#1}}}
\\newcommand{\\hlcom}[1]{\\textcolor[rgb]{0.51,0.51,0.51}{\\it{#1}}}
\\newcommand{\\hldir}[1]{\\textcolor[rgb]{0,0.51,0}{#1}}
\\newcommand{\\hlsym}[1]{\\textcolor[rgb]{0,0,0}{#1}}
% \\newcommand{\\hlline}[1]{\\textcolor[rgb]{0.33,0.33,0.33}{#1}}
\\newcommand{\\hlkwa}[1]{\\textcolor[rgb]{0,0,0}{\\bf{#1}}}
\\newcommand{\\hlkwb}[1]{\\textcolor[rgb]{0.51,0,0}{#1}}
\\newcommand{\\hlkwc}[1]{\\textcolor[rgb]{0,0,0}{\\bf{#1}}}
\\newcommand{\\hlkwd}[1]{\\textcolor[rgb]{0,0,0.51}{#1}}
' )

}
  
header_latex <- function( document, styles, boxes, minipage = FALSE ){
	function( ){
		txt <- "" ; rm( "txt", envir= environment() )
		con <- textConnection( "txt", open = "w" )
		add <- function( ... ){
			cat( paste( ..., sep = "\n" ), file = con )
		}
		if( document ){
			add( '\\documentclass{article}',
				'\\usepackage{color}', 
				'\\usepackage{alltt}\n\\usepackage{hyperref}',
				paste( styles, collapse = "\n")
				)
		}
		if( document ){
			add( boxes )
			add( '\\begin{document}\n' )
		}
		if( isTRUE(minipage) ){
			add( "\\vspace{1em}\\noindent\\fbox{\\begin{minipage}{0.9\\textwidth}" )
		}
		add( '\\ttfamily\\noindent' )
		close( con )
		paste( txt, "\n", sep = "" )
	}
}

footer_latex <- function( document, minipage = FALSE ){
	extra <- if(isTRUE(minipage)) "\\end{minipage}}\\vspace{1em}" else ""
	if( document ) {
		function() {
			sprintf( "\\mbox{}\n\\normalfont\n%s\\end{document}\n", extra )
		}
	} else{
		function() {
			sprintf( "\\mbox{}\n\\normalfont\n%s", extra )
		}
	}
}

# styler assistant for latex
# 
# @param x output of css parser
styler_assistant_latex <- function( x ){
	
	styles <- sapply( x, function( declaration ) {
		settings <- names( declaration )
		has <- function( setting, value ){
			setting %in% settings && grepl( value, declaration[[ setting ]] )
		}
		start <- ''
		end <- ''
		if( "color" %in% settings ){
			start <- paste( start, '\\textcolor[rgb]{', col2latexrgb( declaration[[ "color" ]] ) , '}{' , sep = "" )
			end <- paste( end, "}", sep = "" )
		}
		if( has( "font-weight", "bold" ) ){
			start <- paste( start, "\\textbf{", sep = "" )
			end <- paste( "}", end, sep = "" )
		}
		if( has( "font-style", "italic" ) ){
			start <- paste( start, "\\textit{", sep = "" )
			end <- paste( "}", end , sep = "" )
		}
		if( has( "text-decoration", "underline" ) ){
			start <- paste( start, "\\underline{", sep = "" )
			end <- paste( "}", end, sep = "" )
		}
		sprintf( "%s#1%s", start, end )
	} )
	sprintf( "\\newcommand{\\hl%s}[1]{%s}%%", names( x ), styles )
}

col2latexrgb <- function( hex ){
	col <- col2rgb(hex)[,1] / 255
	paste( col, collapse = "," )
}


renderer_latex <- function( document = TRUE, 
	boxes = boxes_latex(),
	translator = translator_latex, 
	formatter = formatter_latex, space = space_latex, newline = newline_latex, 
	stylesheet = "default", 
	styles = styler( stylesheet, "sty", styler_assistant_latex ), 
	header = header_latex( document, styles = styles, boxes = boxes, minipage = minipage ), 
	footer = footer_latex( document, minipage = minipage) , 
	minipage = FALSE, 
	... ){
	force( document )
	force( boxes )
	force( styles )
	force( header )
	force( footer )
	renderer( translator = translator, 
		formatter = formatter, space = space , newline = newline, 
		header = header, footer = footer, boxes = boxes, 
		styles = styles, ... )
}
# }}}

# {{{ verbatim 
formatter_verbatim <- function( tokens, styles, ... ){
	tokens
}

translator_verbatim <- function( x, size ){
	x
}

space_verbatim <- function( ){
	" "
}

newline_verbatim <- function( ){
	"\n" 
}

header_verbatim <- NULL
footer_verbatim <- NULL

renderer_verbatim <- function(
	translator = translator_verbatim, formatter = formatter_verbatim, 
	space = space_verbatim, newline = newline_verbatim, 
	header = header_verbatim, footer = footer_verbatim ,  
	... ){
	
	renderer( translator = translator, formatter = formatter, 
		space = space, newline = newline, 
		header = header, footer = footer, 
		... )
}

# }}}
# }}}

# {{{ getStyleFile
# helper function to get a style file
#
# @param name the name of the style file to look for
# @param extension the file extension (css, sty, or xterm)
# 
# @details
# the search goes as follows: first the current working directory
# then the directory ~/.R/highlight, then the stylesheet directory
# in the installed package
# 
# @return the name of the first file that is found, or NULL
getStyleFile <- function( name = "default", extension = "css" ){
	
	filename <- if( grepl( sprintf( "%s$", extension, ignore.case = TRUE), name ) ){
		name
	} else { 
		sprintf( "%s.%s", name, extension )
	}
	
	f <- filename
	if( file.exists( f ) ){
		return(f)
	}
	
	f <- file.path( Sys.getenv("HOME"), ".R", "highlight", filename )
	if( file.exists( f ) ){
		return( f )
	}

	f <- system.file( "stylesheet", filename , package = "highlight" )
	if( file.exists( f )){
		return( f) 
	}
	
	invisible( NULL )
}
# }}}

# {{{ styler
styler <- function( stylesheet, extension = "css", assistant ){
	f <- getStyleFile( stylesheet, extension )
	if( !is.null( f ) ){
		return( readLines( f ) )
	}
	f <- getStyleFile( stylesheet, "css" )
	p <- css.parser( f )
	if( !missing( assistant ) ){
		match.fun(assistant)( p )
	}
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

