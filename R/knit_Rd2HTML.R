#' Rd2HTML with knitr
#'
#' \code{Rd} Rd files;
#' \code{extra} options of knitr
#' \code{package} package name
#' @export
knit_Rd2HTML <- function(Rd, extra = "", package) {
    Rd2html <- function(Rd, extra) {
        base <- file_path_sans_ext(Rd)
        out <- paste(base, "Rhtml", sep = ".")
        file.ex.R <- paste(base, "-examples.R", sep = ".")
        tools::Rd2HTML(Rd, out = out, stylesheet = "stylesheet.css")
        Rd2ex(Rd, file.ex.R)
        ex.R <- readLines(file.ex.R)
        ex.R <- c(paste("<!--begin.rcode", extra), ex.R, "end.rcode-->", sep = "\n")
        Rhtml <- readLines(out)
        Rhtml <- c(Rhtml[seq_len(grep("<h3>Examples</h3>", Rhtml, fixed = TRUE))], ex.R, 
		           Rhtml[(max(grep("</pre>", Rhtml, fixed = TRUE)) + 1):length(Rhtml)])
        Rhtml <- gsub("## Not run:", "end.rcode--> \n\n <!--begin.rcode eval=FALSE", Rhtml)
        Rhtml <- gsub("## End(Not run)", paste("end.rcode--> <!--begin.rcode", extra), Rhtml)
        writeLines(Rhtml, out)
        file.html <- knit(out)
        
		## Pull contents of first matched tag from parsed Rd file
		get.tag <- function(tag, parseRd){
			for (x in parseRd) if(attr(x, "Rd_tag") == tag) return(x)
			stop("didn't find tag")
		}
        tmp <- parse_Rd(Rd)
        list(name = unlist(get.tag("\\name", tmp)), title = unlist(get.tag("\\title", tmp)), file = file.html)
    }

    info <- lapply(Rd, function(x) Rd2html(x, extra = extra))
	
    if (length(Rd) > 1) {
        contents <- sapply(info, function(x) sprintf("* [%s](%s) %s", 
		                   x$name, x$file, paste(x$title, collapse = "")))
        contents <- gsub("\n", " ", contents)
        contents <- c(sprintf("# Help files for %s", package), contents)
        
        writeLines(paste(contents, collapse = "\n\n"), "index.md")
        markdown::markdownToHTML("index.md", output="index.html", stylesheet = "stylesheet.css")
        file.remove("index.md")
    }
    
    
    
    ## Default stylesheet, from pandoc's tango theme, plus very minimal
    ## page css styling.  Will be saved as stylesheet.css if it does not
    ## exist.
    default.stylesheet <- "/* Highlighting from pandoc / tango */
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
    margin: 0; padding: 0; vertical-align: baseline; border: none; }
    table.sourceCode { width: 100%; background-color: #f8f8f8; }
    td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
    td.sourceCode { padding-left: 5px; }
    pre, code { background-color: #f8f8f8; }
    code > span.kw { color: #204a87; font-weight: bold; }
    code > span.dt { color: #204a87; }
    code > span.dv { color: #0000cf; }
    code > span.bn { color: #0000cf; }
    code > span.fl { color: #0000cf; }
    code > span.ch { color: #4e9a06; }
    code > span.st { color: #4e9a06; }
    code > span.co { color: #8f5902; font-style: italic; }
    code > span.ot { color: #8f5902; }
    code > span.al { color: #ef2929; }
    code > span.fu { color: #000000; }
    code > span.er { font-weight: bold; }
    
    body { font-family: Helvetica, sans-serif;
    color: #333; 
    padding: 0 5px; 
    margin: 0 auto; 
    font-size: 14px;
    width: 80%;
    max-width: 60em; /* 960px */
    position: relative; 
    line-height: 1.5; 
    }
    
    /* Hide caption */
    p.caption { display:none }
    "

    if ( !file.exists(default.stylesheet) )
        writeLines(default.stylesheet, "stylesheet.css")  
}	