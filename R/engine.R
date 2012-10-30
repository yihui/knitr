#' Engines of other languages
#'
#' This object controls how to execute the code from languages other than R
#' (when the chunk option \code{engine} is not \code{'R'}). Each component in
#' this object is a function that takes a list of current chunk options
#' (including the source code) and returns a character string to be written into
#' the output.
#'
#' The engine function has one argument \code{options}: the source code of the
#' current chunk is in \code{options$code}. Usually we can call external
#' programs to run the code via \code{\link[base]{system}}. Other chunk options
#' are also contained in this argument, e.g. \code{options$echo} and
#' \code{options$eval}, etc.
#' @export
#' @references Usage: \url{http://yihui.name/knitr/objects}
#' @examples knit_engines$get('python'); knit_engines$get('awk')
knit_engines = new_defaults()

# give me source code, text output and I return formatted text using the three
# hooks: source, output and chunk
engine_output = function(code, out, options) {
  if (length(code) != 1L) code = str_c(code, collapse = '\n')
  if (length(out) != 1L) out = str_c(out, collapse = '\n')
  code = str_replace(code, '([^\n]+)$', '\\1\n')
  out = str_replace(out, '([^\n]+)$', '\\1\n')
  txt = paste(c(
    if (options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) knit_hooks$get('output')(out, options)
  ), collapse = '\n')
  if (options$include) knit_hooks$get('chunk')(txt, options) else ''
}
## python perl ruby ghc bash(sh) zsh awk sed
## Python (TODO: how to emulate the console??)
## file is the file to read in cl.opts are additional options to the 
## interpreter
eng_interpreted = function(options) {
  code = str_c(options$code, collapse = '\n')
  code_option = switch(options$engine, bash = '-c', ghc = '-e', perl = '-e', 
                       python = '-c', ruby = '-e', sh = '-c', zsh = '-c', '')
  cmd = paste(options$engine, code_option, shQuote(code), 
              shQuote(options$file), options$cl.opts)
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  engine_output(code, out, options)
}
## C

## Java

## tikz
eng_tikz = function(options) {
    procTikzString <- 
        function   # Converts a tikz-string into pdf by calling `pdflatex`
    (
        tikz        # lines of tikz
       ,tmpl        # file-name of tex-template 
       ,dir         # output-directory
       ,label       # path to output-file
       ,cap = label # figure caption
       ,dev = "pdf" # device to use
       ,repl = "<>" # replacement-string
            
    )
    {
        # Insert tikz into tex-template
        templ_lines <- readLines(tmpl)
        i <- grep(repl, templ_lines)
        s <- c(templ_lines[1:(i-1)], tikz, templ_lines[(i+1):length(templ_lines)])
        # Call `pdflatex` to generate the pdf
        f <- tempfile()
        tex_file <- paste(f, ".tex", sep = "")
        writeLines(s, tex_file)
        cwd = getwd()
        setwd(dirname(tex_file))
        cmd = sprintf("pdflatex %s > /dev/null", tex_file)
        exit_tex = system(cmd)
        outfile = sprintf("%s%s.%s", dir, label, dev)
        if  (exit_tex != 0)
            stop("Problems with pdflatex and input file ", f, "; try to edit ", templ)
        # Convert to the desired output-format, calling `convert`
        if (dev != "pdf")
        {
            exit_conv = system(sprintf("convert %s.pdf %s.%s", f, f, dev))
            if (exit_conv != 0)
                stop("Problems with `convert`; probably not installed")
        }
        setwd(cwd)
        dir.create(dir, showWarnings = FALSE)
        file.copy(paste(f,".", dev, sep = ""), outfile)
        sprintf("![%s](%s)", cap, outfile)
        ### Produces as side effect the output-pdf and returns a markdown string
    }
    out = 
    {
        if (options$eval)
        {
            TIKZ_TMPL = ".tikz2pdf.tex.st"
            if (!file.exists(TIKZ_TMPL))
                file.copy(system.file("misc/tikz2pdf.tex.st", package = "knitr"), TIKZ_TMPL)
            with(options, procTikzString(code, TIKZ_TMPL, fig.path, label, fig.cap, dev))
        }
        else 
            ''
    }
    options$results = 'asis'
    code = str_c(options$code, collapse = '\n')
    engine_output(code, out, options)
}

## dot
eng_dot = function(options){
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  cmd = sprintf('dot -O %s -T%s', shQuote(f), options$dev)
  dir.create(options$fig.path, showWarnings = FALSE)
  out = 
  {
      if (options$eval) 
      {
          system(cmd)
          fig = with(options, paste(fig.path, label, ".", dev, sep = "" ))
          file.copy(paste(f, options$dev, sep = "."), fig)
          sprintf("![%s](%s)", options$fig.cap, fig)
      } else 
          ''
  }
  options$results = 'asis'
  engine_output(code, out, options)
}
## Andre Simon's highlight
eng_highlight = function(options) {
  f = tempfile()
  writeLines(code <- options$code, f)
  on.exit(unlink(f))
  # e.g. highlight.opts can be '-S matlab -O latex'
  cmd = sprintf('highlight -f %s %s', options$highlight.opts %n% '-S text', shQuote(f))
  out = if (options$eval) system(cmd, intern = TRUE) else ''
  options$echo = FALSE; options$results = 'asis'  # do not echo source code
  engine_output(code, out, options)
}

knit_engines$set(
  awk = eng_interpreted, bash = eng_interpreted, gawk = eng_interpreted, 
  ghc = eng_interpreted, highlight = eng_highlight, perl = eng_interpreted, 
  python = eng_interpreted, ruby = eng_interpreted, sed = eng_interpreted, 
  sh = eng_interpreted, zsh = eng_interpreted, tikz = eng_tikz, dot = eng_dot
)

# possible values for engines (for auto-completion in RStudio)
opts_chunk_attr$engine = as.list(sort(c('R', names(knit_engines$get()))))
