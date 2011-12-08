## copy objects in one environment to the other
copy_env = function(from, to) {
    x = ls(envir = from, all = TRUE)
    for (i in x) {
        assign(i, get(i, envir = from, inherits = FALSE), envir = to)
    }
}


knit_counter = function(init = 0L) {
    n = init
    function(reset = FALSE) {
        if (reset) return(n <<- init)
        n <<- n + 1L
        n - 1L
    }
}

plot_counter = knit_counter(1L)
chunk_counter = knit_counter(1L)

## add a prefix to output
comment_out = function(x, options) {
    prefix = options$comment
    if (!is.null(prefix) && nzchar(prefix) && !is.na(prefix)) {
        prefix = str_c(prefix, ' ')
        evaluate:::line_prompt(x, prompt = prefix, continue = prefix)
    } else x
}

hiren_latex = renderer_latex(document = FALSE)
hiren_html = renderer_html(document = FALSE, header = function() '', footer = function() '')

hilight_source = function(x, theme, options) {
    if (!(theme %in% c('latex', 'html'))) return(x)
    con = textConnection(x)
    on.exit(close(con))
    r = if (theme == 'latex') hiren_latex else hiren_html
    out = capture.output(highlight(con, renderer = r, showPrompts = options$prompt, size = options$size))
    str_c(out, collapse = '\n')
}

is_blank = function(x) {
    str_detect(x, '^\\s*$')
}
valid_prefix = function(x) {
    if (length(x) == 0 || is.na(x) || x == 'NA') return('')
    x
}

framed_color = function(x) {
    x = str_split(x, fixed(';'))[[1]]
    if (length(x) != 3) {
        x = rep(.97, 3)
        warning("background color should be of the from red;green;blue; ",
                "using default background color...")
    }
    sprintf('\\definecolor{shadecolor}{rgb}{%s, %s, %s}', x[1], x[2], x[3])
}

## whether dependent chunks have changed; if so, invalidate cache for this chunk
dependson_changed = function(labels, prefix) {
    if (is.null(labels)) return(FALSE)
    if (!file.exists(d <- dirname(prefix))) return(FALSE)
    base = if (isTRUE(file.info(prefix)[, 'isdir'])) '' else basename(prefix)
    for (f in str_split(labels, fixed(';'))[[1]]) {
        p = list.files(d, str_c(base, f, '_[[:alnum:]]{32}\\.(rdb|rdx)'), full.names = TRUE)
        if (length(p)) {
            if (any(file.exists(unique(str_replace(p, '\\.(rdb|rdx)$', '_changed')))))
                return(TRUE)
        }
    }
    FALSE
}

## extract LaTeX packages for tikzDevice
set_tikz_opts = function(input, cb, ce) {
    hb = knit_patterns$get('header.begin')
    if (length(hb) == 1L) {
        idx = str_detect(input, hb)
        if (any(idx)) {
            options(tikzDocumentDeclaration = input[idx][1])
            db = knit_patterns$get('document.begin')
            if (length(db) == 1L) {
                idx2 = str_detect(input, db)
                if (any(idx2)) {
                    idx = which(idx)[1]; idx2 = which(idx2)[1]
                    if (idx2 - idx > 1) {
                        preamble = pure_preamble(input[seq(idx + 1, idx2 - 1)], cb, ce)
                        .knitEnv$tikzPackages = c(preamble, '\n')
                    }
                }
            }
        }
    }
}
## filter out code chunks from preamble if they exist (they do in LyX/Sweave)
pure_preamble = function(preamble, chunk.begin, chunk.end) {
    blks = which(str_detect(preamble, chunk.begin))
    if (!length(blks)) return(preamble)
    ends = which(str_detect(preamble, chunk.end))
    idx = unlist(mapply(seq, from = blks, to = ends, SIMPLIFY = FALSE))
    preamble[-idx]
}
## whether to write results as-is?
output_asis = function(x, options) {
    is_blank(x) || options$results %in% c('tex', 'asis')
}

## path relative to dir of the input file
input_dir = function() {
    id = opts_knit$get('input.dir')
    if (is.null(id)) return('.')
    id
}

## scientific notation in TeX
format_sci = function(x, theme = 'latex', d = getOption('digits')) {
    if (!is.numeric(x)) return(x)
    if (any(abs(lx <- floor(log(abs(x), 10))) >= d)) {
        b = formatC(x/10^lx)
        b[b %in% c('1', '-1')] = ''
        if (theme == 'latex')
            return(sprintf('$%s%s10^{%s}$', b, ifelse(b == '', '', '\\times '), floor(lx)))
        if (theme == 'html')
            return(sprintf('%s%s10<sup>%s</sup>', b, ifelse(b == '', '', ' &times; '), floor(lx)))
    }
    formatC(x)
}

## absolute path?
abs_path = function(x) {
    if (.Platform$OS.type == 'windows')
        grepl(':', x, fixed = TRUE) || grepl('^\\', x) else grepl('^/', x)
}
