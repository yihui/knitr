hilight_source = function(x, format, options) {
  if ((format %in% c('latex', 'html')) && options$highlight) {
    if (options$engine == 'R') {
      opts = opts_knit$get('highr.opts')
      highr::hilight(x, format, prompt = options$prompt, markup = opts$markup)
    } else {
      res = try(highr::hi_andre(x, options$engine, format))
      if (inherits(res, 'try-error')) {
        if (format == 'html') highr:::escape_html(x) else highr:::escape_latex(x)
      } else {
        highlight_header()
        n = length(res)
        # do not touch font size
        if (res[n] == '\\normalsize') res = res[-n]
        res
      }
    }
  } else if (options$engine == 'R' && options$prompt) {
    # if you did not reformat or evaluate the code, I have to figure out which
    # lines belong to one complete expression first (#779)
    if (!options$tidy && isFALSE(options$eval))
      x = vapply(highr:::group_src(x), paste, character(1), collapse = '\n')
    line_prompt(x)
  } else x
}

highlight_header = function() {
  set_header(highlight.extra = paste(c(
    sprintf('\\let\\hl%s\\hlstd', c('esc', 'pps', 'lin')),
    sprintf('\\let\\hl%s\\hlcom', c('slc', 'ppc'))
  ), collapse = ' '))
}

# stolen from Romain's highlight package (v0.3.2)

# http://www.w3schools.com/css/css_colornames.asp
w3c.colors = c(
  aqua = '#00FFFF', black = '#000000', blue = '#0000FF', fuchsia = '#FF00FF',
  gray = '#808080', green = '#008000', lime = '#00FF00', maroon = '#800000',
  navy = '#000080', olive = '#808000', purple = '#800080', red = '#FF0000',
  silver = '#C0C0C0', teal = '#008080', white = '#FFFFFF', yellow = '#FFFF00'
)

css.parse.color = function(txt, default = '#000000') {
  txt = gsub('\\s+', '', tolower(txt))
  if (is.hex(txt)) return(txt)

  # css specs are from 0 to 255
  rgb = function(...) grDevices::rgb(..., maxColorValue = 255)

  # first we try to match against w3c standard colors
  if (!grepl('[^a-z]', txt) && txt %in% names(w3c.colors))
    return(w3c.colors[txt])

  # now we try R colors
  if (!grepl('[^a-z0-9]', txt)) {
    R.colors = colors()
    res = R.colors %in% txt
    if (any(res)) {
      return(rgb(t(col2rgb(R.colors[res]))))
    }
  }

  # next we try an rgb() specification
  if (grepl('rgb', txt)) {
    p = try_silent(parse(text = txt))
    if (!inherits(p, 'try-error')) {
      res = try_silent(eval(p))
      if (!inherits(res, 'try-error')) return(res)
    }
  }

  # fall back on the default color
  default
}

is.hex = function(x) grepl('^#[0-9a-f]{6}$', x)

# minimal css parser
css.parser = function(file, lines = readLines(file)) {

  rx = '^\\.(.*?) *\\{.*$'
  dec.lines = grep(rx, lines)
  dec.names = sub(rx, '\\1', lines[dec.lines])
  if (any(grepl('[0-9]', dec.names))) warning('use of numbers in style names')

  end.lines = grep('^\\s*\\}', lines)

  # find the closing brace of each declaration
  dec.close = end.lines[vapply(dec.lines, function(x) which.min(end.lines < x), integer(1))]

  pos = matrix(c(dec.lines, dec.close), ncol = 2)
  styles = apply(pos, 1, function(x) {
    data = lines[(x[1] + 1):(x[2] - 1)]
    settings.rx = '^\\s*(.*?)\\s*:\\s*(.*?)\\s*;\\s*$'
    settings = sub(settings.rx, '\\1', data, perl = TRUE)
    contents = sub(settings.rx, '\\2', data, perl = TRUE)
    out = list()
    for (i in seq_along(settings)) {
      setting = settings[i]
      content = contents[i]
      out[[setting]] = switch(
        setting,
        color = css.parse.color(content, '#000000'),
        background = css.parse.color(content, '#FFFFFF'),
        content
      )
    }
    out
  })
  names(styles) = dec.names
  styles
}

# styler assistant for latex
styler_assistant_latex = function(x) {

  styles = sapply(x, function(item) {
    settings = names(item)
    has = function(s, value) {
      s %in% settings && grepl(value, item[[s]])
    }
    start = end = ''
    if ('color' %in% settings) {
      start = paste0(start, '\\textcolor[rgb]{', col2latexrgb(item[['color']]), '}{')
      end = paste0(end, '}')
    }
    if (has('font-weight', 'bold')) {
      start = paste0(start, '\\textbf{')
      end = paste0('}', end)
    }
    if (has('font-style', 'italic')) {
      start = paste0(start, '\\textit{')
      end = paste0('}', end)
    }
    sprintf('%s#1%s', start, end)
  })
  sprintf('\\newcommand{\\hl%s}[1]{%s}%%', names(x), styles)
}

col2latexrgb = function(hex) {
  # as.character(0.123) -> 0,123 when "OutDec = ,", so make sure . is used
  outdec = options(OutDec = '.'); on.exit(options(outdec))
  col = col2rgb(hex)[, 1] / 255
  paste(round(col, 3), collapse = ',')
}
