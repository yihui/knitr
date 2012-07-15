## stolen from Romain's highlight package (v0.3.2)

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
    p = try(parse(text = txt), silent = TRUE)
    if (!inherits(p, 'try-error')) {
      res = try(eval(p), silent = TRUE)
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
  dec.close = end.lines[sapply(dec.lines, function(x) which.min(end.lines < x))]

  pos = matrix(c(dec.lines, dec.close), ncol = 2)
  styles = apply(pos, 1, function(x) {
    data = lines[(x[1] + 1):(x[2] - 1)]
    settings.rx = '^\\s*(.*?)\\s*:\\s*(.*?)\\s*;\\s*$'
    settings = sub(settings.rx, '\\1', data, perl = TRUE)
    contents = sub(settings.rx, '\\2', data, perl = TRUE)
    out = list()
    for (i in 1:length(settings)) {
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
