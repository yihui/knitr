library(testit)

assert("warning-only output does not produce leading blank lines in kframe", {
  res = knit(
    text = c('<<echo=FALSE, warning=TRUE>>=', 'warning("a warning")', '@'),
    quiet = TRUE
  )
  # kframe should not start with a blank line (i.e., no \n\n right after \begin{kframe}\n)
  (!grepl('\\begin{kframe}\n\n', res, fixed = TRUE))
})

assert("alt text is included in LaTeX output", {
  # no alt text
  (hook_plot_tex('foo.pdf', list(fig.align = 'center', fig.show = 'asis')) %==%
     '\n\n{\\centering \\includegraphics{foo} \n\n}\n\n')

  # alt text
  (hook_plot_tex('foo.pdf', list(fig.alt = 'Alt', fig.align = 'center',
                                 fig.show = 'asis')) %==%
     '\n\n{\\centering \\includegraphics[alt={Alt}]{foo} \n\n}\n\n')

  # with width
  (hook_plot_tex('foo.pdf', list(fig.alt = 'Alt', fig.align = 'center',
                                 fig.show = 'asis', out.width = '\\maxwidth')) %==%
     '\n\n{\\centering \\includegraphics[width=\\maxwidth,alt={Alt}]{foo} \n\n}\n\n')
})

assert("a user-provided animation hook generates the LaTeX code for animations", {
  hook = function(x, options) sprintf('<%s|%d>', x, options$fig.num)
  opts = function(...) opts_chunk$merge(list(fig.show = 'animate', ...))

  # the hook is called only once, for the last plot of the chunk
  (hook_plot_tex('foo-1.pdf', opts(animation.hook = hook, fig.cur = 1, fig.num = 3)) %==% '')

  # the hook output replaces \animategraphics{} but keeps the surrounding markup
  (hook_plot_tex(
    'foo-3.pdf', opts(animation.hook = hook, fig.cur = 3, fig.num = 3, fig.align = 'center')
  ) %==% '\n\n{\\centering <foo-3.pdf|3>\n\n}\n\n')

  # the package option animation.fun is respected as well
  opts_knit$set(animation.fun = hook)
  (hook_plot_tex('foo-3.pdf', opts(fig.cur = 3, fig.num = 3)) %==% '\n<foo-3.pdf|3>')
  opts_knit$set(animation.fun = NULL)

  # the built-in hooks generate HTML instead of LaTeX, so they must not be used
  # here, no matter how they are provided
  (grepl('animategraphics', hook_plot_tex(
    'foo-3.pdf', opts(animation.hook = 'ffmpeg', fig.cur = 3, fig.num = 3)
  ), fixed = TRUE))
  for (h in list(hook_ffmpeg_html, hook_gifski, hook_scianimator, hook_r2swf)) {
    (grepl('animategraphics', hook_plot_tex(
      'foo-3.pdf', opts(animation.hook = h, fig.cur = 3, fig.num = 3)
    ), fixed = TRUE))
    opts_knit$set(animation.fun = h)
    (grepl('animategraphics', hook_plot_tex(
      'foo-3.pdf', opts(fig.cur = 3, fig.num = 3)
    ), fixed = TRUE))
    opts_knit$set(animation.fun = NULL)
  }

  # without a hook, \animategraphics{} is generated as before
  (grepl('animategraphics', hook_plot_tex(
    'foo-3.pdf', opts(fig.cur = 3, fig.num = 3)
  ), fixed = TRUE))
})

assert("fig.note produces \\figurenote{} inside the figure environment", {
  note_opts = function(note) opts_chunk$merge(list(
    label = 'l', fig.cap = 'Cap', fig.note = note, fig.show = 'asis'
  ))

  # pretend we are at the start of a fresh document
  knitr_env = getFromNamespace('.knitEnv', 'knitr')
  knitr_env$fig.note.defined = FALSE

  res = hook_plot_tex('foo.pdf', note_opts('A note.'))
  # \figurenote{} appears after \caption and before \end{figure}
  (grepl('\\figurenote{A note.}', res, fixed = TRUE))
  (grepl('\\providecommand{\\figurenote}', res, fixed = TRUE))
  # \figurenote{} comes after \caption and before \end{figure}
  (regexpr('\\caption', res, fixed = TRUE) <
     regexpr('\\figurenote{A note.}', res, fixed = TRUE))
  (regexpr('\\figurenote{A note.}', res, fixed = TRUE) <
     regexpr('\\end{figure}', res, fixed = TRUE))

  # the definition is emitted only once per document: a second figure note
  # calls \figurenote{} but does not repeat \providecommand
  res2 = hook_plot_tex('foo.pdf', note_opts('Another note.'))
  (grepl('\\figurenote{Another note.}', res2, fixed = TRUE))
  (!grepl('\\providecommand', res2, fixed = TRUE))

  # an empty/NA note adds nothing
  (!grepl('figurenote', hook_plot_tex('foo.pdf', note_opts(NA)), fixed = TRUE))
})
