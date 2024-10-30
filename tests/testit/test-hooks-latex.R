library(testit)

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
