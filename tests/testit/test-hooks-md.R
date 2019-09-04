library(testit)

render_jekyll('prettify')
options(prompt = '> ', continue = '+ ')

# see http://stackoverflow.com/q/18992260/559676 for the bug
assert(
  'the source hook pastes source code into one character string',
  identical(
    knit_hooks$get('source')(c('1+1', '2-2'), opts_chunk$get()),
    "\n\n<pre><code class=\"prettyprint \">1+1\n2-2</code></pre>\n\n"
  ),
  identical(
    knit_hooks$get('source')(c('1+1+\n1', '2-2'), opts_chunk$merge(list(prompt = TRUE))),
    "\n\n<pre><code class=\"prettyprint \">&gt; 1+1+\n+ 1\n&gt; 2-2</code></pre>\n\n"
  )
)

render_markdown()

img_output = function(path, opts = list()) {
  opts = opts_chunk$merge(opts)
  wrap(knit_print(include_graphics(path)), opts)
}

assert(
  'include_graphics() includes custom images correctly',
  identical(img_output('a.png'), '![](a.png)'),
  identical(img_output(c('a.png', 'b.png'), list(fig.show = 'hold')), '![](a.png)![](b.png)'),
  identical(img_output('a.png', list(fig.cap = 'foo bar')), '![foo bar](a.png)'),
  identical(img_output('a.png', list(out.width = '50%')), '<img src="a.png" width="50%" />'),
  identical(img_output('a.pdf', list(out.width = '300px')), '<embed src="a.pdf" width="300px" type="application/pdf" />')
)

hook_src = knit_hooks$get("source")
options_ = list(engine = "r", prompt = FALSE, highlight = TRUE)

assert(
  'Attributes for souce can be specified class.source and attr.source',
  identical(
    hook_src("1", c(options_, class.source = "a b")),
    "\n\n```{.r .a .b}\n1\n```\n\n"
  ),
  identical(
    hook_src("1", c(options_, attr.source = ".a .b")),
    "\n\n```{.r .a .b}\n1\n```\n\n"
  ),
  identical(
    hook_src("1", c(options_, class.source = "a", attr.source = "b='1'")),
    "\n\n```{.r .a b='1'}\n1\n```\n\n"
  ),
  identical(
    hook_src("1", c(options_, attr.source = ".a b='1'")),
    "\n\n```{.r .a b='1'}\n1\n```\n\n"
  )
)

hook_out = knit_hooks$get("output")

assert(
  'Attributes for souce can be specified class.source and attr.source',
  identical(
    hook_out("1\n", c(options_, class.output = "a b")),
    "\n\n```{.a .b}\n1\n```\n\n"
  ),
  identical(
    hook_out("1\n", c(options_, attr.output = ".a .b")),
    "\n\n```{.a .b}\n1\n```\n\n"
  ),
  identical(
    hook_out("1\n", c(options_, class.output = "a", attr.output = "b='1'")),
    "\n\n```{.a b='1'}\n1\n```\n\n"
  ),
  identical(
    hook_out("1\n", c(options_, attr.output = ".a b='1'")),
    "\n\n```{.a b='1'}\n1\n```\n\n"
  )
)


knit_hooks$restore()
