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
    knit_hooks$get('source')(c('1+1+\n1', '2-2'), opts_chunk$merge(list(prompt=TRUE))),
    "\n\n<pre><code class=\"prettyprint \">&gt; 1+1+\n+ 1\n&gt; 2-2</code></pre>\n\n"
  )
)

knit_hooks$restore()
