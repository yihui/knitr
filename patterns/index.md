---
layout: default
title: Patterns
subtitle: A list of regular expressions to extract R code and chunk options from the input document
---

The [object](objects) `knit_patterns` manages patterns in **knitr**. For example, we can use `knit_patterns$get()` to check the current pattern list. A pattern list includes the following components:

- `chunk.begin`: the pattern for the beginning of a code chunk; it must contain a group defined by `()`, which will be used to extract chunk options
- `chunk.end`: the pattern for the end of a chunk (the original meaning of this pattern in literate programming is different: it used to indicate the beginning of normal text; if you want the original meaning, see the [package option](/knitr/options) `filter.chunk.end`)
- `chunk.code`: the pattern to be used to extract R code from a chunk by removing characters of this pattern
- `inline.code`: the pattern to be used to extract the pieces of R code mixed inline with other texts (i.e. those which are not in separate code chunks); like `chunk.begin`, it must contain a group 
- `inline.comment`: the pattern of inline comments (tokens of inline R code will be removed from lines that match with this pattern)
- `header.begin`: the pattern to find out where the document header begins; this is used to insert some header information into the output document (e.g. commands in the preamble in LaTeX, or CSS styles in HTML)
- `document.begin`: the pattern to find out where the body of the document begins (it can be used, for example, to extract the LaTeX preamble so we can externalize tikz graphics, or insert code for syntax highlighting)

Patterns that are `NULL` will not be matched.

Like Sweave, there are two types of R code in **knitr**: code chunks (like paragraphs) and inline R code which will be executed within text lines. For chunks in a document, options are of the form `label, opt1=TRUE, opt2=FALSE, opt3='character.string'` (connected by `,` and `=`; only the chunk label does not need to have a `value`, since it is implicitly the `value` of the chunk option `label`).

## Built-in Patterns

There are several built-in pattern lists in **knitr** which are stored in `all_patterns`.

{% highlight r %}
library(knitr)
str(all_patterns)
{% endhighlight %}



{% highlight text %}
## List of 8
##  $ rnw     :List of 7
##   ..$ chunk.begin   : chr "^\\s*<<(.*)>>=.*$"
##   ..$ chunk.end     : chr "^\\s*@\\s*(%+.*|)$"
##   ..$ inline.code   : chr "\\\\Sexpr\\{([^}]+)\\}"
##   ..$ inline.comment: chr "^\\s*%.*"
##   ..$ ref.chunk     : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ header.begin  : chr "(^|\n)[^%]*\\s*\\\\documentclass[^}]+\\}"
##   ..$ document.begin: chr "\\s*\\\\begin\\{document\\}"
##  $ brew    :List of 1
##   ..$ inline.code: chr "<%[=]{0,1}\\s+([^%]+)\\s+[-]*%>"
##  $ tex     :List of 8
##   ..$ chunk.begin   : chr "^\\s*%+\\s*begin.rcode\\s*(.*)"
##   ..$ chunk.end     : chr "^\\s*%+\\s*end.rcode"
##   ..$ chunk.code    : chr "^%+"
##   ..$ ref.chunk     : chr "^%+\\s*<<(.+)>>\\s*$"
##   ..$ inline.comment: chr "^\\s*%.*"
##   ..$ inline.code   : chr "\\\\rinline\\{([^}]+)\\}"
##   ..$ header.begin  : chr "(^|\n)[^%]*\\s*\\\\documentclass[^}]+\\}"
##   ..$ document.begin: chr "\\s*\\\\begin\\{document\\}"
##  $ html    :List of 5
##   ..$ chunk.begin : chr "^\\s*<!--\\s*begin.rcode\\s*(.*)"
##   ..$ chunk.end   : chr "^\\s*end.rcode\\s*-->"
##   ..$ ref.chunk   : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ inline.code : chr "<!--\\s*rinline(.+?)-->"
##   ..$ header.begin: chr "\\s*<head>"
##  $ md      :List of 4
##   ..$ chunk.begin: chr "^\\s*```+\\s*\\{r(.*)\\}\\s*$"
##   ..$ chunk.end  : chr "^\\s*```+\\s*$"
##   ..$ ref.chunk  : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ inline.code: chr "`r +([^`]+)\\s*`"
##  $ rst     :List of 5
##   ..$ chunk.begin: chr "^\\s*[.][.]\\s+\\{r(.*)\\}\\s*$"
##   ..$ chunk.end  : chr "^\\s*[.][.]\\s+[.][.]\\s*$"
##   ..$ chunk.code : chr "^[.][.]"
##   ..$ ref.chunk  : chr "^\\.*\\s*<<(.+)>>\\s*$"
##   ..$ inline.code: chr ":r:`([^`]+)`"
##  $ asciidoc:List of 6
##   ..$ chunk.begin   : chr "^//\\s*begin[.]rcode(.*)$"
##   ..$ chunk.end     : chr "^//\\s*end[.]rcode\\s*$"
##   ..$ chunk.code    : chr "^//+"
##   ..$ ref.chunk     : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ inline.code   : chr "`r +([^`]+)\\s*`|[+]r +([^+]+)\\s*[+]"
##   ..$ inline.comment: chr "^//.*"
##  $ textile :List of 5
##   ..$ chunk.begin   : chr "^###[.]\\s+begin[.]rcode(.*)$"
##   ..$ chunk.end     : chr "^###[.]\\s+end[.]rcode\\s*$"
##   ..$ ref.chunk     : chr "^\\s*<<(.+)>>\\s*$"
##   ..$ inline.code   : chr "@r +([^@]+)\\s*@"
##   ..$ inline.comment: chr "^###[.].*"
{% endhighlight %}

**Knitr** will first examine the content of the input to decide an appropriate set of patterns, if this automatic detection fails, then depending on the extension of the input filename, **knitr** will automatically choose a pattern list from the above lists, e.g. `file.Rnw` will use `all_patterns$rnw`, and `file.html` will use `all_patterns$html`, etc.

A series of convenience functions `pat_rnw()`, `pat_html()`, `pat_md()`, `pat_tex()` and `pat_brew()` can be used to set built-in patterns.
