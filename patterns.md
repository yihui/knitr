---
layout: default
title: Patterns
subtitle: A list of regular expressions to extract R code and chunk options from the input document
---

The [object](/knit/objects) `kpat` manages patterns in **knit**. A pattern list includes the following components:

- `chunk.begin`: the pattern for the beginning of a code chunk; it must contain a group defined by `()`, which will be used to extract chunk options
- `chunk.end`: the pattern for the end of a chunk
- `chunk.code`: the pattern to be used to extract R code from a chunk by removing characters of this pattern
- `inline.code`: the pattern to be used to extract the pieces of R code mixed inline with other texts (i.e. those which are not in separate code chunks); like `chunk.begin`, it must contain a group 
- `global.options`: the pattern to extract global options for chunks (options are extracted like those in `chunk.begin` but they permanently change global options, whereas chunk options only locally affect a specific chunk)

Patterns that are `NULL` will not be matched.

Like Sweave, there are two types of R code in **knit**: code chunks (like paragraphs) and inline R code which will be executed within text lines. For chunks in a document, options are of the form `label, opt1=TRUE, opt2=FALSE, opt3=character.string` (connected by `,` and `=`; only the chunk label does not need to have a `value`, since it is implicitly the `value` of the chunk option `label`).
