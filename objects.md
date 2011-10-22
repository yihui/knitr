---
layout: default
title: Objects
subtitle: Objects to manipulate options, patterns and hooks
---

The **knit** package uses a special object to control options and settings (denoted as `obj` below); it has the following methods:

- `obj$get(name)`: returns an option named `name` or a list of several options if `name` is a character vector of length greater than 1, and it returns all the options if `name` not provided
- `obj$set(...)`: permanently changes options; the argument `...` can be of the form `tag = value` or a list of options `list(opt1 = value1, opt2 = value2)`
- `obj$merge(values)`: temporarily merges a list of new options into the current list and returns the merged list (original list not changed)
- `obj$restore()`: restores the object

Four such objects are visible to users in **knit**:

- [`optc`](/knit/options#chunk_options): manages `opt`ions for `c`hunks
- [`optk`](/knit/options#knit_options): manages `opt`ions for the **`k`nit** package
- [`hooks`](/knit/hooks): manages hook functions
- [`patterns`](/knit/patterns): manages regular expressions to extract R code from the input document

Except `patterns`, all other three objects are initialized with default values, and `patterns` will be automatically decided according to the type of input document if not provided.