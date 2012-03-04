## ideas borrowed from cacheSweave but not as sophisticated

## ideas adapted from Hadley's decumar: https://github.com/hadley/decumar
## but it is using .rdb and .rdx as 'hard cache' (instead of cache in memory)
new_cache = function() {
  
  cache_path = function(hash) {
    d = dirname(hash)
    if (!file.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
    file.path(d, basename(hash))
  }
  
  cache_purge = function(hash) {
    for (h in hash) unlink(str_c(cache_path(h), c('.rdb', '.rdx')))
  }
  
  cache_save = function(keys, hash) {
    tools:::makeLazyLoadDB(globalenv(), cache_path(hash), variables = keys)
  }

  save_objects = function(objs, label, path) {
    ## save object names
    x = str_c(c(label, objs), collapse = '\t')
    if (file.exists(path)) {
      lines = readLines(path)
      idx = substr(lines, 1L, nchar(label)) == label
      if (any(idx)) {
        lines[idx] = x  # update old objects
      } else lines = c(lines, x)
    } else lines = x
    writeLines(lines, con = path)
  }
  cache_objects = function(keys, code, label, path) {
    save_objects(keys, label, valid_path(path, '__objects'))
    ## find globals in code; may not be reliable
    save_objects(find_globals(code), label, valid_path(path, '__globals'))
  }

  cache_load = function(hash) {
    lazyLoad(cache_path(hash), envir = globalenv())
  }

  cache_library = function(path, save = TRUE) {
    ## save or load R packages
    path = valid_path(path, '__packages')
    if (save) {
      x = .packages()
      if (file.exists(path)) x = unique(c(x, readLines(path)))
      cat(x, file = path, sep = '\n')
    } else {
      if (!file.exists(path)) return()
      for (p in readLines(path)) library(p, character.only = TRUE)
    }
  }

  cache_exists = function(hash) {
    all(file.exists(str_c(cache_path(hash), c('.rdb', '.rdx'))))
  }
  
  ## code output is stored in .[hash], so cache=TRUE won't lose output as cacheSweave does
  cache_output = function(hash) {
    if (exists(str_c('.', hash), envir = globalenv(), mode = 'character')) {
      get(str_c('.', hash), envir = globalenv(), mode = 'character')
    } else ''
  }
  
  list(purge = cache_purge, save = cache_save, load = cache_load, objects = cache_objects,
       exists = cache_exists, output = cache_output, library = cache_library)
}
# analyze code and find out global variables
find_globals = function(code) {
  fun = eval(parse(text = str_c('function(){\n', str_c(code, collapse='\n'), '\n}')))
  setdiff(codetools::findGlobals(fun), c('{', '<-', '='))
}

cache = new_cache()
