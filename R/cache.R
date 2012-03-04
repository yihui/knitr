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

#' Build automatic dependencies among chunks
#' 
#' When the chunk option \code{autodep = TRUE}, all names of objects created in 
#' a chunk will be saved in a file named \file{__objects} and all global objects
#' used in a chunk will be saved to \file{__globals}. This function can analyze 
#' object names in these files to automatically build cache dependencies, which 
#' is similar to the effect of the \code{dependson} option. It is supposed to be
#' used in the first chunk of a document and this chunk must not be cached.
#' @param path the path to the dependency file
#' @return \code{NULL}. The dependencies are built as a side effect.
#' @note Be cautious about \code{path}: because this function is used in a 
#'   chunk, the working directory when the chunk is evaluated is the directory 
#'   of the input document in \code{\link{knit}}, and if that directory differs 
#'   from the working directory before calling \code{knit()}, you need to adjust
#'   the \code{path} argument here to make sure this function can find the cache
#'   files \file{__objects} and \file{__globals}.
#' @export
#' @references \url{http://yihui.name/knitr/demo/cache/}
build_dep = function(path = opts_chunk$get('cache.path')) {
  paths = valid_path(path, c('__objects', '__globals'))
  locals = parse_objects(paths[1L]); globals = parse_objects(paths[2L])
  if (is.null(locals) || is.null(globals)) return(invisible(NULL))
  if (!identical(names(locals), names(globals))) {
    warning('corrupt dependency files? \ntry remove ', 
            str_c(paths, collapse = '; '))
    return(invisible(NULL))
  }
  nms = intersect(names(knit_code$get()), names(locals)) # guarantee correct order
  for (i in 2:length(nms)) {
    for (j in 1:(i - 1L)) {
      ## check if current globals are in old locals
      if (length(globals[[i]]) && any(globals[[i]] %in% locals[[j]]))
        dep_list$set(structure(list(c(dep_list$get(nms[j]), nms[i])), .Names = nms[j]))
    }
  }
}
# parse objects in dependency files
parse_objects = function(path) {
  if (!file.exists(path)) {
    warning('file ', path, ' not found'); return()
  }
  lines = str_split(readLines(path), fixed('\t'))
  if (length(lines) < 2L) return()  # impossible for dependson
  objs = lapply(lines, `[`, -1L)
  names(objs) = lapply(lines, `[`, 1L)
  objs
}
