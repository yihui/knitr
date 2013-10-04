## ideas borrowed from cacheSweave but not as sophisticated

## closure adapted from Hadley's decumar: https://github.com/hadley/decumar
## but it is using .rdb and .rdx as 'hard cache' (instead of cache in memory)
new_cache = function() {

  cache_path = function(hash) {
    d = dirname(hash)
    if (!file.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
    file.path(d, basename(hash))
  }

  cache_purge = function(hash) {
    for (h in hash) unlink(paste(cache_path(h), c('rdb', 'rdx', 'RData'), sep = '.'))
  }

  cache_save = function(keys, outname, hash) {
    # keys are new variables created; outname is the text output of a chunk
    path = cache_path(hash)
    # add random seed to cache if exists
    if (exists('.Random.seed', envir = globalenv())) {
      copy_env(globalenv(), knit_global(), '.Random.seed')
      outname = c('.Random.seed', outname)
    }
    save(list = outname, file = paste(path, 'RData', sep = '.'), envir = knit_global())
    # random seed is always load()ed
    keys = setdiff(keys, '.Random.seed')
    getFromNamespace('makeLazyLoadDB', 'tools')(knit_global(), path, variables = keys)
  }

  save_objects = function(objs, label, path) {
    if (length(objs) == 0L) objs = ''
    ## save object names
    x = paste(c(label, objs), collapse = '\t')
    if (file.exists(path)) {
      lines = readLines(path)
      lines = lines[lines != label] # knitr < 1.5 may have lines == label
      idx = substr(lines, 1L, nchar(label) + 1L) == paste(label, '\t', sep = '')
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
    path = cache_path(hash)
    if (!is_abs_path(path)) path = file.path(getwd(), path)
    lazyLoad(path, envir = knit_global())
    # load output from last run if exists
    if (file.exists(path2 <- paste(path, 'RData', sep = '.'))) {
      load(path2, envir = knit_global())
      if (exists('.Random.seed', envir = knit_global()))
        copy_env(knit_global(), globalenv(), '.Random.seed')
    }
  }

  cache_library = function(path, save = TRUE) {
    ## save or load R packages
    path = valid_path(path, '__packages')
    if (save) {
      x = .packages()
      if (file.exists(path)) x = setdiff(c(x, readLines(path)), .base.pkgs)
      writeLines(sort(x), path)
    } else {
      if (!file.exists(path)) return()
      for (p in readLines(path))
        suppressPackageStartupMessages(library(p, character.only = TRUE))
    }
  }

  cache_exists = function(hash) {
    is.character(hash) &&
      all(file.exists(paste(cache_path(hash), c('rdb', 'rdx'), sep = '.')))
  }

  # when cache=3, code output is stored in .[hash], so cache=TRUE won't lose
  # output as cacheSweave does; for cache=1,2, output is the evaluate() list
  cache_output = function(hash, mode = 'character') {
    get(sprintf('.%s', hash), envir = knit_global(), mode = mode, inherits = FALSE)
  }

  list(purge = cache_purge, save = cache_save, load = cache_load, objects = cache_objects,
       exists = cache_exists, output = cache_output, library = cache_library)
}
# analyze code and find out global variables
find_globals = function(code) {
  fun = eval(parse_only(c('function(){', code, '}')))
  setdiff(codetools::findGlobals(fun), known_globals)
}
known_globals = c(
  '{', '[', '(', ':', '<-', '=', '+', '-', '*', '/', '%%', '%/%', '%*%', '%o%', '%in%'
)

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
#' @seealso \code{\link{dep_prev}}
#' @references \url{http://yihui.name/knitr/demo/cache/}
dep_auto = function(path = opts_chunk$get('cache.path')) {
  # this function should be evaluated in the original working directory
  owd = setwd(opts_knit$get('output.dir')); on.exit(setwd(owd))
  paths = valid_path(path, c('__objects', '__globals'))
  locals = parse_objects(paths[1L]); globals = parse_objects(paths[2L])
  if (is.null(locals) || is.null(globals)) return(invisible(NULL))
  if (!identical(names(locals), names(globals))) {
    warning('corrupt dependency files? \ntry remove ',
            paste(paths, collapse = '; '))
    return(invisible(NULL))
  }
  nms = intersect(names(knit_code$get()), names(locals)) # guarantee correct order
  # locals may contain old chunk names; the intersection can be of length < 2
  if (length(nms) < 2) return(invisible(NULL))
  for (i in 2:length(nms)) {
    if (length(g <- globals[[nms[i]]]) == 0) next
    for (j in 1:(i - 1L)) {
      ## check if current globals are in old locals
      if (any(g %in% locals[[nms[j]]]))
        dep_list$set(setNames(list(unique(c(dep_list$get(nms[j]), nms[i]))), nms[j]))
    }
  }
}
# parse objects in dependency files
parse_objects = function(path) {
  if (!file.exists(path)) {
    warning('file ', path, ' not found'); return()
  }
  lines = strsplit(readLines(path), '\t')
  if (length(lines) < 2L) return()  # impossible for dependson
  objs = lapply(lines, `[`, -1L)
  names(objs) = lapply(lines, `[`, 1L)
  objs
}

#' Make later chunks depend on previous chunks
#'
#' This function can be used to build dependencies among chunks so that all
#' later chunks depend on previous chunks, i.e. whenever the cache of a previous
#' chunk is updated, the cache of all its later chunks will be updated.
#' @return \code{NULL}; the internal dependency structure is updated as a side
#'   effect.
#' @export
#' @seealso \code{\link{dep_auto}}
#' @references \url{http://yihui.name/knitr/demo/cache/}
dep_prev = function() {
  labs = names(knit_code$get())
  if ((n <- length(labs)) < 2L) return() # one chunk or less; no sense of deps
  for (i in 1L:(n - 1L)) {
    dep_list$set(setNames(list(labs[(i + 1L):n]), labs[i]))
  }
}

#' An unevaluated expression to return .Random.seed if exists
#'
#' This expression returns \code{.Random.seed} when \code{eval(rand_seed)} and
#' \code{NULL} otherwise.
#'
#' It is designed to work with \code{opts_knit$set(cache.extra = rand_seed)} for
#' reproducibility of chunks that involve with random number generation. See
#' references.
#' @export
#' @references \url{http://yihui.name/knitr/demo/cache/}
#' @examples eval(rand_seed)
#' rnorm(1) # .Random.seed is created (or modified)
#' eval(rand_seed)
rand_seed = quote({
  if (exists('.Random.seed', envir = globalenv()))
    get('.Random.seed', envir = globalenv())
})
