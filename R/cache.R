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

  cache_save = function(keys, outname, hash, lazy = TRUE) {
    meta_name = cache_meta_name(hash)
    if (exists(meta_name, envir = knit_global())) outname = c(outname, meta_name)
    out0 = outname
    on.exit(rm(list = out0, envir = knit_global()), add = TRUE)
    # keys are new variables created; outname is the text output of a chunk
    path = cache_path(hash)
    # add random seed to cache if exists
    if (exists('.Random.seed', envir = globalenv(), inherits = FALSE)) {
      copy_env(globalenv(), knit_global(), '.Random.seed')
      outname = c('.Random.seed', outname)
    }
    if (!lazy) outname = c(keys, outname)
    save(list = outname, file = paste(path, 'RData', sep = '.'), envir = knit_global())
    if (!lazy) return()  # everything has been saved; no need to make lazy db
    # random seed is always load()ed
    keys = setdiff(keys, '.Random.seed')
    getFromNamespace('makeLazyLoadDB', 'tools')(knit_global(), path, variables = keys)
  }

  save_objects = function(objs, label, path) {
    if (length(objs) == 0L) objs = ''
    # save object names
    x = paste(c(label, objs), collapse = '\t')
    if (file.exists(path)) {
      lines = read_utf8(path)
      lines = lines[lines != label] # knitr < 1.5 may have lines == label
      idx = substr(lines, 1L, nchar(label) + 1L) == paste0(label, '\t')
      if (any(idx)) {
        lines[idx] = x  # update old objects
      } else lines = c(lines, x)
    } else lines = x
    write_utf8(lines, path)
  }
  cache_objects = function(keys, globals, label, path) {
    save_objects(keys, label, valid_path(path, '__objects'))
    save_objects(globals, label, valid_path(path, '__globals'))
  }

  cache_load = function(hash, lazy = TRUE) {
    path = cache_path(hash)
    if (!is_abs_path(path)) path = file.path(getwd(), path)
    if (lazy) lazyLoad(path, envir = knit_global())
    # load output from last run if exists
    if (file.exists(path2 <- paste(path, 'RData', sep = '.'))) {
      load(path2, envir = knit_global())
      if (exists('.Random.seed', envir = knit_global(), inherits = FALSE))
        copy_env(knit_global(), globalenv(), '.Random.seed')
      name = cache_meta_name(hash)
      if (exists(name, envir = knit_global())) {
        .knitEnv$meta = c(
          .knitEnv$meta, get(name, envir = knit_global(), inherits = FALSE)
        )
        rm(list = name, envir = knit_global())
      }
    }
  }

  cache_library = function(path, save = TRUE) {
    # save or load R packages
    path = valid_path(path, '__packages')
    if (save) {
      x = rev(.packages())
      if (file.exists(path)) x = setdiff(c(read_utf8(path), x), .base.pkgs)
      write_utf8(x, path)
    } else {
      if (!file.exists(path)) return()
      for (p in read_utf8(path))
        suppressPackageStartupMessages(library(p, character.only = TRUE))
    }
  }

  cache_exists = function(hash, lazy = TRUE) {
    is.character(hash) &&
      all(file.exists(paste(
        cache_path(hash), if (lazy) c('rdb', 'rdx') else 'RData', sep = '.'
      )))
  }

  # when cache=3, code output is stored in .[hash], so cache=TRUE won't lose
  # output as cacheSweave does; for cache=1,2, output is the evaluate() list
  cache_output = function(hash, mode = 'character') {
    name = cache_output_name(hash)
    res = get(name, envir = knit_global(), mode = mode, inherits = FALSE)
    # clean up this hidden variable after we obtain its value
    if (mode == mode(res)) rm(list = name, envir = knit_global())
    res
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

# analyze code and find out all possible variables (not necessarily global variables)
find_symbols = function(code) {
  if (is.null(code) || length(p <- parse(text = code, keep.source = TRUE)) == 0) return()
  p = getParseData(p)
  p = p[p$terminal & p$token %in% c('SYMBOL', 'SYMBOL_FUNCTION_CALL', 'SPECIAL'), ]
  unique(p$text)
}

# a variable name to store the metadata object from code chunks
cache_meta_name = function(hash) sprintf('.%s_meta', hash)
# a variable name to store the text output of code chunks
cache_output_name = function(hash) sprintf('.%s', hash)

cache = new_cache()

# a regex for cache files
cache_rx = '_[abcdef0123456789]{32}[.](rdb|rdx|RData)$'

#' Build automatic dependencies among chunks
#'
#' When the chunk option \code{autodep = TRUE}, all names of objects created in
#' a chunk will be saved in a file named \file{__objects} and all global objects
#' used in a chunk will be saved to \file{__globals}. This function can analyze
#' object names in these files to automatically build cache dependencies, which
#' is similar to the effect of the \code{dependson} option. It is supposed to be
#' used in the first chunk of a document and this chunk must not be cached.
#' @param path Path to the dependency file.
#' @return \code{NULL}. The dependencies are built as a side effect.
#' @note Be cautious about \code{path}: because this function is used in a
#'   chunk, the working directory when the chunk is evaluated is the directory
#'   of the input document in \code{\link{knit}}, and if that directory differs
#'   from the working directory before calling \code{knit()}, you need to adjust
#'   the \code{path} argument here to make sure this function can find the cache
#'   files \file{__objects} and \file{__globals}.
#' @export
#' @seealso \code{\link{dep_prev}}
#' @references \url{https://yihui.org/knitr/demo/cache/}
dep_auto = function(path = opts_chunk$get('cache.path')) {
  # this function should be evaluated in the original working directory
  owd = setwd(opts_knit$get('output.dir')); on.exit(setwd(owd))
  paths = valid_path(path, c('__objects', '__globals'))
  locals = parse_objects(paths[1L]); globals = parse_objects(paths[2L])
  if (is.null(locals) || is.null(globals)) return(invisible(NULL))
  if (!identical(names(locals), names(globals))) {
    warning('corrupt dependency files? \ntry remove ', paste(paths, collapse = '; '))
    return(invisible(NULL))
  }
  nms = intersect(names(knit_code$get()), names(locals)) # guarantee correct order
  # locals may contain old chunk names; the intersection can be of length < 2
  if (length(nms) < 2) return(invisible(NULL))
  for (i in 2:length(nms)) {
    if (length(g <- globals[[nms[i]]]) == 0) next
    for (j in 1:(i - 1L)) {
      # check if current globals are in old locals
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
  lines = strsplit(read_utf8(path), '\t')
  if (length(lines) < 2L) return()  # impossible for dependson
  objs = lapply(lines, `[`, -1L)
  names(objs) = lapply(lines, `[`, 1L)
  objs
}

#' Load the cache database of a code chunk
#'
#' If a code chunk has turned on the chunk option \code{cache = TRUE}, a cache
#' database will be established after the document is compiled. You can use this
#' function to manually load the database anywhere in the document (even before
#' the code chunk). This makes it possible to use objects created later in the
#' document earlier, e.g. in an inline R expression before the cached code
#' chunk, which is normally not possible because \pkg{knitr} compiles the
#' document in a linear fashion, and objects created later cannot be used before
#' they are created.
#' @param label The chunk label of the code chunk that has a cache database.
#' @param object The name of the object to be fetched from the database. If it is
#'   missing, \code{NULL} is returned).
#' @param notfound A value to use when the \code{object} cannot be found.
#' @param path Path of the cache database (normally set in the global chunk
#'   option \code{cache.path}).
#' @param lazy Whether to \code{\link{lazyLoad}} the cache database (depending
#'   on the chunk option \code{cache.lazy = TRUE} or \code{FALSE} of that code
#'   chunk).
#' @note Apparently this function loads the value of the object from the
#'   \emph{previous} run of the document, which may be problematic when the
#'   value of the object becomes different the next time the document is
#'   compiled. Normally you must compile the document twice to make sure the
#'   cache database is created, and the object can be read from it. Please use
#'   this function with caution.
#' @references See the example #114 at
#'   \url{https://github.com/yihui/knitr-examples}.
#' @return Invisible \code{NULL} when \code{object} is not specified (the cache
#'   database will be loaded as a side effect), otherwise the value of the
#'   object if found.
#' @export
load_cache = function(
  label, object, notfound = 'NOT AVAILABLE', path = opts_chunk$get('cache.path'),
  lazy = TRUE
) {
  owd = setwd(opts_knit$get('output.dir')); on.exit(setwd(owd))
  path = valid_path(path, label)
  p0 = dirname(path); p1 = basename(path)
  p2 = list.files(p0, cache_rx)
  if (length(p2) == 0) return(notfound)
  p2 = p2[substr(p2, 1, nchar(p1)) == p1]
  if (length(p2) == 0) return(notfound)
  if (length(p2) > 3) stop(
    'Wrong cache databases for the chunk ', label,
    '. You need to remove redundant cache files. Found ', paste(p2, collapse = ', ')
  )
  p2 = unique(gsub('[.](rdb|rdx|RData)$', '', p2))
  if (length(p2) != 1) stop('Cannot identify the cache database for chunk ', label)
  cache$load(file.path(p0, p2), lazy)
  if (missing(object)) return(invisible(NULL))
  if (exists(object, envir = knit_global(), inherits = FALSE)) {
    get(object, envir = knit_global(), inherits = FALSE)
  } else notfound
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
#' @references \url{https://yihui.org/knitr/demo/cache/}
dep_prev = function() {
  labs = names(knit_code$get())
  if ((n <- length(labs)) < 2L) return() # one chunk or less; no sense of deps
  opts_knit$set(warn.uncached.dep = FALSE)
  for (i in 1L:(n - 1L)) {
    dep_list$set(setNames(list(labs[(i + 1L):n]), labs[i]))
  }
}

#' An unevaluated expression to return .Random.seed if exists
#'
#' This expression returns \code{.Random.seed} when \code{eval(rand_seed)} and
#' \code{NULL} otherwise.
#'
#' It is designed to work with \code{opts_chunk$set(cache.extra = rand_seed)}
#' for reproducibility of chunks that involve with random number generation. See
#' references.
#' @export
#' @format NULL
#' @references \url{https://yihui.org/knitr/demo/cache/}
#' @examples eval(rand_seed)
#' rnorm(1) # .Random.seed is created (or modified)
#' eval(rand_seed)
rand_seed = quote({
  .GlobalEnv$.Random.seed
})

#' Clean cache files that are probably no longer needed
#'
#' If you remove or rename some cached code chunks, their original cache files
#' will not be automatically cleaned. You can use this function to identify
#' these possible files, and clean them if you are sure they are no longer
#' needed.
#' @param clean Boolean; whether to remove the files.
#' @param path Path to the cache.
#' @note  The identification is not guaranteed to be correct, especially when
#'   multiple documents share the same cache directory. You are recommended to
#'   call \code{clean_cache(FALSE)} and carefully check the list of files (if
#'   any) before you really delete them (\code{clean_cache(TRUE)}).
#'
#'   This function must be called within a code chunk in a source document,
#'   since it needs to know all chunk labels of the current document to
#'   determine which labels are no longer present, and delete cache
#'   corresponding to these labels.
#' @export
clean_cache = function(clean = FALSE, path = opts_chunk$get('cache.path')) {
  odir = opts_knit$get('output.dir')
  if (is.null(odir)) {
    warning('This function must be called inside a source document')
    return()
  }
  owd = setwd(odir); on.exit(setwd(owd))
  if (file_test('-d', path)) {
    p0 = path; p1 = ''
  } else {
    p0 = dirname(path); p1 = basename(path)
  }
  files = list.files(p0, cache_rx, full.names = TRUE)
  if (length(files) == 0) return()
  base = basename(files)
  labs = .knitEnv$labels
  if (length(labs) == 0) return()
  i = !(sub(cache_rx, '', base) %in% paste0(p1, labs))
  if (p1 != '') i = i & (substr(base, 1, nchar(p1)) == p1)
  if (!any(i)) return()
  if (clean) unlink(files[i]) else message(
    'Clean these cache files?\n\n', one_string(files[i]), '\n'
  )
}
