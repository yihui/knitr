## ideas borrowed from cacheSweave but not as sophisticated

## ideas adapted from Hadley's decumar: https://github.com/hadley/decumar
## but it is using R image files (.RData) as 'hard cache' (instead of cache in memory)
new_cache = function() {

    cache_path = function(hash) {
        id = input_dir()
        d = file.path(id, dirname(hash))
        if (!file.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
        file.path(id, hash)
    }

    cache_purge = function(hash) {
        unlink(str_c(cache_path(hash), c('.rdb', '.rdx', '_changed')))
    }

    cache_save = function(keys, hash) {
        tools:::makeLazyLoadDB(globalenv(), cache_path(hash), variables = keys)
    }

    cache_load = function(hash) {
        lazyLoad(cache_path(hash), envir = globalenv())
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

    cache_mark = function(hash) {
        file.create(str_c(cache_path(hash), '_changed'))
    }

    cache_unmark = function(hash) {
        unlink(str_c(cache_path(hash), '_changed'))
    }

    list(purge = cache_purge, save = cache_save, load = cache_load,
         exists = cache_exists, output = cache_output,
         mark = cache_mark, unmark = cache_unmark)
}

cache = new_cache()
