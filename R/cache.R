## ideas borrowed from cacheSweave but not as sophisticated

## ideas adapted from Hadley's decumar: https://github.com/hadley/decumar
## but it is using .rdb and .rdx as 'hard cache' (instead of cache in memory)
new_cache = function() {

    cache_path = function(hash) {
        d = dirname(hash)
        if (!is_abs_path(d)) {
            id = input_dir()
            d = file.path(id, d)  # turn relative path to abs path
        }
        if (!file.exists(d)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
        file.path(d, basename(hash))
    }

    cache_purge = function(hash) {
        for (h in hash) unlink(str_c(cache_path(h), c('.rdb', '.rdx')))
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

    list(purge = cache_purge, save = cache_save, load = cache_load,
         exists = cache_exists, output = cache_output)
}

cache = new_cache()
