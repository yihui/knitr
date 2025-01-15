has_rlang = FALSE

.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)
  
  default_handlers <<- evaluate::new_output_handler()

  has_rlang <<- requireNamespace("rlang", quietly = TRUE)

  if (has_rlang)
    rlang_entrace_handler <<- list(error = function(e) rlang::entrace(e))
}
