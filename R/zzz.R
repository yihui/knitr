.onLoad = function(lib, pkg) {
  register_vignette_engines(pkg)
  
  default_handlers <<- evaluate::new_output_handler()
}
