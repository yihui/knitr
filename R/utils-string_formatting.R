str_complete_replace = function(string, pattern, replacement, perl = TRUE) {
  gsub(pattern, replacement, string, perl = perl)
}
