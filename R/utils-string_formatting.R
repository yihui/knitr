str_complete_replace = function(string, pattern, replacement, perl = TRUE, fixed = FALSE) {
  gsub(pattern, replacement, string, perl = perl, fixed = fixed)
}
