str_trim_sides <- function(string) {
  sub("\\s+$", "", sub("^\\s+", "", string))
}
