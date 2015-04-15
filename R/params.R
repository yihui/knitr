


knit_params <- function(lines) {

  # read the yaml front matter and see if there is a params element in it
  yaml <- yaml_front_matter(lines)
  if (!is.null(yaml)) {
    parsed_yaml <- yaml::yaml.load(yaml)
    if (is.list(parsed_yaml) && !is.null(parsed_yaml$params)) {
      # found params, return resolved version of them
      resolve_params(parsed_yaml$params)
    } else {
      NULL
    }
  } else {
    NULL
  }
}



# Extract the yaml front matter (if any) from the passed lines. The front
# matter is returned as a single-element character vector (with newlines
# delimited by \n) suitable for passing to yaml::load. This code is based on
# the partition_yaml_front_matter and parse_yaml_front_matter functions here:
# https://github.com/rstudio/rmarkdown/blob/master/R/output_format.R
yaml_front_matter <- function(lines) {

  # verify that the first two front matter delimiters (---) are not preceded
  # by other content
  has_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1)) {
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }

  # ensure that the front-matter won't cause a crash when passed to yaml::load
  validate_front_matter <- function(front_matter) {

    # trim trailing whitespace
    front_matter <- sub("\\s+$", "", front_matter)

    # look for termination with a :
    !grepl(":$", front_matter)
  }

  # find delimiters in the document
  delimiters <- grep("^---\\s*$", lines)

  # if it's valid then return front matter as a text block suitable
  # for passing to yaml::load
  if (has_front_matter(delimiters)) {

    # return the yaml as a single-element character vector if
    # appears to be valid yaml
    front_matter_lines <- lines[(delimiters[1]):(delimiters[2])]
    if (length(front_matter_lines) > 2) {
      front_matter <- front_matter_lines
      front_matter <- front_matter[2:(length(front_matter)-1)]
      front_matter <- paste(front_matter, collapse="\n")
      if (validate_front_matter(front_matter)) {
        front_matter
      } else {
        NULL
      }
    } else {
      NULL
    }
  }
  else {
    NULL
  }
}

resolve_params <- function(params) {
  params
}


lines <- c(
  "---",
  "foo: bar",
  "snap: tap",
  "---",
  ""
)

knit_params(lines)

