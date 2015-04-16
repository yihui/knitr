
# ideas:
#
# codecs that have a name and conversion function(s)
# codecs leave an attribute with the original type name
# we look for this and extract it into the 'type' field
#
# functions:
#
# knit_params(lines)
# knit_params_from_file(file)
# knit_params_as_list
# knit_params_as_code
# knit_params_as_yaml

knit_params <- function(lines) {

  # read the yaml front matter and see if there is a params element in it
  yaml <- yaml_front_matter(lines)
  if (!is.null(yaml)) {
    parsed_yaml <- yaml::yaml.load(yaml, handlers = list(
      date = as.Date
    ))
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

# resolve the raw params list into the full params data structure
# (with name, type, value, and other optional fields included)
resolve_params <- function(params) {

  full_params <- list()

  names <- names(params)
  for (name in names) {

    # get the parameter
    param <- params[[name]]

    # if it's not a list then a plain value was specified, create
    # the list based on the value
    if (!is.list(param)) {
      param <- list(
        name = name,
        type = class(param)[[1]],
        value = param
      )
    } else {

      # validate that the "value" field is included
      # (ignore with a warning if it isn't)
      if (!"value" %in% names(param)) {
        warning("no value field specified for yaml parameter '", name, "'")
        next
      }

      # ensure we have a name and type
      param$name <- name
      param$type <- class(param$value)[[1]]
    }

    # normalize parameter values
    param <- list(
      name = param$name,
      type = param$type,
      value = param$value
    )

    # add the parameter
    full_params[[length(full_params) + 1]] <- param
  }

  # return params
  full_params
}


lines <- c(
  "---",
  "params:",
  "  tip: !date 2015-2-15",
  "  sap:",
  "    value: !date 2015-2-15",
  "  bad:",
  "    valuet: sip",
  "---",
  ""
)

p <- knit_params(lines)

str(p)
cat(yaml::as.yaml(p))

