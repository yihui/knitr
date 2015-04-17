
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
#
# extractors for various contexts:
#
# knit_params_as_list   (used by rmarkdown)
# knit_params_as_code   (used by knitr)
# knit_params_as_yaml   (used by rstudio / rsconnect)
#

knit_params <- function(lines) {

  # read the yaml front matter and see if there is a params element in it
  yaml <- yaml_front_matter(lines)
  if (!is.null(yaml)) {
    parsed_yaml <- yaml::yaml.load(yaml, handlers = list(
      date = date_handler,
      file = type_handler("file")
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

  # get the type attribute (if any)
  type_attr <- function(value) {
    attr(value, "type")
  }

  # deduce type from attribute or class
  param_type <- function(value) {
    type <- type_attr(value)
    if (!is.null(type))
      type
    else
      class(value)[[1]]
  }

  # function to return a parameter value without the type attribute
  param_value <- function(value) {
    attr(value, "type") <- NULL
    if (is.null(names(value)))
      unlist(value)
    else
      value
  }

  # params we will return
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
        type = param_type(param),
        value = param_value(param)
      )
    } else {

      # validate that the "value" field is included
      if (!"value" %in% names(param)) {
        stop("no value field specified for yaml parameter '", name, "'",
             call. = FALSE)
      }

      # ensure we have a name
      param$name <- name

      # look for type info at the object level then value level
      param$type <- type_attr(param)
      if (is.null(param$type))
        param$type <- param_type(param$value)
    }

    # normalize parameter values
    param <- list(
      name = param$name,
      type = param$type,
      value = param_value(param$value)
    )

    # add the parameter
    full_params[[length(full_params) + 1]] <- param
  }

  # return params
  full_params
}


date_handler <- function(value) {
  value <- as.Date(value)
  attr(value, "type") <- "date"
  value
}

type_handler <- function(type) {
  function(value) {
    attr(value, "type") <- type
    value
  }
}


lines <- c(
  "---",
  "params:",
  "  tip: !date 2015-2-15",
  "  sap:",
  "    value: !date 2015-2-15",
  "  bad:",
  "    value: !file",
  "       ship: 10.7",
  "       flip: 20",
  "---",
  ""
)

p <- knit_params(lines)

str(p)


