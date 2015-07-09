#' Extract knit parameters from a document
#'
#' This function reads the YAML front-matter section of a document and returns a
#' list of any parameters declared there. This function exists primarily to
#' support the parameterized reports feature of the \pkg{rmarkdown} package,
#' however is also used by the knitr \code{\link{purl}} function to include
#' the default parameter values in the R code it emits.
#'
#' @param text Character vector containing the document text
#'
#' @return List of objects of class \code{knit_param} that correspond to the
#'   parameters declared in the \code{params} section of the YAML front matter.
#'   These objects have the following fields:
#'
#'   \describe{
#'     \item{\code{name}}{The parameter name.}
#'     \item{\code{type}}{The parameter type. This can be a standard R object
#'     type such as \code{character}, \code{integer}, \code{numeric}, or
#'     \code{logical} as well as the special \code{date}, \code{datetime}, and
#'     \code{file} types. See the \emph{Types} section below for additional
#'     details.}
#'     \item{\code{value}}{The default value for the parameter.}
#'   }
#'
#'   In addition, other fields included in the YAML may also be present
#'   alongside the name, type, and value fields (e.g. a \code{label} field
#'   that provides front-ends with a human readable name for the parameter).
#'
#' @details
#'
#' Parameters are included in YAML front matter using the \code{params} key.
#' This key can have any number of subkeys each of which represents a
#' parameter. For example:
#'
#' \preformatted{
#' ---
#' title: My Document
#' output: html_document
#' params:
#'   frequency: 10
#'   show_details: true
#' ---
#' }
#'
#' Parameter values can be provided inline as illustrated above or can be
#' included in a \code{value} sub-key. For example:
#'
#' \preformatted{
#' ---
#' title: My Document
#' output: html_document
#' params:
#'   frequency:
#'     value: 10
#' ---
#' }
#'
#' This second form is useful when you need to provide additional details
#' about the parameter (e.g. a \code{label} field as describe above).
#'
#' Parameter types are deduced implicitly based on the value provided. However
#' in some cases additional type information is required (for example when
#' a character vector needs to be interpreted as a date or as a file path).
#' In these cases a special type designater precedes the value. For example:
#'
#' \preformatted{
#' ---
#' title: My Document
#' output: html_document
#' params:
#'   start: !date 2015-01-01
#' ---
#' }
#'
#' @section Types:
#'
#' All of the standard R types that can be parsed using
#' \code{\link[yaml]{yaml.load}} are supported. These types are used
#' implicitly based on the \code{value} provided so no special type
#' designater is required. Built-in types include \code{character},
#' \code{integer}, \code{numeric}, and \code{logical}.
#'
#' In addition there are a number of custom types used to represent
#' dates and times as well as to note that character values have
#' special semantics (e.g. are the name of a file). These types are
#' specified by prefacing the YAML \code{value} with !\emph{typename},
#' for example:
#'
#' \preformatted{
#' ---
#' title: My Document
#' output: html_document
#' params:
#'   start: !date 2015-01-01
#'   end: !datetime 2015-01-01 12:30:00
#'   data: !file data.csv
#' ---
#' }
#'
#' Supported custom types include:
#'
#' \describe{
#'   \item{\code{date}}{A character value representing a date.
#'   The underlying date value is parsed from the character
#'   value using the \code{\link[base]{as.Date}} function.}
#'   \item{\code{datetime}}{A character value representing a
#'   date and time. The underlying datetime value is parsed from
#'   the character value using the \code{\link[base]{as.POSIXct}}
#'   function. Note that these values should always speicifed using
#'   UTC (Universal Time, Coordinated).}
#'   \item{\code{file}}{A character value representing the name
#'   of a file.}
#' }
#'
#' @export
knit_params = function(text) {

  # make sure each element is on one line
  text = split_lines(text)

  # read the yaml front matter and see if there is a params element in it
  yaml = yaml_front_matter(text)
  if (is.null(yaml)) return(list())

  yaml = enc2utf8(yaml)
  # parse the yaml using our handlers
  parsed_yaml = yaml::yaml.load(yaml, handlers = knit_params_handlers())

  # if we found paramters then resolve and return them
  if (is.list(parsed_yaml) && !is.null(parsed_yaml$params)) {
    resolve_params(mark_utf8(parsed_yaml$params))
  } else {
    list()
  }
}

# turn params into a named list of values
flatten_params = function(params) {
  res = list()
  for (param in params) res[[param$name]] = param$value
  res
}

# copied from rmarkdown:::mark_utf8
mark_utf8 = function(x) {
  if (is.character(x)) {
    Encoding(x) = 'UTF-8'
    return(x)
  }
  if (!is.list(x)) return(x)
  attrs = attributes(x)
  res = lapply(x, mark_utf8)
  attributes(res) = attrs
  res
}

# Extract the yaml front matter (if any) from the passed lines. The front
# matter is returned as a single-element character vector (with newlines
# delimited by \n) suitable for passing to yaml::load. This code is based on
# the partition_yaml_front_matter and parse_yaml_front_matter functions here:
# https://github.com/rstudio/rmarkdown/blob/master/R/output_format.R
yaml_front_matter = function(lines) {

  # verify that the first two front matter delimiters (---) are not preceded
  # by other content
  has_front_matter = function(delimiters) {
    length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1) &&
      (delimiters[1] == 1 || is_blank(head(lines, delimiters[1] - 1)))
  }

  # find delimiters in the document
  delimiters = grep("^---\\s*$", lines)

  # if it's valid then return front matter as a text block suitable for passing
  # to yaml::load
  if (!has_front_matter(delimiters)) return()

  # return the yaml as a single-element character vector if
  # appears to be valid yaml
  front_matter_lines = lines[(delimiters[1]):(delimiters[2])]
  if (length(front_matter_lines) <= 2) return()

  front_matter = front_matter_lines
  front_matter = front_matter[2:(length(front_matter) - 1)]
  # FIXME: this is only for apex on CRAN (https://github.com/thibautjombart/apex/pull/15)
  if (length(grep('^params:', front_matter)) == 0) return()
  front_matter = paste(front_matter, collapse = "\n")

  # ensure that the front-matter doesn't terminate with ':', so it won't cause a
  # crash when passed to yaml::load
  if (!grepl(":\\s*$", front_matter)) front_matter

}


# define custom handlers for knitr_params
knit_params_handlers = function() {

  # generic handler for intrinsic types that need a special 'type' designator as
  # a hint to front-ends (e.g. 'file' to indicate a file could be uploaded)
  type_handler = function(type) {
    force(type)
    function(value) {
      attr(value, "type") = type
      value
    }
  }

  list(

    `bool#yes` = function(value) {
      if (tolower(value) == "y") value else TRUE
    },

    `bool#no` = function(value) {
      if (tolower(value) == "n") value else FALSE
    },

    # date
    date = function(value) {
      value = as.Date(value)
      attr(value, "type") = "date"
      value
    },

    # datetime
    datetime = function(value) {
      value = as.POSIXct(value, tz = "GMT")
      attr(value, "type") = "datetime"
      value
    },

    # file
    file = type_handler("file"),

    # expr
    expr = function(value) {
      value = eval(parse(text = value))
      attr(value, "expr") = TRUE
      value
    }
  )
}


# resolve the raw params list into the full params data structure (with name,
# type, value, and other optional fields included)
resolve_params = function(params) {

  # get the type attribute (if any)
  type_attr = function(value) {
    attr(value, "type", exact = TRUE)
  }

  # was this value a result of an expression
  expr_attr = function(value) {
    isTRUE(attr(value, "expr", exact = TRUE))
  }

  # deduce type from attribute or class (also resolve date / datetime types
  # that we may have obtained from an expression)
  param_type = function(value) {
    type <- type_attr(value) %n% class(value)[[1]]
    if (type == "Date")
      "date"
    else if (type == "POSIXct")
      "datetime"
    else
      type
  }

  # return a parameter value with type and expr attributes stripped and
  # as a vector rather than list if it's unnamed
  param_value = function(value) {
    attr(value, "type") = NULL
    attr(value, "expr") = NULL
    if (is.null(names(value))) unlist(value) else value
  }

  # params we will return
  resolved_params = list()

  # iterate over names
  names = names(params)
  for (name in names) {

    # get the parameter
    param = params[[name]]

    # if it's not a list then a plain value was specified, create the list based
    # on the value
    if (!is.list(param)) {

      param = list(
        name = name,
        type = param_type(param),
        expr = expr_attr(param),
        value = param_value(param)
      )

    } else {

      # validate that the "value" field is included
      if (!"value" %in% names(param)) {
        stop("no value field specified for YAML parameter '", name, "'",
             call. = FALSE)
      }

      # ensure we have a name
      param$name = name

      # look for type info at the object level then value level
      param$type = type_attr(param)
      if (is.null(param$type)) param$type = param_type(param$value)
    }

    # normalize parameter value
    param$value = param_value(param$value)

    # add knit_param class
    param = structure(param, class = "knit_param")

    # add the parameter
    resolved_params[[length(resolved_params) + 1]] = param
  }

  # return params
  resolved_params
}
