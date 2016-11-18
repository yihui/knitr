#' Extract knit parameters from a document
#'
#' This function reads the YAML front-matter section of a document and returns a
#' list of any parameters declared there. This function exists primarily to
#' support the parameterized reports feature of the \pkg{rmarkdown} package,
#' however is also used by the knitr \code{\link{purl}} function to include
#' the default parameter values in the R code it emits.
#'
#' @param text Character vector containing the document text
#' @param evaluate If TRUE, expression values embedded within the YAML will be
#' evaluated. This is the default. When FALSE, parameters defined by an
#' expression will have the parsed expression in its \code{value} field.
#'
#' @return List of objects of class \code{knit_param} that correspond to the
#'   parameters declared in the \code{params} section of the YAML front matter.
#'   These objects have the following fields:
#'
#'   \describe{
#'     \item{\code{name}}{The parameter name.}
#'     \item{\code{value}}{The default value for the parameter.}
#'     \item{\code{expr}}{The R expression (if any) that yielded the default value.}
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
#' You can also use R code to yield the value of a parameter by prefacing the value
#' with \code{!r}, for example:
#'
#' \preformatted{
#' ---
#' title: My Document
#' output: html_document
#' params:
#'   start: !r Sys.Date()
#' ---
#' }
#'
#' @export
knit_params = function(text, evaluate = TRUE) {

  # make sure each element is on one line
  text = split_lines(text)

  # read the yaml front matter and see if there is a params element in it
  yaml = yaml_front_matter(text)
  if (is.null(yaml)) return(list())

  knit_params_yaml(enc2utf8(yaml), evaluate = evaluate)
}

#' Extract knit parameters from YAML text
#'
#' This function reads the YAML front-matter that has already been extracted
#' from a document and returns a list of any parameters declared there.
#'
#' @param yaml Character vector containing the YAML text
#' @param evaluate If TRUE, expression values embedded within the YAML will be
#' evaluated. This is the default. When FALSE, parameters defined by an
#' expression will have the parsed expression in its \code{value} field.
#'
#' @return List of objects of class \code{knit_param} that correspond to the
#' parameters declared in the \code{params} section of the YAML. See
#' \code{\link{knit_params}} for a full description of these objects.
#'
#' @seealso \code{\link{knit_params}}
#'
#' @export
knit_params_yaml = function(yaml, evaluate = TRUE) {
  # parse the yaml using our handlers
  parsed_yaml = yaml::yaml.load(yaml, handlers = knit_params_handlers(evaluate = evaluate))

  # if we found paramters then resolve and return them
  if (is.list(parsed_yaml) && !is.null(parsed_yaml$params)) {
    resolve_params(parsed_yaml$params, evaluate = evaluate)
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
      (delimiters[1] == 1 || is_blank(head(lines, delimiters[1] - 1))) &&
      grepl("^---\\s*$", lines[delimiters[1]])
  }

  # find delimiters in the document
  delimiters = grep("^(---|\\.\\.\\.)\\s*$", lines)

  # if it's valid then return front matter as a text block suitable for passing
  # to yaml::load
  if (!has_front_matter(delimiters)) return()

  # return the yaml as a single-element character vector if
  # appears to be valid yaml
  front_matter_lines = lines[(delimiters[1]):(delimiters[2])]
  if (length(front_matter_lines) <= 2) return()

  front_matter = front_matter_lines
  front_matter = front_matter[2:(length(front_matter) - 1)]
  if (length(grep('^params:', front_matter)) == 0) return()  # no params in YAML
  front_matter = paste(front_matter, collapse = "\n")

  # ensure that the front-matter doesn't terminate with ':', so it won't cause a
  # crash when passed to yaml::load
  if (!grepl(":\\s*$", front_matter)) front_matter
}


# define custom handlers for knitr_params
knit_params_handlers = function(evaluate = TRUE) {

  # generic handler for r expressions where we want to preserve both the original
  # code and the fact that it was an expression.
  expr_handler = function(value) {
    expression = parse_only(value)
    transformed_value = if (evaluate) {
      eval(expression)
    } else {
      # When we are not evaluating, provide the parsed expression as the transformed value
      expression
    }

    wrapped = list(
        value = transformed_value,
        expr = value)
    wrapped = structure(wrapped, class = "knit_param_expr")
    wrapped
  }

  list(

    # r expressions where we want to preserve both the original code
    # and the fact that it was an expression.
    r = expr_handler,
    expr = expr_handler,

    # date and datetime (for backward compatibility with previous syntax)
    date = function(value) {
      value = as.Date(value)
      value
    },
    datetime = function(value) {
      value = as.POSIXct(value, tz = "GMT")
      value
    },

    # workaround default yaml parsing behavior to allow keys named 'y' and 'n'
    `bool#yes` = function(value) {
      if (tolower(value) == "y") value else TRUE
    },
    `bool#no` = function(value) {
      if (tolower(value) == "n") value else FALSE
    }
  )
}


# resolve the raw params list into the full params data structure (with name,
# type, value, and other optional fields included)
resolve_params = function(params, evaluate = TRUE) {

  # params we will return
  resolved_params = list()

  # iterate over names
  names = names(params)
  for (name in names) {

    # get the parameter
    param = params[[name]]

    if (inherits(param, "knit_param_expr")) {
      # We have a key: !r expr
      param = list(
          expr = param$expr,
          value = param$value)
    } else if (is.list(param)) {
      if ("value" %in% names(param)) {
        # This looks like a complex parameter configuration.
        value = param$value
        if (inherits(value, "knit_param_expr")) {
          # We have a key: { value: !r expr }
          param$expr  = value$expr
          param$value = value$value
        }
      } else {
        stop2("no value field specified for YAML parameter '", name, "'")
      }
    } else {
      # A simple key: value
      param = list(value = param)
    }

    # param is now always a named list. record name and add knit_param class.
    param$name = name
    param = structure(param, class = "knit_param")

    # add the parameter
    resolved_params[[name]] = param
  }

  # return params
  resolved_params
}
