#' Javascript V8 engine (require the V8 package)
#'
#' example at \link{https://gist.github.com/stla/10718cda54bc5c0aa6fb}
#'
#' @param options a list of chunk options
#'
#' @import V8
#' @export
engine_V8 = function(options) {
  require(V8)
  context <- ifelse(!is.null(options$V8.context), options$V8.context, "ct")
  if(!exists(context)){
    assign(context, new_context(), envir = .GlobalEnv)
    if(!is.null(options$V8.libraries)){
      for(library in options$V8.libraries){
        eval(parse(text=sprintf('%s$source("%s")', context, library)))
      }
    }
  }
  code <- as.character(c(options$code))
  if(!eval(parse(text=sprintf("%s$validate(code)", context)))) stop("unvalid javascript code")
  if(!is.element(options$results, c("hide", "last"))){
    code <- group_src_V8(code, context)
    output <- sapply(code, function(code) eval(parse(text=sprintf("%s$eval(code)", context))) )
    code <-  sapply(code, function(code) knitr:::wrap.source(list(src= paste(code, collapse = "\n")), options))
  }else{
    if(options$results=="last") output <- eval(parse(text=sprintf("%s$eval(code)", context)))
    code <-  knitr:::wrap.source(list(src= paste(code, collapse = "\n")), options)
    if(options$results=="hide") return(code)
    return(c(code, knitr:::wrap.character(output, options)))
  }
  return(c(sapply(seq_along(code), function(i) c(code[i], knitr:::wrap.character(output[i], options)))))
}

#' Group V8 source lines
#'
#' Group V8 source lines into complete expressions (using a brutal-force method)
group_src_V8 = function(code, context) {
  if ((n <- length(code)) < 1) return(list(code))
  i = i1 = i2 = 1
  x = list()
  while (i2 <= n) {
    piece = code[i1:i2]
    if (eval(parse(text=sprintf("%s$validate(piece)", context))) &&  eval(parse(text=sprintf('%s$eval(piece)!="undefined"', context)))) {
      x[[i]] = piece; i = i + 1
      i1 = i2 + 1 # start from the next line
    }
    i2 = i2 + 1
  }
  if (i1 <= n) parse(text = piece)  # must be an error there
  x
}
