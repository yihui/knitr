# Do the base64 encoding in C ( provided by other packages ) or in R ( 
# if there is no such package existed )
#
# @param file_name   the file to be base64 encoded
# @return base64 encoded string
base64_encode <- function(file_name) {
	packages.list <- rownames(installed.packages())
	# check package lists
	for ( pkg_name in names(base64_encoder.candidate)) {
		if (sum( packages.list == base64_encoder.candidate[ pkg_name ] )) {
			break
		}
	}
	base64_encoder.wrapper(pkg_name)(file_name)
}

# candidates of encoder and related package
base64_encoder.candidate <- c(
	markdown = "markdown:::.b64EncodeFile",
	RCurl = "RCurl::base64Encode",
	utils = "base64_encoder.R"
	)

# specify the input type of the related package
attr(base64_encoder.candidate,"type") <- c(
	markdown = "file",
	RCurl = "string",
	utils = "string"
	)

# wrapper of encoder whose input is string
base64_encoder.string_wrapper <- function(f) {
	return(function(file_name) { 
		fcontent <- readBin(file_name,what="raw",n=file.info(file_name)$size)
		paste("data:", get_mime_type(file_name), ";base64,",as.character(f(fcontent)),sep="")
	})	
}

# general wrapper for unit test
base64_encoder.wrapper <- function(pkg_name) {
	encoder.origin <- eval(parse(text=base64_encoder.candidate[pkg_name]))
	encoder <- switch( EXPR = attr(base64_encoder.candidate,"type")[pkg_name], 
		"file" = encoder.origin,
		"string" = base64_encoder.string_wrapper(encoder.origin)
		)
	encoder
}

# base64 encoder in R
base64_table <- c(LETTERS, letters, 0:9, '+', '/')

base64_encoder.R <- function( raw_string ) {
	n <- length(raw_string)
	int_string <- as.integer(raw_string)
	retval <- rep(NA, (n + 2) / 3 * 4)
	index <- 0
	input_index <- 1
	while(n > 2) {
		retval[index <- index + 1] <- base64_table[int_string[ input_index ] %/% 4 + 1]
		retval[index <- index + 1] <- base64_table[ 16 * (int_string[ input_index ] %% 4) + int_string[ input_index + 1 ] %/% 16 + 1]
		retval[index <- index + 1] <- base64_table[ 4 * (int_string[ input_index + 1 ] %% 16) + int_string[ input_index + 2 ] %/% 64 + 1]
		retval[index <- index + 1] <- base64_table[int_string[ input_index + 2 ] %% 64 + 1]
		input_index <- input_index + 3
		n <- n - 3
	}
	if(n) {
		retval[index <- index + 1] <- base64_table[int_string[ input_index ] %/% 4 + 1]
		if (n > 1) {
			retval[index <- index + 1] <- base64_table[16 * (int_string[ input_index ] %% 4) + int_string[ input_index + 1] %/% 16 + 1]
			retval[index <- index + 1] <- base64_table[ 4 * (int_string[ input_index + 1 ] %% 16) + 1]
			retval[index <- index + 1] <- "="
		} else {
			retval[index <- index + 1] <- base64_table[16 * (int_string[ input_index ] %% 4) + 1]
			retval[index <- index + 1] <- "="
			retval[index <- index + 1] <- "="
		}
	}
	paste(retval[!is.na(retval)],collapse="")
}

# Function copied from package markdown to specify the mimetype of the figure
#
# @param file the filename
# @return character the mimetype
get_mime_type <- function(file) 
{
    if (grepl(".png$", file, perl = TRUE, ignore.case = TRUE)) 
        "image/png"
    else if (grepl(".gif$", file, perl = TRUE, ignore.case = TRUE)) 
        "image/gif"
    else if (grepl("(.jpg$|.jpeg$)", file, perl = TRUE, ignore.case = TRUE)) 
        "image/jpeg"
    else if (grepl(".tiff?$", file, perl = TRUE, ignore.case = TRUE)) 
        "image/tiff"
    else ""
}
