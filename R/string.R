#' Remove leading and trailing whitespace characters from a string
#'
#' does not coerce non-character vectors so you can safely use this on an
#  entire df.
#
#' @param x Character string or vector.
#' @export
chomp <- function(x){
  if(is.character(x)){
  gsub("^\\s+|\\s+$","",x, perl = T)
    }else{x}
}


#' Automated date parsing from mixed vector of strings
#'
#' This function will attempt to match each string value
#' to an appropriate date and export harmonized string format.
#' Currently it is limited to recognizing mixtures of Excel
#' integer dates and string dates formatted in a single specified
#' order (e.g. mdy for month-day-year).
#'
#' @param input.order Character string designating what order the input string
#' currently exist. Possible values= "dmy", "ymd", and "mdy". Additional symbols
#' are ignored (2017-10-31 == 2017/10/31 == 20171031)
#' @param output.format Character string passed to format function to specify desired output
#' @param default Non-NULL character to return if unable to parse date. Specifying
#' default = "keep" will retain the unchanged original value.
#' @return character vector of parsable dates in format *output.format*
#' @export
format_dates <- function(x, input.order= "dmy", output.format = "%Y-%m-%d", default = "keep"){

  unlist(lapply(x, function(y){

    # can't do anything with NA values except replace with default as specified
    if( is.na(y) ){
      ifelse(default == "keep", NA, default)

    # Excel integer formatting
    ## Excel is said to use 1900-01-01 as day 1 (Windows default) or
    ## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
    ## incorrectly treating 1900 as a leap year.
    ## rejects integer dates more than 100 years old
    }else if(
      tryCatch( {
      if(   (!is.na(as.integer(y))) & (as.integer(y) > 5000) ){
        TRUE
      }else{
          FALSE
        }
                        }, warning = function(w){FALSE}) ){
      format(as.Date(as.integer(y), origin = "1899-12-30"), output.format)

    # parse as date if possible
    }else if( input.order == "dmy" & !is.na(lubridate::dmy(y, quiet = T)) ){
      format(lubridate::dmy(y), output.format)
    }else if( input.order == "ymd" & !is.na(lubridate::ymd(y, quiet = T)) ){
      format(lubridate::ymd(y), output.format)
    }else if( input.order == "mdy" & !is.na(lubridate::mdy(y, quiet = T)) ){
      format(lubridate::mdy(y), output.format)

    # return unparsed versions
    }else if( !is.na(default) & default == "keep" ){
      paste0("[",y,"]")
    }else{
      default
    }
  }))

}

# Supports both quoted/unquoted values but value cannot include delimiters
parse_keyvals <- function(x){

  # assume quoted values
  foo   <- unlist(strsplit(x, split = "; "))

  y <- gsub(  "^(.*?)=[\\\"]*(.*?)[\\\"]*$", "\\2", foo)
  names(y) <- gsub(  "^(.*?)=[\\\"]*(.*?)[\\\"]*$", "\\1", foo)
  y
}