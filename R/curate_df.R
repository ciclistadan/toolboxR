#############################################
#' Renames specified columns in data.frame
#'
#' @param old current column name
#' @param new replacement column name
#' @param df which data frame to replace it in
#' @return A df with changed column names
#' @export
rename_columns <- function(old, new, df){
  n <- names(df)
  if(all(old %in% n)){
    n <- gsub(old, new, n)
    names(df) <- n
    df
  }
  else warning("Column name not found")

}

#############################################
#' Sort specified columns to front of a data.frame
#'
#' @param names a char vector of current columns you want
#'              to move to the front of the df
#' @param df which data frame to replace it in
#' @param reverse [not yet implemented] move named columns
#'                to end of df.
#' @return A df with rearranged columns
#' @export
move_columns <- function(names, df, reverse = F){
  n  <- names(df)

  if(all(names %in% n)){
    df <- df[,c(names, n[!(n %in% names)])]
  } else {
    warning("Column name not found")
    }
  return(df)
}


#' Format columns names to lower, UPPER or Title.Case
#'
#' @param df which data frame to replace it in
#' @param convert_case character default="lower", also accepts "Title", "UPPER"
#'                     specify convert.case = "" to leave alone. NOTE: make_unique
#'                     overrides this by always converting to "lower"
#' @param replace_symbol character replace all non-alpha characters with
#'              specified character (e.g. "_" or "."), removes leading/trailing
#' @param make_unique boolean use make.names(unique = T) to make enforce
#'                    valid unique names
#' @return df with renamed columns
#' @export
format_column_names <- function(df,
                                type           = "lower",
                                replace_symbol = ".",
                                make_unique     = T){

  type <- tolower(type)
  n    <- names(df)

  # Convert case
  if( type == "lower" ){
    n <- tolower(n)
  }else if (type == "upper") {
    n <- toupper(n)
  }else if (type == "title") {
    n <- gsub("^(.)", "\\U\\1", n , perl = T)
    n <- gsub("\\.(.)", ".\\U\\1", n , perl = T)
  }

  # Make unique
  if( make_unique ){
    n <- make.names(names = n, unique = T, allow_ = T)
  }

  # Replace symbols
  if( !is.null(replace_symbol) &&
      !is.na(replace_symbol) &&
      is.character(replace_symbol) ){

    n <- gsub("([^a-zA-Z0-9]+)", replace_symbol , n)
    n <- gsub(paste0("^(\\",replace_symbol,"+)|(\\",replace_symbol,"+)$"), "", n)
  }

  names(df) <- n
  df
}

#############################################
#' Sort specified columns to front of a data.frame
#'
#' @param names a char vector of current columns yto remove
#' @param df data frame containing columns "names"
#' @return A df with rearranged columns
#' @export
remove_columns <- function(names, df){
  n  <- names(df)

  if(all(names %in% n)){
    df <- df[,c( n[ !(n %in% names) ])]
  } else {
    warning("Column name not found")
  }
  return(df)
}


#' Automated normalization of string formats
#'
#' I commonly use this to clean abnormally formatted df variable names.
#'
#'
#' @param x Character vector.
#' @param case Character value "upper" or "lower"
#' @param length integer length which we will trim longer strings to. If non-alphanumeric
#'               characters exist (e.g., dashes or spaces) we will trim +/-5 characters
#'               of this length to retain intact words.
#' @return character vector
#' @export
normalize_strings <- function(x, case = "upper", length = 20){

  # substitute non-alphanumeric characters
  n <- gsub("[^[:alnum:]]+", ".", x)
  # chomp
  n <- gsub("^[ \\.]+|[ \\.]+$", "", n)

  # Raw upper case helps differentiate from lower case
  if(case == "upper"){n <- toupper(n)
  }else if (case == "lower") {n <- tolower(n)}

  n <- unlist(lapply(n, function(x){
    if( is.na(x) ){ x <- "COL"}

    char.vector <- unlist(strsplit(x, split = ""))
    gaps <- grep("\\.", char.vector)

    if(any(gaps > length-5 & gaps < length+5)){
      e <- min(gaps[gaps > length-5 & gaps < length+5])-1
    }else{
      e <- min(length(char.vector), length)
    }
    paste(char.vector[1:e], collapse = "")
  }))

  # append serial if duplicated
  n <- make.unique(n)

  if( any(duplicated(n)) ){
    warning(paste("Abbreviated column titles are non-unique:", paste(unique(n[duplicated(n)]), collapse = "; "), sep = " "))
  } else{n}

  n
}

#
# mutate_cond Create a simple function for data frames or data tables that can
# be incorporated into pipelines. This function is like mutate but only acts on
# the rows satisfying the condition:
#
#  usage: DF %>% mutate_cond(measure == 'exit', qty.exit = qty, cf = 0, delta.watts = 13)
#
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
