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

#############################################
#' Format columns names to lower, UPPER or Title.Case
#'
#' @param df which data frame to replace it in
#' @param type default="title", also accepts "lower" or "upper"
#' @return df with renamed columns
#' @export
format_column_names <- function(df, type = "title"){

  n <- tolower(names(df))

  if(type == "title"){
    n <- gsub("^(.)", "\\U\\1", n , perl = T)
    n <- gsub("\\.(.)", ".\\U\\1", n , perl = T)
  } else if (type == "upper") {
    n <- toupper(n)
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
