
#' Attempts to automatically recognize file type and import
#'  as dataframe
#'
#' @param file path to an xlsx binary file
#' @param sheet number or name of workbook sheet to import.
#'              Defaults to first sheet.
#' @param check.names logical. If TRUE then the names of the variables in the 
#'                    data frame are checked to ensure that they are syntactically 
#'                    valid variable names. If necessary they are adjusted 
#'                    (by make.names) so that they are, and also to ensure 
#'                    that there are no duplicates.
#' @param skip number of lines to skip before import.
#' @return dataframe
#' @export
AutoRead <- function(file, sheet = 1, check.names = T, skip = 0, colnames = T){
  tryCatch({
    if(endsWith(file,"xlsx") | endsWith(file, "xls")){
      t <- readxl:::xlsx_col_types(file)
      df <- readxl::read_excel(path = file, 
                               sheet = sheet, 
                               skip = skip, 
                               col_names = colnames,
                               col_types = t )
      if(check.names){
        df <- df[,!is.na(names(df))]
        names(df) <- make.names(names(df), unique = TRUE)
        }
      df
    }else if (endsWith(file, "txt")){
      df <- read.delim(file = file, stringsAsFactors = F, header = colnames)
      if(skip > 0){
        names(df) <- df[skip+1,]
        df <- df[(skip+1):nrow(df),]
      }
      if(check.names){
        df <- df[,!is.na(names(df))]
        names(df) <- make.names(names(df), unique = TRUE)
      }

      df
      
    }else if (endsWith(file, "csv")){
      df <- read.csv(file = file, stringsAsFactors = F, header = colnames)
      if(skip > 0){
        names(df) <- df[skip+1,]
        df <- df[(skip+1):nrow(df),]
      }
      if(check.names){
        df <- df[,!is.na(names(df))]
        names(df) <- make.names(names(df), unique = TRUE)
      }
      
      df
    }
  },error=function(e){stop("File unable to be imported")})
}

