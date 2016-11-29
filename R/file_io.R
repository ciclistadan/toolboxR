
#' Attempts to automatically recognize file type and import
#'  as dataframe
#'
#' @param file path to an xlsx binary file
#' @param sheet number or name of workbook sheet to import.
#'              Defaults to first sheet.
#' @return dataframe
#' @export
autoread <- function(file, sheet = 1){
  tryCatch({
    if(endsWith(file,"xlsx") | endsWith(file, "xls")){
     tmp <-  readxl::read_excel(file = file, sheet = sheet)
    }else if (endsWith(file, "txt")){
      read.delim(file = file, stringsAsFactors = F)
    }else if (endsWith(file, "csv")){
      read.csv(file = file, stringsAsFactors = F)
    }
  },error=function(e){stop("File unable to be imported")})
}
