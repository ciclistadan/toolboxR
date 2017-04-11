#' Attempts to automatically recognize table type and import
#'  as tibble. Latest version incorporates *dots* to facilitate
#'  passing all possible options for each file reader type.
#'
#' @param file path to an xlsx binary file
#' @param reader Specify which package to read in your table, options include:
#'              read_excel, read.delim, read.csv, fread. NOTE: fread option
#'              will return a data.table, all others return data frame.
#' @param remove.empty.columns,remove.empty.rows logical to remove columns or
#'            rows composed entirely of NA, "NA", or blank values
#' @param ... Pass any file-type appropriate arguments on to their handler 
#'            functions. e.g., readxl::read_excel(), read.delim(), read.csv()
#' @return dataframe or data.table, see reader parameter.
#' @export
auto_read <- function(file, reader = "", stringsAsFactors = F, remove.empty.columns = T, remove.empty.rows = T, ...){
  
  if( reader !=  "" ){                                    tool <- reader
  }else if(endsWith(file,"xlsx") | endsWith(file, "xls")){tool <- "read_excel"
  }else if(endsWith(file,"csv") ){                        tool <- "read.csv"
  }else if(endsWith(file,"txt")  | endsWith(file, "tsv")){tool <- "read.delim"
  }else{ stop("file reader was not set or an automatically recognized (xlsx, xls, txt, tsv, csv)") }
  
  if(       tool == "read_excel" ){ df <- readxl::read_excel(file, ...)
  }else if( tool == "read.delim") { df <- dplyr::as.tbl(read.delim(file, stringsAsFactors = stringsAsFactors, ...))
  }else if( tool == "read.csv")   { df <- dplyr::as.tbl(read.csv(file, stringsAsFactors = stringsAsFactors, ...))
  }else if( tool == "fread")      { df <- data.table::fread(file, ...)
  }else{ stop("tool not set") }
  
  # remove empty rows and colums
  if(remove.empty.columns) df <- df[, !apply(df, MARGIN = 2, function(x){all( is.na(x) | x=="NA" | x == "" )})]
  if(remove.empty.rows)    df <- df[!apply(df, MARGIN = 2, function(x){all( is.na(x) | x=="NA" | x == "" )}), ]
  df
}


#' Deprecated, use auto_read(). 
#' Attempts to automatically recognize file type and import
#'  as dataframe
#'
#' @param file path to an xlsx binary file
#' @param ... Pass any file-type appropriate arguments on to their handler 
#'            functions. e.g., readxl::read_excel(), read.delim(), read.csv()
#' @return dataframe
#' @export
AutoRead <- function(file, ...){
  tryCatch({
    if(endsWith(file,"xlsx") | endsWith(file, "xls")){
      t <- readxl:::xlsx_col_types(file)
      df <- readxl::read_excel(file, ...)
      if(check.names){
        df <- df[,!is.na(names(df))]
        names(df) <- make.names(names(df), unique = TRUE)
        }
      df
    }else if (endsWith(file, "txt")){
      df <- read.delim(file = file, 
                       stringsAsFactors = F, 
                       header = colnames, 
                       check.names = check.names)
      if(skip > 0){
        names(df) <- df[skip+1,]
        df <- df[(skip+1):nrow(df),]
      }
      # if(check.names){
      #   df <- df[,!is.na(names(df))]
      #   names(df) <- make.names(names(df), unique = TRUE)
      # }

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

