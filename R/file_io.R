#' Automatic file reader
#'
#' Attempts to automatically recognize table type and import
#'  as tibble or data.table. Latest version incorporates *dots*
#'  to facilitate passing all possible options for each file reader type.
#'
#' @param file path to an tabular data file
#' @param reader string, Specify which package to read in your table, options include:
#'              read_excel, read.delim, read.csv, fread. NOTE: fread option
#'              will return a data.table, all others return data frame.
#' @param remove.empty.columns,remove.empty.rows logical, to remove columns or
#'            rows composed entirely of NA, "NA", or blank values
#' @param make.names.valid,make.names.unique logical, uses base make.names function
#'            to clean table names and make unique.
#' @param ... Pass any file-type appropriate arguments on to their handler
#'            functions. e.g., readxl::read_excel(), read.delim(), read.csv()
#' @return dataframe or data.table, see reader parameter.
#' @export
auto_read <- function(file, reader = "", stringsAsFactors = F,
                      remove.empty.columns = F, remove.empty.rows = F,
                      make.names.valid = T, make.names.unique = T, ...){

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

  if(make.names.valid) names(df) <- make.names(names(df))
  if(make.names.unique) names(df) <- make.names(names(df), unique = T)

  # remove empty rows and colums
  if(remove.empty.columns) df <- df[, !apply(df, 2, function(x){all( is.na(x) | x=="NA" | x == "" )})]
  if(remove.empty.rows)    df <- df[!apply(df, 1, function(x){all( is.na(x) | x=="NA" | x == "" )}), ]
  df
}

#' Automatic file writer
#'
#' Simple tab-delim wrapper to ensure normalized function parameters
#'
#' @param x dataframe or datatable
#' @param file path to write the output file
#' @param ... Pass any file-type appropriate arguments on to their handler
#'            functions.
#' @return data frame used as input
#' @export
auto_write <- function(x, file, ...){

  new.args     <- list(...)
  default.args <- list(append    = F,
                       quote     = T,
                       sep       = "\t",
                       na        = "NA",
                       row.names = F,
                       col.names = T )
  args <- c(new.args, default.args[!(names(default.args) %in% names(new.args))])

  write.table(x, file, append    = args$append    ,
              quote     = args$quote     ,
              sep       = args$sep       ,
              na        = args$na        ,
              row.names = args$row.names ,
              col.names = args$col.names  )
}
