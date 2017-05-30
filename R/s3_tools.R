#' Read tables from S3 bucket directly into df object
#'
#' Works for all delimited tables supported by toolboxR::Autoread().
#' which includes csv, tab-delim, and xlsx). Only
#'
#' @param s3.path Full character string uri of the table.
#' @param cache logical value as to whether local file should be
#'              left, default for cache=F removes file after transfer.
#' @return data.frame
#' @export
GetS3Table <- function(s3.path, cache = F, aws.args = "", ...){
  message('DEPRECATED. this function will be removed in future versions,
          please use new s3_get_table() method ')
  name  <- basename(s3.path)
  local.filename <- file.path("/tmp", name)
  system(  paste('aws s3 cp', s3.path, local.filename, aws.args, sep = " "))
  df <- toolboxR::auto_read(local.filename, ...)
  if(cache == FALSE){unlink(local.filename)}
  df
}

#' Write tab-delim tables to S3 bucket
#'
#' @param object a table object (df, data.table, tibble
#' @param s3.path Full character string uri of the table.
#' @return aws response code
#' @export
PutS3Table <- function(object, s3.path, sep = "\t", row.names = F, quote = F, aws.args = "", ...){
  message('DEPRECATED. this function will be removed in future versions,
          please use new s3_put_table() method ')
  name  <- basename(s3.path)
  local <- file.path("/tmp", name)
  write.table(object, local, sep = sep, row.names = row.names, quote = quote, ...)
  system(  paste('aws s3 cp', local, s3.path, "--sse", aws.args, sep = " "))
}