#  -----------------------------------------------------------------------------
#' Establish an environment which defines the current S3 bucket metadata.
#'
#' This environment is required for all of my S3... functions. Sets bucket name,
#' defines and creates a local cache directory, tracks working directory
#'
#' @param bucket base name for s3 bucket e.g. s3://bucket
#' @param wd character string path, optional to define a working prefix
#' @param local.cache character string, path of a local scratch directory
#'
#' @return environment e returned invisibly to global environment
#' @export
s3_set_env <- function(bucket        = NULL,
                       wd          = NULL,
                       local.cache = "/tmp/s3.cache"){
  e <<- new.env(parent = emptyenv())
  if( !is.null(bucket) ) e$bucket <- bucket

  if( !is.null(wd) ) e$wd <- wd

  e$local <- local.cache
  if( !dir.exists(e$local) ) dir.create(e$local)
  if( !dir.exists(e$local) ) stop('local scratch directory does not exist')
}

#  -----------------------------------------------------------------------------
#' Read tables from S3 bucket directly into df object
#'
#' Works for all delimited tables supported by toolboxR::auto_read().
#' which includes csv, tab-delim, and xlsx). Only
#'
#' @param s3.path Full character string uri of the table.
#' @param cache logical value as to whether local file should be
#'              left, default for cache=F removes file after transfer.
#' @return data.frame
#' @export
s3_put_table <- function(table, pre, env = e, cache = F, aws.args = "", ...){

  if( is.null(env$bucket) ){stop('please set bucket environment: set.s3.bucket(bucket = "s3://bucket-name")')}

  name  <- basename(s3.path)
  local <- file.path("/tmp", name)
  write.table(object, local, sep = sep, row.names = row.names, quote = quote, ...)
  system(  paste('aws s3 cp', local, s3.path, "--sse", aws.args, sep = " "))
}

#  -----------------------------------------------------------------------------
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
s3_get_table <- function(pre, env = e, cache = F, aws.args = "", ...){
return("in development")
  # if( is.null(env$bucket) ){stop('please set bucket environment: set.s3.bucket(bucket = "s3://bucket-name")')}
  #
  # name    <- basename(pre)
  # s3.path <- file.path(e$bucket, pre)
  # local.filename <- file.path("/tmp", name)
  # system(  paste('aws s3 cp', s3.path, local.filename, aws.args, sep = " "))
  # df <- toolboxR::auto_read(local.filename, ...)
  # if(cache == FALSE){unlink(local.filename)}
  # df
}

s3write_using <- function(x, FUN, ..., object, bucket, opts = NULL) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    value <- FUN(x, tmp, ...)
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)
    if (is.null(opts)) {
        r <- put_object(file = tmp, bucket = bucket, object = object)
    } else {
        r <- do.call("put_object", c(list(file = rawConnectionValue(tmp), bucket = bucket, object = object), opts))
    }
    return(invisible(r))
}


s3read_using <- function(FUN, ..., object, bucket, opts = NULL) {

    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    }
    object <- get_objectkey(object)

    tmp <- tempfile(fileext = paste0(".", tools::file_ext(object)))
    if (is.null(opts)) {
        r <- save_object(bucket = bucket, object = object, file = tmp)
    } else {
        r <- do.call("save_object", c(list(bucket = bucket, object = object, file = tmp), opts))
    }
    return(FUN(tmp, ...))
}

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
  name  <- basename(s3.path)
  local <- file.path("/tmp", name)
  write.table(object, local, sep = sep, row.names = row.names, quote = quote, ...)
  system(  paste('aws s3 cp', local, s3.path, "--sse", aws.args, sep = " "))
}