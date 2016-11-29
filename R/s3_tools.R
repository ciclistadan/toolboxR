#' Helper function for writing tab-delimited tables to an S3 bucket 
#'
#' A helper function to build S3 path-specific tools. Since you will commonly write
#' to a single path in an S3 bucket repeatedly, this simplifys the process by
#' pre-populating longer arguments such as bucket and path strings.
#'
#' @param bucket S3 bucket name
#' @param path S3 prefix structure to prepend to the name, 
#'                leading and trailing slashes are cleaned up.
#' @param obj The Dataframe  you want to write
#' @param name File name for the table
#' @return Response code from AWS, "0" when successful
#' @export
WriteTableToS3 <- function(bucket, path = "", arguments = ""){
  function(obj, name){
    
    base.name <- basename(name)
    local.path <- file.path("/tmp",base.name)
    
    if(path == ""){
      s3.path    <- file.path(gsub("\\/+$","",bucket), gsub("^\\/+|\\/+$","",name))
    }else{
    s3.path    <- file.path(gsub("\\/+$","",bucket), gsub("^\\/+|\\/+$","",path), gsub("^\\/+|\\/+$","",name))
    }
    
    write.table(obj, local.path, row.names = F, col.names = T, sep = "\t", quote = F)
    command.string <- paste('aws s3 cp', local.path, s3.path , '--sse',arguments , sep = " ")
    print(command.string)
    system( command.string )
    response <- system('echo $?', intern = T)
    if( response == 0 ){
      unlink(local.path)
    }else{
      warning(paste("Error writing",name, "to S3", sep = " "))
    }
    response
  }
}
# f <- WriteTableToS3(bucket = "s3://celgene-helsinki-bucket", path = "AML")
# df <- data.frame(one = c(1,2,3), two = c(1,2,3))
# f(obj = df, name= "upload_tracking/test.txt")
# 
# 

