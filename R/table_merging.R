#' Collapse a complex array into unique components
#'
#' Given a vector or list, break into atomic units, remove
#'  any redundancies and return a vector of unique elements
#'
#' @param x Vector or list. This can contain all valid element types, however
#'         the current version always returns a character type. 
#' @param delim A non-null character use to both split complex character 
#'              strings, and delimit output string elements containing more than 
#'              one element.
#' @param unique Logical If TRUE, warns when more than one element is returned 
#'               from x after simplification.
#' @param na.rm Logical if TRUE individual NA elements will be removed before
#'              simplification.
#' @param ignore.case Logical should character string comparisons ignore case. 
#'                    A FALSE argument could return "FOO; Foo; foo" while a TRUE
#'                    argument would only return "FOO". 
#' @param sort Logical should the final values be sorted before return.                                 
#' @param null.return Value to be returned if na.rm = TRUE and non-NA values 
#'             do not exist. By default NA will be returned for null return even
#'             if na.rm = TRUE, set this value to change this behavior.
#' @return delimiter separated character string.
#' @export
Simplify <- function(x, delim = "; ", unique = F, na.rm = T, ignore.case = T, sort = T, null.return = NA){
  
  # clean up character elements
  x <- unlist(lapply(x, function(y){
    if(is.character(y)){
      # split complex strings
      y <- strsplit(y, split = delim)
    }else{y}
  }))
  
  # chomp each value
  x <- gsub("^\\s+", "", x)
  x <- gsub("\\s+$", "", x)
  
  # remove duplicate values
  if( length(unique(toupper(x))) == length(unique(x)) ){
    x <- unique(x)
  }else{
    x <- unique(toupper(x))
  }
  
  # get rid of NA fields
  if(na.rm == TRUE){
    # remove both actual NAs and character "NA"s
    x <- x[!is.na(x)]
    x <- x[x != "NA"]
    }
  # fix null returns
  if( identical(x, character(0)) ){x <- null.return}
  # sort if desired
  if(sort){x <- x[order(x)]}
  
  # collapse to final string
  out <- paste(x, collapse = delim)
  
  # warn on non-unique return
  if(unique & (length(x) > 1) ){
    warning(paste0("Elements (",out,") did not collapse to unique as required"))
  }
  out
}

#' Append DF using specified overwrite modes
#'
#' Given a main df, either replace, append, or warn of conflict for values
#'  found in matching column and row of new df. Row is matched by a paired id
#'  value.
#'
#' @param main,new data frames, each with column "id".
#' @param id column name used to designate matching rows.
#' @param mode "append" any new values to preexisting values using delimiter,
#'             "replace" any preexisting values with new value, or "safe" (DEFAULT)
#'             to only write new value if no preexisting valuethe search column name.
#' @param verbose not yet implemented
#' @param delim string value used to append new values during "append" mode.
#' @return merged df
#' @export
append_df <- function(main, new, id = "Patient",
                      mode = "safe", verbose = TRUE, delim = "; "){
  
  if(!id %in% names(new)){
    message(paste0("dataframe does not contain specified id column: ", id))
    return(main)
  }else if(sum(names(new) %in% names(main)) == 1){
    message(paste0("dataframe does not contain additional columns to add"))
    return(main)
  }
  
  # subset new df to only columns in main
  new <- new[,names(new) %in% names(main)]
  
  # add new rows if new patients are found
  if( any(!new[[id]] %in% main[[id]]) ) {
    for(i in unique(new[[id]][!new[[id]] %in% main[[id]]])){
      main[nrow(main)+1,id] <- i
    }
  }
  
  # add column to force merged sorting
  main[['table']] <- "main"
  new[['table']]  <- "new"
  
  p <- new[[id]]
  n <- names(new)
  
  # extract the rows and columns of main that are in new
  main_subset   <- main[main[[id]] %in% p ,n]
  
  # merge without any "by" arguments duplicates non-identical rows
  m <- merge(main_subset, new, all = T)
  m <- m[ order(m[,id], m[,"table"]), ]
  
  # lapply for each patient, this allows to
  #  subset to just the rows of a single patient
  l <- lapply(unique(m[[id]]), function(identifier){
    
    # inside the apply, we then perform the merge for each column set
    # x is a character vector of the available values that needs to be collapsed
    a<- apply(m[m[[id]] == identifier, !names(m) %in% c("table")], MARGIN = 2, function(x){
      
      # capture pre-existing vs new values separately
      original_values <- x[1]
      new_values     <- x[2:length(x)]
      had_value      <- !is.na(original_values) & original_values != ""
      has_new_value  <- any(!is.na(new_values)) & any(new_values != "")
      
      original_values <- Simplify(original_values)
      new_values <- Simplify(new_values)
      
      # Cases:
      # if didn't have a value,      return the new value, this works with blank new values too
      # else if value is the same,   return the value
      # else if the mode is append,  join them and return
      # else if the mode is replace, return the new values
      # else if the mode is safe,    return the old value and warning
      # else warning
      
      if( had_value == FALSE ){
        out <- new_values
      }else if(has_new_value & all(new_values == original_values)){
        out <- new_values
      }else if(mode == "append"){
        out <- Simplify(c(new_values,original_values))
      }else if(mode == "replace"){
        out <- new_values
      }else if(mode == "safe"){
        #TODO: don't warn if replacement is blank.
        warning(paste0("Identifier:", identifier, " has existing value (",paste(original_values, collapse = "; "),"), attempted overwrite with (",paste(new_values, collapse = "; "), ") with safe mode enabled"))
        out <- original_values
      }else {
        out <- original_values
        warning("Error005 during datamerge")
      }
      
      if(length(out) == 0){out<-NA}
      if(all(is.na(out))){
        out <- NA
      }else{
        out <- paste(out, collapse = delim)
      }
      
      return(out)
    })
    
    a <- c(a, table="joined")
    a
  })
  
  updated_fields <- as.data.frame(Reduce(rbind, l), stringsAsFactors = F)
  # updated_fields[[id]] <- unique(m[[id]])
  # updated_fields <- updated_fields[, n]
  
  main <- main[ order(main[[id]]), ]
  updated_fields <- updated_fields[ order(updated_fields[[id]]), ]
  
  main[main[[id]] %in% p, n] <- updated_fields
  # field_update_count
  main$table <- NULL
  
  
  return(main)
}

#' Collapse redundant rows of a df using the Simplify function 
#'
#' This function performs similar to aggregate.data.frame, but with several
#' conveniences. For simplicity it currently only allows grouping
#' by columns that exist in df by explicit column name. Collapse columns are
#' moved to the front of the df.
#'
#' @param df DataFrame containing column.names
#' @param column.names character vector of column names used for grouping rows. 
#'                     Performs a similar function as "by=" in aggreagte() 
#' @param unique Logical Should fields containing more then one unique value be 
#'                     allowed. It still returns a delimited list of multiple 
#'                     values but throws warning from Simplify() if enforced.
#' @return collapsed df
#' @examples 
#' df <- data.frame(
#'   Patient = c(1,   1,  2,  2,  3,  4),
#'   Age     = c(31, 31, 32, NA, 33, NA),
#'   Score   = c( 9, 10,  8,  8, "",  4))
#' CollapseDF(df, "Patient")
#'  #   Patient   Age   Score
#'  # 1       1    31   10; 9
#'  # 2       2    32       8
#'  # 3       3    33      
#'  # 4       4    NA       4
#' @export
CollapseDF <- function(df, column.names, unique = F){
  warning("Deprecated function, please use toolboxR::collapse_dt")
  # get a list of columns used for selecting
  columns <- lapply(column.names, function(x){df[[x]]})
  
  # aggregate the df with Simplify function
  df <- aggregate.data.frame(df, by = columns, Simplify, unique = unique)
  
  # Remove aggregate grouping columns
  df <- df[,!grepl("Group\\.[0-9]+$", names(df))]
  
  # Move the id columns to the front
  df <- toolboxR::move_columns(column.names, df)
  df
}

#' Collapse redundant rows of a df using the Simplify function 
#'
#' This function performs similar to aggregate.data.frame, but with several
#' conveniences. This version also improves on the previous CollapseDF by 
#' temporarily coercing into a data.table structure, making it handle Big Data
#' much better. For simplicity it currently only allows grouping
#' by columns that exist in df by explicit column name. Collapse columns are
#' moved to the front of the df.
#'
#' @param df DataFrame containing column.names
#' @param column.names character vector of column names used for grouping rows. 
#'                     Performs a similar function as "by=" in aggregate() 
#' @return collapsed data.table
#' @examples 
#' df <- data.frame(
#'   Patient = c(1,   1,  2,  2,  3,  4),
#'   Age     = c(31, 31, 32, NA, 33, NA),
#'   Score   = c( 9, 10,  8,  8, "",  4))
#' collapse_dt(df, "Patient")
#'  #   Patient   Age   Score
#'  # 1       1    31   10; 9
#'  # 2       2    32       8
#'  # 3       3    33      
#'  # 4       4    NA       4
#' @export
collapse_dt <- function(df, column.names, unique = F){
  
  dt   <- data.table::as.data.table(df)

  # suppress the coersion warning since it is expected
  # <simpleWarning in melt.data.table(dt, id.vars = "Sample_Name", na.rm = TRUE):
  # 'measure.vars' [File_Name, Patient, Study, Study_Phase, ...] are not all of
  # the same type. By order of hierarchy, the molten data value column will be of
  # type 'character'. All measure variables not of type 'character' will be coerced
  # to. Check DETAILS in ?melt.data.table for more on coercion.>
  suppressWarnings(long <- data.table::melt(dt, id.vars = column.names, na.rm = TRUE)
  )
  # filter to remove all NA, blank, or non-duplicated rows
  # remove sample-variable sets that are already unique
  already.unique <- long[(value != "NA"), `:=`(n=.N), by = c(column.names, "variable")][n==1, 1:3]
  duplicated     <- long[(value != "NA"), `:=`(n=.N), by = c(column.names, "variable")][n>1, 1:3]

  # summarize remaining fields to simplify
  dedup          <- duplicated[, .(value = toolboxR::Simplify(value)), by = c(column.names, "variable")]

  # join and spread
  long <- rbind(already.unique, dedup)
  wide <- data.table::dcast(long, get(column.names) ~ variable, value.var = "value")
  wide

}
