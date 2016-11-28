#' Check for explicit value in df
#'
#' A helper function to build df-specific search tools.
#' These functions are typically used in conjunction with lapply to
#' determine if they are present in a df.
#'
#' Important: since both the child function accepts multiple arguments (...) when
#'            it is built with multiple ids, you must explicitly refer to additional
#'            variables my name (dat, field, value). Without them the function
#'            returns NA even with valid queries.
#'
#' @param X Search term
#' @param dat a lookup table data frame with both "id" and "field" columns
#' @param field the search column name
#' @param value the value to match
#' @param exclude return the inverse logical value.
#' @param unique_match If the combination of specified search terms return multiple
#'                    fields this function will return TRUE by default if ANY match
#'                    the supplied value. Specify unique_match = TRUE to require a
#'                    single field to be identified before comparison.
#' @return logical
#' @export
#' @examples
#'   df <- data.frame(Patient.ID = c("p1", "p1", "p2", "p2", "p3"),
#'                    Visit      = c(1,     2,    1,    2,    NA),
#'                    Test.Result= c("neg","pos","neg", NA,   "pos"),
#'                    stringsAsFactors = F)
#'
#'   check_value_by_patient_id <- check.value("Patient.ID")
#'   check_value_by_patient_id("p1", dat = df, field = "Test.Result", value = "pos")
#'   # TRUE
#'   check_value_by_patient_id("p1", df, "Test.Result", "pos")
#'   # returns NA inappropriately
#'   check_value_by_patient_id("p2", dat = df, field = "Test.Result", value = "pos")
#'   # FALSE
#'
#'   check_by_id_and_visit <- check.value(c("Patient.ID", "Visit"))
#'   check_by_id_and_visit("p2", 1, dat = df, field = "Test.Result", value = "pos")
#'   # FALSE
#'   check_by_id_and_visit("p3", NA, dat = df, field = "Test.Result", value = "pos")
#'   # NA
check.value <- function(ids){
  function(..., dat, field, value, fixed_pattern = F, unique_match = T){
    tryCatch({
      query <- c(...)
      # check arguments
      if( !all(ids %in% names(dat)) ){warning("Specified filter columns do not exist in lookup table")}
      if( !is.data.frame(dat) ){warning("Lookup table not supplied")}
      if( !field %in% names(dat) ){warning("Specified *field* does not exist in lookup table")}
      if( value == ""){warning("Lookup *value* not supplied")}

      #if blank or NA filter terms are supplied return NA
      if(any( c(NA, "") %in% query)) {return(NA)}

      # create logical lists for each dat[[id]]==X selection set and combine
      opts <- mapply(ids, query, FUN = function(i,x){ dat[[i]] == x }, SIMPLIFY = F)
      selector <-  Reduce("&", opts)
      # check for unique field match
      if(unique_match & sum(selector) > 1){
        warning(paste0("Multiple rows were matched for ",query," with unique_match required "))}

      return(any(grepl( value, dat[[field]][selector], ignore.case = T, fixed = fixed_pattern)))
    },error=function(e){NA})
  }
}



#######################################################
#' Generate function to fetch the value from specified row/column
#'
#' A helper function to build df-specific search tools.
#' These functions are typically used in conjunction with lapply to
#' determine if they are present in a df.
#'
#' @param id Used during function creation to customize which df columns
#'            are used for search. This can be a vector.
#' @param X Search term, this can be a vector
#' @param dat a data frame with both "id" and "field" columns
#' @param field the search column name
#' @return value from field corresponding to search terms. This function
#'         can select multiple field values that are collapsed with
#'         separator to conserve dimensionality
#' @export
#' @examples
#'   df <- data.frame(Patient.ID = c("p1", "p1", "p2", "p2", "p3"),
#'                    Visit      = c(1,     2,    1,    2,    NA),
#'                    Test.Result= c("neg","pos","neg", NA,   "pos"),
#'                    stringsAsFactors = F)
#'    lookup_by_patientid <- lookup.values("Patient.ID")
#'    lookup_by_patientid("patient2", df, "Test.Result")
#'    # "negative"
#'    lookup_by_patientid("patient1", df, "Test.Result")
#'    # "negative; positive"
#'
#'    lookup_by_id_and_visit <- lookup.values(id = c("Patient.ID", "Visit"))
#'    lookup_by_id_and_visit(X = c("patient1", 2), dat = df, field = "Test.Result")
#'    # "positive"
#'
#'    # get results for all patients on their first visit
#'    p <- strsplit(paste(unique(df$Patient.ID),1, sep = ";"), split = ";")
#'    unlist(lapply(p, lookup_by_id_and_visit, dat = df, field = "Test.Result"))
#'    # "negative" "negative"
lookup.values <- function(id) {
  function(..., dat, field, separator = "; ") {
    tryCatch({

      #if blank or NA search terms are supplied return NA
      if(any( c(NA, "") %in% c(...))) {return(NA)}

      # create logical lists for each dat[[id]]==X selection set
      opts <- mapply(id, c(...), FUN = function(i,x){ dat[[i]] == x }, SIMPLIFY = F)
      # reduce multiple lists to a single logical selector
      selector <-  Reduce("&", opts)

      # this function does have the potential to select multiple fields that
      #  must be merged before return to conserve proper dimensionality
      foo <- dat[[field]][selector]
      # return NA if that's all there is
      if(all(is.null(foo))){return(NA)}
      if(all(is.na(foo))){return(NA)}
      # remove blank elements
      foo <- foo[foo != ""]
      # reduce to case-insensitive unique elements, only capitalize if there are case differences
      if(!identical(toupper(unique(foo)),unique(toupper(foo)))){ foo <- toupper(foo)}
      paste(unique(na.exclude(foo)), collapse = separator)
    },error=function(e){NA})
  }
}


#######################################################
#' Generate function to set the value of a specified row/column
#'
#' A helper function to build df-specific search tools.
#' These functions are typically used in conjunction with lapply to
#' determine if they are present in a df.
#'
#' @param id Used during function creation to customize which df columns
#'            are used for search. This can be a vector.
#' @param X Search term, this can be a vector
#' @param dat a data frame with both "id" and "field" columns
#' @param field the search column name
#' @return value from field corresponding to search terms. This function
#'         can select multiple field values that are collapsed with
#'         separator to conserve dimensionality
#' @export
set.value <- function(id) {
  function(..., dat, field, value) {
    tryCatch({

      #if blank or NA search terms are supplied return NA
      if(any( c(NA, "") %in% c(...))) {return(NA)}

      # create a boolean row selection vector
      # create logical lists for each dat[[id]]==X selection set
      opts <- mapply(id, c(...), FUN = function(i,x){ dat[[i]] == x }, SIMPLIFY = F)
      # reduce multiple lists to a single logical selector
      selector <-  Reduce("&", opts)

      # this function does have the potential to select multiple fields that
      #  must be merged before return to conserve proper dimensionality
      dat[[field]][selector] <- value
      return(dat)
    },error=function(e){NA})
  }
}


