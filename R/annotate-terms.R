#' Gene and Protein Synonyms Table from ExPASy
#'
#' Fetch a synonym table from the GPSDB synonym tool this tool returns synonyms
#' from EntrezGene, HGNC, OMIM and SwissProt(UniProt) databases if your search
#' term matched more than one geneset, it will be reflected in the "geneset" values
#'
#' @param term The term you'd like to search for.
#' @param acc The corresponding UniProt accession number, used for filtering when "term" returns multiple results
#' @param unique_only Default = TRUE, otherwise you can return multiple results.
#' @export
#' @examples
#' gpsdb_table("npc1")

gpsdb_table <- function(term, acc = "", unique_only = TRUE) {
  out <- tryCatch(
    {
      root   <-  "http://gpsdb.expasy.org/cgi-bin/gpsdb/show?name="
      term <- URLencode(term)
      filter <- "&model=Homo%20sapiens&format=txt"
      query <-paste(root,term,filter,sep = "")

      df <- read.table(query, header = FALSE, sep="\t", fill=T, quote = "",
                       col.names = c("term", "geneset", "db", "dbid", "q1","q2","q3","species_name","nom","phylo","lower"))
      # remove date line from end of table
      df <- head(df,-1)

      # more than one geneset returned,
      if( (length(unique(df$geneset)) > 1)){
        #filter by acc if supplied
        if ( nchar(acc) > 0 ){
          geneset <- df[df$dbid == acc, "geneset"][[1]]
          df <- df[df$geneset == geneset,]
          return(df)

        } else if ( unique_only == FALSE){
          return(df)

        } else {
          stop(paste("Multiple matches returned for", term, "and request specified unique_only","\n"))
        }
      }
      #unique match with just name
      else if( length(unique(df$geneset)) == 1){
        return(df)
      }
      # catchall for failed query
      else{
        stop(paste("No matches returned for", term, "\n"))
      }
    },
    error=function(cond) {
      message(cond)
      return(NA)
    }
  )
  return(out)
}


# simple function to launch GPSDB search in local browser
launch_browser <- function(term){
  root   <-  "http://gpsdb.expasy.org/cgi-bin/gpsdb/show?name="
  term <- term
  filter <- "&model=Homo%20sapiens"
  query <-paste(root,term,filter,sep = "")
  browseURL(query)
}

extract_db_keys <- function(df,db,colname){
  e<-as.character(unique(df[df$db==db,colname]))
  return(paste(e, sep="", collapse = "; "))
}


uniprotID_from_acc <- function(uniprot_acc){
  out <- tryCatch(
    {doc <- XML::xmlTreeParse(paste(
      "http://www.uniprot.org/uniprot/",
      uniprot_acc,".xml", sep = ""),
      isURL = T)
    r <- XML::xmlRoot(doc)
    XML::xmlValue(r[[1]]["name"][[1]])
    },
    error=function(cond) {
      #       message(cond)
      return(NA)
    }
  )
  return(out)
}


# get_xml <- function(uniprot_acc){
#   doc <- xmlParse(paste(
#     "http://www.uniprot.org/uniprot/",
#     uniprot_acc,".xml", sep = ""),
#     isURL = T)
#   saveXML(doc, file = "tmp.xml")
# }


name_from_UniProtAcc <- function(uniprot_acc){
  out <- tryCatch(
    {
      doc <- XML::xmlParse(paste(
        "http://www.uniprot.org/uniprot/",
        uniprot_acc,".xml", sep = ""),
        isURL = T)
      #so far these two nodes work for everything, but it has not been extensively tested.
      gene_name_node <- XML::getNodeSet(doc, "//x:gene/x:name", "x")
      protin_name_node <- XML::getNodeSet(doc, "//x:protein/x:recommendedName/x:fullName", "x")

      if ( XML::xmlSize(gene_name_node) > 0 ){
        XML::xmlValue(gene_name_node[[1]])
      } else if (XML::xmlSize(protin_name_node) > 0){
        XML::xmlValue(protin_name_node[[1]])
      }

    },
    error=function(cond) {
      #       message(cond)
      return(NA)
    }
  )
  return(out)
}




###########################
## Entrez eDirect Utilities
##

## needs to be refined to perform simlar to gene_lookup()
# protein_lookup <- function(term) {
#   query <- paste(term, '[PROT] AND Homo sapiens[ORGN]', sep="")
#   uid   <- esearch(query, db = "protein", retmax = 1, )
#   if(as.integer(uid$xmlValue("/eSearchResult/Count"))){
#     xml   <- efetch(uid, db="protein", retmode="xml", rettype="gp")
#     desc  <- xml$xmlValue("/GBSet/GBSeq/GBSeq_definition")
#     return(desc)
#   }
#   else{return("NA")}}

#' Entrez text search with filterability
#'
#' General purpose function which takes a disorganized list and
#' returns a sorted unique list of values.
#'
#' @param term vector search string
#' @param field which field type should the search terms query (https://www.ncbi.nlm.nih.gov/books/NBK49540/)
#' @param db Database to search (gene, protein, etc)
#' @param organism filter for a specific organism
#' @return integer vector of entrez ids
#' @export
#' @examples
#'  entrez_id("hsa-let-7a-1")
#'  entrez_id(c("hsa-let-7a-1", "tp53"))
entrez_id <- function(term, field = "All Fields", db = "gene", organism = "Homo sapiens", retmax = 1){

  sapply(term, function(x){
    # construct search query
    query <- paste(x, "[",field,"]", sep = " ")
    if(nchar(organism) > 0) query <- paste(query, "AND", organism, "[ORGN]", sep = " ")
    uid   <-  reutils::esearch(query, db = db, retmax = retmax, sort="relevance")
    return(as.integer(uid$xmlValue("/eSearchResult/IdList/Id")))
  })
}


##########
## general string manipulation functions
##  need to wrap in functions before uncommenting


# from a df with fields of delimited strings, aggregates all unique values
# and returns a single delimmited string
list_from_df <-function(df, delim = "; "){

  # collapse rows
  rows <- apply(df, MARGIN=1, function(row){
    paste(row[!(is.na(row))], collapse = delim)
  })

  #collapse columns
  all <- paste(rows, collapse = delim)

  #
  all <- unlist(strsplit(all, split = delim))
  unique <- sort(unique(tolower(all)))
  single_string <- paste(unique, collapse = delim)
  return(single_string)
}



####################
# to merge lists manually
# str1 <- "HSP90AA2P; HSP90AA2; HSP90ALPHA; HSPCA; HSPCAL3; heat shock 90kD protein, alpha-like 3; heat shock 90kDa protein 1, alpha-like 3; heat shock protein 86; heat shock protein 90kDa alpha (cytosolic), class A member 1 pseudogene; heat shock protein 90kDa alpha (cytosolic), class A member 2, pseudogene"
# str2 <- "heat shock 90kDa protein 1, alpha-like 3; HSP90ALPHA; HSP90AA2; HSPCA; HSPCAL3; heat shock 90kD protein 1, alpha-like 3; heat shock protein 90kDa alpha (cytosolic), class A member 2"
# str3 <- "HEAT-SHOCK PROTEIN, 90-KD, ALPHA, CLASS A, MEMBER 2; HSP90AA2; HEAT-SHOCK 90-KD PROTEIN 1, ALPHA-LIKE 3; HSPCAL3"
# str4 <- "HSP90AA2; HSPCAL3; Putative heat shock protein HSP 90-alpha A2; Heat shock 90 kDa protein 1 alpha-like 3"
# foo<-list(str1,str2,str3,str4)
# foo_split <- sapply(foo, strsplit, split="; ")
# merge_redundant_lists(unlist(foo_split))

# row <- hbv[30,]
# this is meant to be called with *apply* on a df
all_synonyms <- function(row, cols){

  #select fields with synonyms
  c <- grep("*names", names(row))
  r <-  row[c]

  #remove na fields
  r <- r[!(is.na(r))]

  #merge into a single delim string
  all <- merge_redundant_lists(r)
  return(all)
}
#' Merge and Reduce List of Lists
#'
#' General purpose function which takes a disorganized list and
#' returns a sorted unique list of values.
#'
#' @param l A list, data frame or vector. Lists and dataframes can be composed of
#'         mixtures of any other data type. Currently data frame columns encoded
#'         as factors are converted to their base structure (e.g. c("factor1", "factor2") -> c(1,2)).
#'         This can be fixed by using stringsAsFactors = TRUE for dataframe creation.
#' @param input_delim Delimiter use to split combined strings
#'        into distinct values (e.g. "one; two; three" -> "one", "two", "three")
#' @param output_delim Delimiter used to separate values in a single output string.
#'        Default blank value returns a list.
#' @param case_sensitive Boolean indicating whether character case should be considered
#'        during comparison (e.g. case_sensitive = TRUE means "abc" != "ABC"). If the default
#'        FALSE value is used, all returned values are returned as lower case.
#' @export
#' @examples
#' l <- data.frame( a = c("one", "two"),
#'                  b = c(1,1),
#'                  c = c("two", "three"),
#'                  d = c(14, "zzz"),
#'                  stringsAsFactors = FALSE)
#'  merge_redundant_lists(l)
#'  merge_redundant_lists(l, output_delim = "; ")
#'
#'  l<-list( c("before"),
#'           17,
#'           "R is cool",
#'           "A;B;C;D",
#'           "a;b;c;d",
#'           c(1,1,1))
#'  merge_redundant_lists(l, output_delim = ", ")
merge_redundant_lists <- function(l, input_delim = ";", output_delim = "", case_sensitive = FALSE){
  # TODO: split individual "pre-grouped" list items before unique sorting
  l <- unlist(l)
  if(!(case_sensitive)){
  l <- tolower(l)}
  # remove empty fields
  l <- l[l!=""]
  l <- paste(l, collapse = input_delim)
  l <- unlist(strsplit(l, split = input_delim))
  # chomp
  l <- sapply(l, FUN = function(x){x=gsub("^\\s", "", x); x=gsub("\\s$", "", x); return(x)})
  l <- unique(l)
  l <- sort(l)
  if(output_delim != ""){
  l <- paste(l, collapse = output_delim)}
  return(l)
}
