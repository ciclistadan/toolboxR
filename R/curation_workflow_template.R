# basic data curation workflow

# usage notes and description of workflow, exceptions, outputs

# package import and tool sourcing

# global variable definition

# raw data import
  # define NA values before import

# generic cleaning and tidying functions
  # trim leading/trailing whitespace from all values
  # confirm all variables imported as correct types, revise import above or coerce as necessary
  # reshape or join tables as necessary
  # if desired this is when you’d globally format column names

# manual fixes a needed.
  # use this code space to coerce individual strange or exceptional values into values able to be parsed by a generic function.
  # generic function application
  # recoding variables; dplyr::recode()
  # conditional coercion; dplyr::if_else() or dplyr::case_when()
  # rename variables
    # df %>% mutate(new = old) is preferred to df$new <- df$old
  # select only desired output variables

# QC checks
  # table() to confirm expected values and their corresponding frequencies
  # check for NAs as expected
  # plot ggplot2::geom_freqpoly() of continuous numeric values to identify outliers/miscodings.
  # Similar check of expected ranges for dates.
  # ensure all identifiers are correctly present; in this examples check that all measurements have a subject and sample in the corresponding tables. This means you’ll need to generate the curated subject/samples tables first and then read them in again for this qc.

# export tables
  # by default we prefer tab-separated values with headers but not row.names. Always view the exported file in it’s most “raw” form after export, e.g. open a with notepad instead of Excel.

