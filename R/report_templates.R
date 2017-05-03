#' Custom PDF format for Rancho Project Reporting
#'
#' @return a pdf report generated from pandoc
#' @export
rancho_project_report <- function(){

  # get the locations of resource files located within the package
  template    <- system.file("extdata/rancho_project_report.latex", package = "toolboxR")

  # call the base html_document function
  rmarkdown::pdf_document(
    fig_width  = 6.5,
    fig_height = 4,
    template   = "/home/parallels/R/x86_64-pc-linux-gnu-library/3.3/toolboxR/extdata/rancho_project_report.latex")
}