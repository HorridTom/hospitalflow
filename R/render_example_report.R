#' render_example_report
#'
#' @param sample_weeks The number of weeks of sample data to use for the report.
#' @return The system.time output for the time taken to render the report.
#' @export
#'
#' @examples
render_example_report <- function(sample_weeks = 4,
                                  sample_no_of_patients = NA_integer_) {

  system.time(rmarkdown::render(input = "vignettes/example-report.Rmd",
                                output_format = "beamer_presentation",
                                output_file = paste0("example-report_",
                                                     gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-")),
                                                     "_", stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8),
                                                     ".pdf"),
                                params = list(sample_weeks = sample_weeks,
                                              sample_no_of_patients = sample_no_of_patients)
  ))
}
