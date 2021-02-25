#' render_example_report
#'
#' @param sample_weeks The number of weeks of sample data to use for the report.
#' @param sample_no_of_patients The sample size of patients to be used. If this
#' value is not sepecified, the total number of patients in the data set will be used.
#' @param import_list_path The path to the import_list for the data that you want to
#' use for the report.
#' @param output_dir The directory in which the output report will be saved.
#' @return The system.time output for the time taken to render the report.
#' @export
#'
#' @examples
render_example_report <- function(sample_weeks = 4, sample_no_of_patients = 500,
                                  import_list_path = "../example-config/example_import_list.rds",
                                  output_dir = "vignettes/example-reports") {

  system.time(rmarkdown::render(input = "vignettes/example-report.Rmd",
                                output_format = "beamer_presentation",
                                output_file = paste0("example-report_",
                                                     gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-")),
                                                     "_", stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8),
                                                     ".pdf"),
                                output_dir = output_dir,
                                intermediates_dir = "vignettes/intermediates",
                                params = list(sample_weeks = sample_weeks, sample_no_of_patients = sample_no_of_patients,
                                              import_list_path = import_list_path)
  ))
}
