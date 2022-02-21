#' render_example_report
#'
#' @param title The title to use for the report.
#' @param author The author to be named on the report.
#' @param hospital_name The hospital name to use for the report and all graphs.
#' @param sample_weeks The number of weeks of sample data to use for the report.
#' @param sample_no_patients The number of patients in the sample data to use for the report.
#' @param import_list_path The directory containing the import list as an RDS file.
#' @param output_dir The directory where the report will be saved.
#' @return The system.time output for the time taken to render the report.
#' @export
#'
#' @examples
render_example_report <- function(title = "Example Hospital Flow Report",
                                  author = "CLAHRC NWL Information Intelligence Team",
                                  hospital_name = "Anytown General Hospital",
                                  sample_weeks = 20,
                                  sample_no_of_patients = NULL,
                                  import_list_path = "../example-config/example_import_list.rds",
                                  output_dir = "../reports") {

  system.time(rmarkdown::render(input = "vignettes/example-report.Rmd",
                                output_file = paste0("/example-report_",
                                                     gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-")),
                                                     "_", stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8),
                                                     ".pdf"),
                                output_dir = output_dir,
                                params = list(title = title,
                                              author = author,
                                              hospital_name = hospital_name,
                                              sample_weeks = sample_weeks,
                                              sample_no_of_patients = sample_no_of_patients,
                                              import_list_path = import_list_path)
  ))
}
