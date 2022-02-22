#' render_example_report
#'
#' @param title The title to use for the report.
#' @param author The author to be named on the report.
#' @param hospital_name The hospital name to use for the report and all graphs.
#' @param use_example_data If TRUE, use the package example data.
#' @param sample_weeks The number of weeks of sample data to use for the report.
#' @param sample_no_patients The number of patients in the sample data to use
#' for the report.
#' @param import_list_path The directory containing the import list as an RDS
#' file. Ignored if use_example_data is TRUE.
#' @param output_dir The directory where the report will be saved.
#' @param use_existing_spell_table If TRUE, skip import_standardise_bind and
#' just load a spell table and a moves table directly. If use_example_data is also
#' TRUE, uses the example spell table provided in the package.
#' @param existing_spell_table_path path to existing spell table. Ignored if
#' use_existing_spell_table is FALSE or use_example_data is TRUE.
#' @param existing_moves_table_path path to existing moves table. Ignored if
#' use_existing_spell_table is FALSE or use_example_data is TRUE.
#' @return The system.time output for the time taken to render the report.
#' @export
#'
#' @examples
#' \dontrun{
#' render_example_report(output_dir = "../reports", use_existing_spell_table = FALSE)
#' render_example_report(output_dir = "../reports", use_existing_spell_table = TRUE)
#' }
render_example_report <- function(title = "Example Hospital Flow Report",
                                  author = "CLAHRC NWL Information Intelligence Team",
                                  hospital_name = "Anytown General Hospital",
                                  use_example_data = TRUE,
                                  sample_weeks = 20,
                                  sample_no_of_patients = NULL,
                                  import_list_path = system.file("extdata",
                                                                 "example-config",
                                                                 "example_import_list.rds",
                                                                 package = "hospitalflow"),
                                  output_dir,
                                  use_existing_spell_table = TRUE,
                                  existing_spell_table_path = NULL,
                                  existing_moves_table_path = NULL) {

  if(missing(output_dir)) {
    stop("output_dir argument missing, you must specify where to save the output",
         call. = FALSE)
  }

  system.time(rmarkdown::render(input = "vignettes/example-report.Rmd",
                                output_file = paste0("/example-report_",
                                                     gsub(":","-",paste(strsplit(x = toString(Sys.time()),split = " ")[[1]], collapse="-")),
                                                     "_", stringr::str_sub(system("git rev-parse HEAD", intern=TRUE), 1, 8),
                                                     ".html"),
                                output_dir = output_dir,
                                params = list(title = title,
                                              author = author,
                                              hospital_name = hospital_name,
                                              use_example_data = use_example_data,
                                              sample_weeks = sample_weeks,
                                              sample_no_of_patients = sample_no_of_patients,
                                              import_list_path = import_list_path,
                                              use_existing_spell_table = use_existing_spell_table,
                                              existing_spell_table_path = existing_spell_table_path,
                                              existing_moves_table_path = existing_moves_table_path)
  ))
}
