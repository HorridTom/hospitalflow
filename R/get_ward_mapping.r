#' get_ward_mapping
#'
#' @param ward_name Exact ward name to use to look up corresponding specialism
#' @param config_path Path to config file with ward mapping
#'
#' @return The broad specialism corresponding to the input ward
#' @export
#'
#' @examples
get_ward_mapping <- function(ward_name, config_path = "lgt-config/inpatient/"){
  ward_type_mapping <- readRDS(file.path(config_path, "ward_type_mapping.rds"))
  getWardMapping <- ward_type_mapping$ward_broad_specialism
  names(getWardMapping) <- ward_type_mapping$ward
  unname(getWardMapping[ward_name])
}
