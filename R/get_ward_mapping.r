#function to get the broad specialism of a ward
get_ward_mapping <- function(ward_name, config_path){
  ward_type_mapping <- readRDS(file.path(config_path, "ward_type_mapping.rds"))
  getWardMapping <- ward_type_mapping$ward_broad_specialism
  names(getWardMapping) <- ward_type_mapping$ward
  unname(getWardMapping[ward_name])
}
