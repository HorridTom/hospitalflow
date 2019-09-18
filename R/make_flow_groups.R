#' make_flow_groups
#'
#' @param spell_data Hospital flow data with columns for
#'  ed non-admission (boolean);
#'  directorate (e.g. "Medical", "Surgical")
#'
#' @return spell_data with additional column for flow group
#' @export
#'
#' @examples
make_flow_groups <- function(spell_data){

  spell_data <- spell_data %>%
    dplyr::mutate(flow_groups = dplyr::case_when(ed_non_adm == TRUE  ~ "Flow A",
                                                 directorate == "Medical" ~ "Flow 3",
                                                 directorate == "Surgical" ~ "Flow 4"))
  spell_data

}
