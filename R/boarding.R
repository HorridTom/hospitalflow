

#' get_contingency_table
#'
#' @param spell_table
#'
#' @return
#' @export
#'
#' @examples
get_contigency_table <- function(spell_table){
  # Perform check if the necessary column exist
  # Include parameter "between". Use should input one of "PC", "PW", "cw"
  tbl <- as.data.frame.matrix(
    table(
      spell_table$ward_category, spell_table$main_specialty
      )
    )
  return (tbl)
}
