#' Simulated moves table corresponding to ed_data and inpatient_data.
#'
#' @description
#' A dataset containing the output of make_moves_table() when applied to ed_data and inpatient_data.
#'
#' @format A data frame with 8 variables. Each observation corresponds to a single move of a patient from
#' a ward, ED, or outside of the hospital to the next destination in their spell.  If documented as a separate
#' episode in the spell table, it is possible that the move will be to the same ward (described as a "static move").
#' Each episode from the spell table will have two corresponding moves associated with it (a move in and a move out).
#' \describe{
#'   \item{spell_number}{The spell number for the spell that the move is part of.
#'   The corresponding spell can be found in the spell_table with the same spell_number.}
#'   \item{pseudo_id}{Unique ID number of the patient. Original mapping back to
#'   hospital number should be destroyed or adequately protected according to
#'   all relevant data governance legislation and protocols}
#'   \item{move_from}{The location within the spell that the patient has moved from.
#'   E.g. "Ward 3", "ED", "External Incoming".}
#'   \item{move_to}{The location within the spell that the patient has moved to.
#'   E.g. "Ward 3", "ED", "External Outgoing".}
#'   \item{move_datetime}{The datetime at which the move occured.}
#'   \item{move_number}{Primary key of the moves table. Each move has a distinct move_number.}
#'   \item{move_from_category}{The category under which the move_from location falls into.
#'   E.g. Medical, Surgical, External}
#'   \item{move_to_category}{The category under which the move_to location falls into.
#'   E.g. Medical, Surgical, External}
#' }
"moves_table"
