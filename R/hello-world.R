
#' hello_world
#'
#' @return The string "Hello world!"
#' @export
#'
#' @examples
#' hello_world()
hello_world <- function() {
  dplyr::as.tbl(as.data.frame("Hello world! It's so hot!!"))
}
