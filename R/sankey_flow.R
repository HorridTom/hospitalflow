#' plot_flow_diagram
#'
#' @param moves_table table of patient episode moves within and outside the hospital
#' @param ward_level boolean of whether the analysis is done ward by ward or by
#' hospital category (e.g. Mecical, Surgical, Acute etc.)
#' @param start the datetime from which the moves should be counted
#' @param end the datetime from which the moves should stop being counted
#' @param selected_levels a vector of strings stating the wards or categories
#' that should be included in the analysis
#' e.g. c("External Incoming", "Medical", "Surgical", "External Outgoing")
#'
#' @return spell table
#' @export
#'
#' @examples
plot_flow_diagram <- function(moves_table, ward_level = F,
                              start = NULL,
                              end = NULL,
                              selected_levels = NULL,
                              remove_static_moves = F
                              ){

  #set defualt start and end unless specified
  if(is.null(start)){
    start <- min(moves_table$move_datetime)
  }

  if(is.null(end)){
    end <- max(moves_table$move_datetime)
  }

  #filters moves table between start and end
  moves_table <- moves_table %>%
    dplyr::filter(move_datetime >= as.POSIXct(start) & move_datetime < as.POSIXct(end))

  #if ward level is TRUE, diagram will be made ward by ward, if false, it will be done my category
  if(ward_level == F){
    moves_table <- moves_table %>%
      dplyr::mutate(move_from = move_from_category, move_to = move_to_category)
  }

  #filters down based on selected wards or categories
  #selected levels should be a vetor of strings of ward names or categories
  if(!is.null(selected_levels)){
    moves_table <- moves_table %>%
      dplyr::filter(move_from %in% selected_levels & move_to %in% selected_levels)
  }

  if(remove_static_moves == T){
    moves_table <- moves_table %>%
      dplyr::filter(move_from != move_to)
  }

  # A connection data frame is a list of flows with intensity for each flow
  links <- dplyr::count(moves_table, move_from, move_to) %>%
    dplyr::rename(source = move_from, target = move_to, value = n)

  # a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source),
           as.character(links$target)) %>% unique()
  ) ####add group

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  #prepare colour scale
  #my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

  # Make the Network
  p <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight=FALSE,
                     fontSize = 12)
  p
}

