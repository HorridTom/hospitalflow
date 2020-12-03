library(networkD3)
library(dplyr)

plot_flow_diagram <- function(moves_table, ward_level = F, #selected_levels = NULL
                              selected_levels = c("External Incoming", "Medical", "Surgical", "External Outgoing")){

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

  # A connection data frame is a list of flows with intensity for each flow
  links <- dplyr::count(moves_table, move_from, move_to) %>%
    dplyr::rename(source = move_from, target = move_to, value = n)

  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source),
           as.character(links$target)) %>% unique()
  )

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name",
                     sinksRight=FALSE)
  p
}

