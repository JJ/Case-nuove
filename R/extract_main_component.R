#' Extract and Simplify the Main Component of a Graph
#' 
#' This function extracts the largest connected component from a graph and simplifies it
#' by removing multiple edges and combining their attributes.
#'
#' @param graph An igraph graph object
#' @param remove_multiple Logical, whether to remove multiple edges (default: TRUE)
#' @param edge_attr_comb Function or string specifying how to combine edge attributes
#'        when simplifying the graph (default: "sum")
#' 
#' @return A simplified igraph graph object containing only the main component
#' @importFrom igraph components V subgraph simplify
#' @export
extract_main_component <- function(graph, remove_multiple = TRUE, edge_attr_comb = "sum") {
  # Find connected components
  comp <- components(graph)
  
  # Get the largest component
  main_comp <- which.max(comp$csize)
  main_vertices <- V(graph)$name[comp$membership == main_comp]
  
  # Extract subgraph of the main component
  main_graph <- subgraph(graph, main_vertices)
  
  # Simplify the graph if requested
  if (remove_multiple) {
    main_graph <- simplify(main_graph, 
                          remove.multiple = TRUE, 
                          edge.attr.comb = edge_attr_comb)
  }
  
  return(main_graph)
}
