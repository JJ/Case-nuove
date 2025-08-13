#' Analyze Community Structure in a Graph
#' 
#' This function performs community detection using edge betweenness and calculates
#' the proportion of families in the largest community.
#'
#' @param graph An igraph graph object
#' @param total_families Total number of families in the graph (for proportion calculation)
#' 
#' @return A list containing:
#'   - communities: The community object from cluster_edge_betweenness
#'   - membership: Vector of community memberships
#'   - community_sizes: Table of community sizes
#'   - sorted_communities: Community sizes sorted in descending order
#'   - proportion_in_largest: Proportion of families in the largest community
#' 
#' @importFrom igraph cluster_edge_betweenness E membership
#' @export
analyze_communities <- function(graph, total_families) {
  # Detect communities using edge betweenness
  communities <- cluster_edge_betweenness(graph, weights = E(graph)$weight)
  
  # Get community memberships and sizes
  community_membership <- membership(communities)
  community_sizes <- table(community_membership)
  sorted_communities <- sort(community_sizes, decreasing = TRUE)
  
  # Calculate proportion of families in the largest community
  proportion_in_largest <- sorted_communities[1] / total_families
  
  # Return all results in a list
  list(
    communities = communities,
    membership = community_membership,
    community_sizes = community_sizes,
    sorted_communities = sorted_communities,
    proportion_in_largest = proportion_in_largest
  )
}
