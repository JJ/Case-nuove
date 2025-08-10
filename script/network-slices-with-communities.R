library(dplyr)
library(igraph)

load("data/venice-marriages-filtered.rda")
load("data/ducali_dogi_data.rda")

DEPTH_IN_YEARS <- 90

results_df <- data.frame( Year = integer(),
                          Casata_doge = character(),
                          Type = character(),
                          DogeFamilyCommunityRank = integer(),
                          DogeFamilyDucaliPercentage = numeric(),
                          DogeFamilyLunghiPercentage = numeric() );

for (i in 1:nrow(ducali_dogi_data)) {
  election_year <- ducali_dogi_data$Year[i]
  casata <- ducali_dogi_data$Casata[i]
  type <- ducali_dogi_data$Type[i]



  marriages_before_ducale <- venice_marriages %>%
    filter(year <= election_year & year >= election_year - DEPTH_IN_YEARS)

  marriage_graph_before_first_ducale <- graph_from_data_frame(
    marriages_before_ducale,
    directed = FALSE,
    vertices = unique(c(marriages_before_ducale$husband_familyname_std, marriages_before_ducale$wife_familyname_std))
  )

  main_component <- components(marriage_graph_before_first_ducale)$membership
  main_component_graph <- subgraph(marriage_graph_before_first_ducale, V(marriage_graph_before_first_ducale)$name[main_component == 1])

  main_component_graph <- simplify(main_component_graph, remove.multiple = TRUE, edge.attr.comb = "sum")

  communities <- cluster_edge_betweenness(main_component_graph, weights = E(main_component_graph)$weight)

  community_membership <- membership(communities)
  community_sizes <- table(community_membership)
  sorted_communities <- sort(community_sizes, decreasing = TRUE)
  for (community in names(sorted_communities)) {
    cat("Community", community, "has", sorted_communities[community], "members:\n")
    members <- V(main_component_graph)$name[community_membership == as.numeric(community)]
    cat(paste(members, collapse = ", "), "\n\n")
  }

}
