library(dplyr)
library(igraph)

devtools::load_all() 

load("data/venice-marriages-filtered.rda")
load("data/ducali_dogi_data.rda")

DEPTH_IN_YEARS <- 90

results_df <- data.frame( Year = integer(),
                          Casata_doge = character(),
                          Type = character(),
                          DogeFamilyBetweennessPosition = integer(),
                          DogeFamilyClosenessPosition = integer(),
                          DogeFamilyEigenCentralityPosition = integer(),
                          TopFamilyBetweenness = character(),
                          TopFamilyCloseness = character(),
                          TopFamilyEigenCentrality = character())

for (i in 1:nrow(ducali_dogi_data)) {
  election_year <- ducali_dogi_data$Year[i]
  casata <- ducali_dogi_data$Casata[i]
  type <- ducali_dogi_data$Type[i]

  marriage_graph_slice <- create_marriage_network(
    venice_marriages,
    election_year = election_year,
    years_before = DEPTH_IN_YEARS
  )
  
  # Extract and simplify the main component of the graph
  main_component_graph <- extract_main_component(marriage_graph_slice)
  
  # Analyze community structure
  community_analysis <- analyze_communities(
    graph = main_component_graph,
    total_families = families_in_graph
  )
  
  # Extract the proportion of families in the largest community
  families_in_first_community <- community_analysis$proportion_in_largest

}

