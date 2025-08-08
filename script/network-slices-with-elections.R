library(dplyr)
library(igraph)

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

  V(main_component_graph)$betweenness <- betweenness(main_component_graph)
  V(main_component_graph)$closeness <- closeness(main_component_graph)
  V(main_component_graph)$eigen_centrality <- unlist(unname(eigen_centrality(main_component_graph)[[1]]))

  centrality_df <- data.frame(
    Family = V(main_component_graph)$name,
    Betweenness = V(main_component_graph)$betweenness,
    Closeness = V(main_component_graph)$closeness,
    EigenCentrality = V(main_component_graph)$eigen_centrality
  )

  centrality_by_betweenness <- centrality_df %>%
    arrange(desc(Betweenness))
  centrality_by_closeness <- centrality_df %>%
    arrange(desc(Closeness))
  centrality_by_eigen <- centrality_df %>%
    arrange(desc(EigenCentrality))

  doge_family_betweenness_position <- which(centrality_by_betweenness$Family == casata)
  doge_family_closeness_position <- which(centrality_by_closeness$Family == casata)
  doge_family_eigen_centrality_position <- which(centrality_by_eigen$Family == casata)

  if (length(doge_family_betweenness_position) == 0) {
    doge_family_betweenness_position <- NA
  }
  if (length(doge_family_closeness_position) == 0) {
    doge_family_closeness_position <- NA
  }
  if (length(doge_family_eigen_centrality_position) == 0) {
    doge_family_eigen_centrality_position <- NA
  }

  results_df <- results_df %>%
    add_row(Year = election_year,
            Casata_doge = casata,
            Type = type,
            DogeFamilyBetweennessPosition = doge_family_betweenness_position,
            DogeFamilyClosenessPosition = doge_family_closeness_position,
            DogeFamilyEigenCentralityPosition = doge_family_eigen_centrality_position,
            TopFamilyBetweenness = centrality_by_betweenness$Family[1],
            TopFamilyCloseness = centrality_by_closeness$Family[1],
            TopFamilyEigenCentrality = centrality_by_eigen$Family[1])
}

