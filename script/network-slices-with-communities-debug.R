library(dplyr)
library(igraph)

options(show.error.locations = TRUE)

load("data/venice-marriages-filtered.rda")
load("data/ducali_dogi_data.rda")

DEPTH_IN_YEARS <- 90

communities_results_df <- data.frame( Year = integer(),
                          Casata_doge = character(),
                          Type = character(),
                          FamiliesInNetwork = integer(),
                          FamiliesinMainComponent = numeric(),
                          FamiliesInFirstCommunity = numeric(),
                          DogeCommunityRank = numeric());

for (index in 1:nrow(ducali_dogi_data))  {
  cat("Processing index:", index, "\n")
  election_year <- ducali_dogi_data$Year[index]
  casata <- ducali_dogi_data$Casata[index]
  type <- ducali_dogi_data$Type[index]

  marriages_before_ducale <- venice_marriages %>%
    filter(year <= election_year & year >= election_year - DEPTH_IN_YEARS)

  cat("index1", index, "of", nrow(ducali_dogi_data), "\n")
  marriage_graph_before_first_ducale <- graph_from_data_frame(
    marriages_before_ducale,
    directed = FALSE,
    vertices = unique(c(marriages_before_ducale$husband_familyname_std, marriages_before_ducale$wife_familyname_std))
  )
  families_in_graph <- length(V(marriage_graph_before_first_ducale)$name)

  cat("index2", index, "of", nrow(ducali_dogi_data), "\n")

  main_component <- components(marriage_graph_before_first_ducale)$membership
  main_component_graph <- subgraph(marriage_graph_before_first_ducale, V(marriage_graph_before_first_ducale)$name[main_component == 1])

  families_in_main_component <- length(V(main_component_graph)$name)/ families_in_graph

  main_component_graph <- simplify(main_component_graph, remove.multiple = TRUE, edge.attr.comb = "sum")

  cat("index3", index, "of", nrow(ducali_dogi_data), "\n")

  communities <- cluster_edge_betweenness(main_component_graph, weights = E(main_component_graph)$weight)

  community_membership <- membership(communities)
  community_sizes <- table(community_membership)
  sorted_communities <- sort(community_sizes, decreasing = TRUE)

  families_in_first_community <- sorted_communities[1] / families_in_graph

  doge_community <- community_membership[casata]
  doge_community_rank <- 0
  if (!is.na(doge_community)) {
    cat(sorted_communities, "\n")
    doge_community_rank <- which(names(sorted_communities) == doge_community)
  } else {
    cat("Doge family", casata, "not found in communities for year", election_year, "\n")
  }
  communities_results_df <- rbind(communities_results_df,
                                data.frame(Year = election_year,
                                           Casata_doge = casata,
                                           Type = type,
                                           FamiliesInNetwork = families_in_graph,
                                           FamiliesinMainComponent = families_in_main_component,
                                           FamiliesInFirstCommunity = families_in_first_community,
                                           DogeCommunityRank = doge_community_rank))

}
save(communities_results_df, file = "data/communities_results_df.rda")
library(ggplot2)
communities_results_df$DogeCommunityRank <- as.factor(communities_results_df$DogeCommunityRank)
ggplot(communities_results_df, aes(x = Year, y = FamiliesInFirstCommunity)) +
  geom_line(color = "blue") +
  geom_point(aes(color = Type,shape=DogeCommunityRank), size = 3) +
  labs(title = "Families in Network Over Time",
       x = "Year",
       y = "Families in Network") +
  theme_minimal() +
  scale_color_manual(values = c("Ducali"="gold", "Nuovissime" = "blue", "Nuove" = "green", "Apostoliche" = "red", "Evangeliche" = "black")) +
  theme(legend.title = element_blank())

ggsave("figures/communities_over_time.png", width = 10, height = 6)
