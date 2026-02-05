library(igraph)
library(dplyr)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)
load("data/doges.years.rda")

DEPTH_IN_YEARS <- 75

lunghi <- family_labels[ family_labels$Group == "Lunghi", ]$Family
ducali <- family_labels[ family_labels$Ducale == 1,]$Family
quaranta_famiglie <- c(lunghi,ducali)

quaranta_famiglie <- quaranta_famiglie[ !(quaranta_famiglie %in% c("Steno", "Da Ponte", "Cicogna")) ]

window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1660, by = 5)
assortativity <- data.frame( curti_lunghi = numeric(),
                             ducali = numeric(),
                             in_quaranta = numeric(),
                             distance_curti_lunghi = numeric(),
                             distance_ducali_corti = numeric(),
                             distance_ducali_lunghi = numeric(),
                             intra_lunghi = numeric(),
                             intra_ducali = numeric(),
                             intra_corti = numeric(),
                             intra_quaranta = numeric(),
                             intra_non_quaranta = numeric(),
                             quaranta_e_non_quaranta = numeric(),
                             year = integer() )

for (y in window_sequence ) {

  marriages_window <- noble_marriages_filtered %>%
    filter(year <= y + DEPTH_IN_YEARS & year >= y)

  marriage_graph <- graph_from_data_frame(
    marriages_window,
    directed = FALSE,
    vertices = unique(c(marriages_window$husband_familyname_std, marriages_window$wife_familyname_std))
  )
  
  doges <- doges.years[ doges.years$Start >= y & doges.years$Start <= y + DEPTH_IN_YEARS, ]$Family.doge
  E(marriage_graph)$weight <- 1
  marriage_graph <- simplify(marriage_graph, edge.attr.comb = "sum")
  
  V(marriage_graph)$Faction <- ifelse(V(marriage_graph)$name %in% lunghi, "Lunghi", "Corti")
  V(marriage_graph)$types_curti_lunghi <- ifelse(V(marriage_graph)$Faction == "Lunghi", 1,2)
  assortativity_curti_lunghi <- assortativity_nominal(marriage_graph, V(marriage_graph)$types_curti_lunghi, directed = FALSE)
  
  V(marriage_graph)$Ducale <- ifelse(V(marriage_graph)$name %in% ducali, TRUE, FALSE)
  V(marriage_graph)$types_ducali <- ifelse(V(marriage_graph)$Ducale == TRUE, 1,2)
  assortativity_ducali <- assortativity_nominal(marriage_graph, V(marriage_graph)$types_ducali, directed = FALSE)
  
  V(marriage_graph)$InQuaranta <- ifelse(V(marriage_graph)$name %in% quaranta_famiglie, TRUE, FALSE)
  V(marriage_graph)$types_quaranta <- ifelse(V(marriage_graph)$InQuaranta == TRUE, 1,2)
  assortativity_quaranta <- assortativity_nominal(marriage_graph, V(marriage_graph)$types_quaranta, directed = FALSE)
  
  E(marriage_graph)$distances <- 1 / E(marriage_graph)$weight
  V(marriage_graph)$Group <- ifelse(V(marriage_graph)$name %in% lunghi, "Lunghi", ifelse(V(marriage_graph)$name %in% ducali, "Ducali", "Corti"))
  
  component <- components(marriage_graph)$membership
  marriage_graph_main <- subgraph(marriage_graph, V(marriage_graph)$name[component == 1])
  
  vertices_lunghi <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Lunghi"]
  vertices_ducali <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Ducali"]
  vertices_corti <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Corti"]
  
  distance_lunghi_ducali <- distances(marriage_graph_main, v = vertices_lunghi, to = vertices_ducali, weights = E(marriage_graph_main)$distances)
  mean_lunghi_ducali <- mean(distance_lunghi_ducali)
  
  distance_lunghi_lunghi <- distances(marriage_graph_main, v = vertices_lunghi, to = vertices_lunghi, weights = E(marriage_graph_main)$distances)
  mean_lunghi_lunghi <- mean(distance_lunghi_lunghi)
  
  distance_lunghi_corti <- distances(marriage_graph_main, v = vertices_lunghi, to = vertices_corti, weights = E(marriage_graph_main)$distances)
  mean_lunghi_corti <- mean(distance_lunghi_corti)
  
  distance_ducali_ducali <- distances(marriage_graph_main, v = vertices_ducali, to = vertices_ducali, weights = E(marriage_graph_main)$distances)
  mean_ducali_ducali <- mean(distance_ducali_ducali)
                                                      
  distance_ducali_corti <- distances(marriage_graph_main, v = vertices_ducali, to = vertices_corti, weights = E(marriage_graph_main)$distances)
  mean_ducali_corti <- mean(distance_ducali_corti)
  
  distance_corti_corti <- distances(marriage_graph_main, v = vertices_corti, to = vertices_corti, weights = E(marriage_graph_main)$distances)
  mean_corti_corti <- mean(distance_corti_corti)
  
  vertices_quaranta <- V(marriage_graph_main)[V(marriage_graph_main)$name %in% quaranta_famiglie]
  vertices_non_quaranta <- V(marriage_graph_main)[!(V(marriage_graph_main)$name %in% quaranta_famiglie)]
  
  distance_quaranta_non_quaranta <- distances(marriage_graph_main, v = vertices_quaranta, to = vertices_non_quaranta, weights = E(marriage_graph_main)$distances)
  mean_quaranta_non_quaranta <- mean(distance_quaranta_non_quaranta)
  
  distance_quaranta_quaranta <- distances(marriage_graph_main, v = vertices_quaranta, to = vertices_quaranta, weights = E(marriage_graph_main)$distances)
  mean_quaranta_quaranta <- mean(distance_quaranta_quaranta)
  
  distance_non_quaranta_non_quaranta <- distances(marriage_graph_main, v = vertices_non_quaranta, to = vertices_non_quaranta, weights = E(marriage_graph_main)$distances)
  mean_non_quaranta_non_quaranta <- mean(distance_non_quaranta_non_quaranta)
  
  assortativity <- rbind(assortativity,
                         data.frame( curti_lunghi = assortativity_curti_lunghi,
                                     ducali = assortativity_ducali,
                                     in_quaranta = assortativity_quaranta,
                                     distance_curti_lunghi = mean_lunghi_corti,
                                     distance_ducali_corti = mean_ducali_corti,
                                     distance_ducali_lunghi = mean_lunghi_ducali,
                                     intra_lunghi = mean_lunghi_lunghi,
                                     intra_ducali = mean_ducali_ducali,
                                     intra_corti = mean_corti_corti,
                                     intra_quaranta = mean_quaranta_quaranta,
                                     intra_non_quaranta = mean_non_quaranta_non_quaranta,
                                     quaranta_e_non_quaranta = mean_quaranta_non_quaranta,
                                     year = y))
}

library(ggplot2)

ggplot( assortativity, aes(x = year)) +
  geom_line(aes(y = curti_lunghi, color = "Curti vs Lunghi")) +
  geom_line(aes(y = ducali, color = "Ducali vs Non-Ducali")) +
  geom_line(aes(y = in_quaranta, color = "In Quaranta vs Non-In Quaranta")) +
  labs(title = "Assortativity Coefficient Over Time",
       x = "Year",
       y = "Assortativity Coefficient") +
  scale_color_manual(values = c("Curti vs Lunghi" = "blue", "Ducali vs Non-Ducali" = "red", "In Quaranta vs Non-In Quaranta" = "green")) +
  theme_minimal() +
  ylim(-0.1, 0.3)

ggplot( assortativity, aes(x = year)) +
  geom_line(aes(y = distance_curti_lunghi, color = "Lunghi vs Corti")) +
  geom_line(aes(y = distance_ducali_corti, color = "Ducali vs Corti")) +
  geom_line(aes(y = distance_ducali_lunghi, color = "Ducali vs Lunghi")) +
  geom_line(aes(y = intra_lunghi, color = "Intra Lunghi"), linetype = "dashed") +
  geom_line(aes(y = intra_ducali, color = "Intra Ducali"), linetype = "dashed") +
  geom_line(aes(y = intra_corti, color = "Intra Corti"), linetype = "dashed") +
  geom_line(aes(y = quaranta_e_non_quaranta, color = "Quaranta vs Non-Quaranta"), linetype = "dotdash") +
  geom_line(aes(y = intra_quaranta, color = "Intra Quaranta"), linetype = "dotdash") +
  geom_line(aes(y = intra_non_quaranta, color = "Intra Non Quaranta"), linetype = "dotdash") +
  labs(title = "Average Shortest Path Length Over Time",
       x = "Year",
       y = "Average Shortest Path Length") +
  scale_color_manual(values = c("Lunghi vs Corti" = "blue", "Ducali vs Corti" = "red", 
                                "Ducali vs Lunghi" = "green",
                                "Intra Lunghi" = "darkgray","Intra Ducali" = "pink", "Intra Corti" = "gold",
                                "Quaranta vs Non-Quaranta" = "brown",
                                "Intra Quaranta"= "cyan",
                                "Intra Non Quaranta" = "magenta")) +
  theme_minimal()
