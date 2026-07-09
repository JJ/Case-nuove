library(igraph)
library(dplyr)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)

DEPTH_IN_YEARS <- 75

lunghi <- family_labels[ family_labels$Group == "Lunghi", ]$Family
ducali <- family_labels[ family_labels$Ducale == 1,]$Family

curti_congiurati <- ducali[ !(ducali %in% c("Steno", "Ponte", "Cicogna")) ]

window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1660, by = 5)
distances_window <- data.frame( total = numeric(),
                                congiurati = numeric(),
                                congiurati_norm = numeric(),
                                non_congiurati = numeric(),
                                non_congiurati_norm = numeric(),
                                congiurati_vs_non = numeric(),
                                congiurati_vs_non_norm = numeric(),
                                lunghi = numeric(),
                                lunghi_norm = numeric(),
                                non_lunghi = numeric(),
                                non_lunghi_norm = numeric(),
                                lunghi_vs_non = numeric(),
                                lunghi_vs_non_norm = numeric(),
                                year = integer() )

core_periphery_timeline <- data.frame( ducali = numeric(),
                                        clique_index_congiurati = numeric(),
                                        lunghi = numeric(),
                                        clique_index_lunghi = numeric(),
                                        year = integer() )
  
for (y in window_sequence ) {

  marriages_window <- noble_marriages_filtered %>%
    filter(year <= y + DEPTH_IN_YEARS & year >= y)

  marriage_graph <- graph_from_data_frame(
    marriages_window,
    directed = FALSE,
    vertices = unique(c(marriages_window$husband_familyname_std, marriages_window$wife_familyname_std))
  )
  
  E(marriage_graph)$weight <- 1
  marriage_graph <- simplify(marriage_graph, edge.attr.comb = "sum")
  E(marriage_graph)$distances <- 1 / E(marriage_graph)$weight
  
  V(marriage_graph)$Group <- ifelse(V(marriage_graph)$name %in% lunghi, "Lunghi", ifelse(V(marriage_graph)$name %in% curti_congiurati, "Curti Congiurati", "Curti"))
  
  component <- components(marriage_graph)$membership
  marriage_graph_main <- subgraph(marriage_graph, V(marriage_graph)$name[component == 1])
  
  distance <- distances(marriage_graph_main, weights = E(marriage_graph_main)$distances)
  average_distance <- mean(distance)
  
  vertices_lunghi <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Lunghi"]
  vertices_non_lunghi <- V(marriage_graph_main)[V(marriage_graph_main)$Group != "Lunghi"]
  
  distance_lunghi_non_lunghi <- distances(marriage_graph_main, v = vertices_lunghi, to = vertices_non_lunghi, weights = E(marriage_graph_main)$distances)
  mean_lunghi_non_lunghi <- mean(distance_lunghi_non_lunghi)
  
  distances_lunghi_lunghi <- distances(marriage_graph_main, v = vertices_lunghi, to = vertices_lunghi, weights = E(marriage_graph_main)$distances)
  mean_lunghi_lunghi <- mean(distances_lunghi_lunghi)
  
  distances_non_lunghi_non_lunghi <- distances(marriage_graph_main, v = vertices_non_lunghi, to = vertices_non_lunghi, weights = E(marriage_graph_main)$distances)
  mean_non_lunghi_non_lunghi <- mean(distances_non_lunghi_non_lunghi)
  
  clique_index_lunghi <- mean_lunghi_non_lunghi - mean_lunghi_lunghi
  core_periphery_lunghi_index <- ( mean_non_lunghi_non_lunghi - mean_lunghi_non_lunghi ) / clique_index_lunghi
                           
  vertices_congiurati <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Curti Congiurati"]
  vertices_non_congiurati <- V(marriage_graph_main)[V(marriage_graph_main)$Group != "Curti Congiurati"]
  
  distance_congiurati_non_congiurati <- distances(marriage_graph_main, v = vertices_congiurati, to = vertices_non_congiurati, weights = E(marriage_graph_main)$distances)
  mean_congiurati_non_congiurati <- mean(distance_congiurati_non_congiurati)
  
  distances_congiurati_congiurati <- distances(marriage_graph_main, v = vertices_congiurati, to = vertices_congiurati, weights = E(marriage_graph_main)$distances)
  mean_congiurati_congiurati <- mean(distances_congiurati_congiurati)
  
  distances_non_congiurati_non_congiurati <- distances(marriage_graph_main, v = vertices_non_congiurati, to = vertices_non_congiurati, weights = E(marriage_graph_main)$distances)
  mean_non_congiurati_non_congiurati <- mean(distances_non_congiurati_non_congiurati)
  
  clique_index_congiurati <- mean_congiurati_non_congiurati - mean_congiurati_congiurati
  core_periphery_congiurati_index <- ( mean_non_congiurati_non_congiurati - mean_congiurati_non_congiurati )/clique_index_congiurati
  
  distances_window <- rbind(distances_window,
                         data.frame( total = average_distance,
                                     congiurati = mean_congiurati_congiurati,
                                     congiurati_norm = mean_congiurati_congiurati / average_distance,
                                     non_congiurati = mean_non_congiurati_non_congiurati,
                                     non_congiurati_norm = mean_non_congiurati_non_congiurati / average_distance,
                                     congiurati_vs_non = mean_congiurati_non_congiurati,
                                     congiurati_vs_non_norm = mean_congiurati_non_congiurati / average_distance,
                                     lunghi = mean_lunghi_lunghi,
                                     lunghi_norm = mean_lunghi_lunghi / average_distance,
                                     non_lunghi = mean_non_lunghi_non_lunghi,
                                     non_lunghi_norm = mean_non_lunghi_non_lunghi / average_distance,
                                     lunghi_vs_non = mean_lunghi_non_lunghi,
                                     lunghi_vs_non_norm = mean_lunghi_non_lunghi / average_distance,
                                     year = y))
  
  core_periphery_timeline <- rbind(core_periphery_timeline,
                                     data.frame( congiurati = core_periphery_congiurati_index,
                                                 clique_index_congiurati = clique_index_congiurati,
                                                 lunghi = core_periphery_lunghi_index,
                                                 clique_index_lunghi = clique_index_lunghi,
                                                 year = y))
}

library(ggplot2)

ggplot( distances_window, aes(x = year)) +
  geom_line(aes(y = congiurati, color = "congiurati")) +
  geom_line(aes(y = non_congiurati, color = "Non congiurati"), linetype = "dashed") +
  geom_line(aes(y = congiurati_vs_non, color = "congiurati vs. Non"),linetype = "dotdash") +
  geom_line(aes(y = lunghi, color = "Lunghi")) +
  geom_line(aes(y = non_lunghi, color = "Non Lunghi"), linetype = "dashed") +
  geom_line(aes(y = lunghi_vs_non, color = "Lunghi vs. Non"), linetype = "dotdash") +
  labs(title = "Average Shortest Path Length Over Time",
       x = "Year",
       y = "Average Shortest Path Length") +
  scale_color_manual(values = c("congiurati" = "blue", "Non congiurati" = "blue", "congiurati vs. Non" = "blue",
                                "Lunghi" = "gold","Non Lunghi" = "gold", "Lunghi vs. Non" = "gold"
                                )) +
  theme_minimal()

ggplot( distances_window, aes(x = year)) +
  geom_line(aes(y = congiurati_norm, color = "congiurati")) +
  geom_line(aes(y = non_congiurati_norm, color = "Non congiurati"), linetype = "dashed") +
  geom_line(aes(y = congiurati_vs_non_norm, color = "congiurati vs. Non"),linetype = "dotdash") +
  geom_line(aes(y = lunghi_norm, color = "Lunghi")) +
  geom_line(aes(y = non_lunghi_norm, color = "Non Lunghi"), linetype = "dashed") +
  geom_line(aes(y = lunghi_vs_non_norm, color = "Lunghi vs. Non"), linetype = "dotdash") +
  labs(title = "Normalized Average Shortest Path Length Over Time",
       x = "Year",
       y = "Normalized Average Shortest Path Length") +
  scale_color_manual(values = c("congiurati" = "blue", "Non congiurati" = "blue", "congiurati vs. Non" = "blue",
                                "Lunghi" = "gold","Non Lunghi" = "gold", "Lunghi vs. Non" = "gold"
                                )
                     ) +
  theme_minimal()

ggplot( core_periphery_timeline, aes(x = year)) + 
  geom_line(aes(y = clique_index_congiurati, color = "congiurati")) +
  geom_line(aes(y = clique_index_lunghi, color = "Lunghi")) +
  labs(title = "Clique Index Over Time",
       subtitle="Lower is better",
       x = "Year",
       y = "Power-Periphery Index") +
  scale_color_manual(values = c("congiurati" = "blue", "Lunghi" = "gold")) +
  theme_minimal()

ggplot( core_periphery_timeline, aes(x = year)) + 
  geom_line(aes(y = congiurati, color = "congiurati")) +
  geom_line(aes(y = lunghi, color = "Lunghi")) +
  labs(title = "Core-Periphery Index Over Time",
       subtitle = "Relationship between differences in average distances non-core - core and non core / core and non-core - core\nLower is better"
       x = "Year",
       y = "Core-Periphery Index") +
  scale_color_manual(values = c("congiurati" = "blue", "Lunghi" = "gold")) +
  theme_minimal()


