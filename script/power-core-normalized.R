library(igraph)
library(dplyr)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)

DEPTH_IN_YEARS <- 75

lunghi <- family_labels[ family_labels$Group == "Lunghi", ]$Family
ducali <- family_labels[ family_labels$Ducale == 1,]$Family
quaranta_famiglie <- c(lunghi,ducali)

quaranta_famiglie <- quaranta_famiglie[ !(quaranta_famiglie %in% c("Steno", "Ponte", "Cicogna")) ]

chojnacki_50 <- c( quaranta_famiglie, "Barbo", "Belegno", "Da Molin", "Lion", "Mosto", "Barbaro", "Minio", "Bondumier", "Capello", "De Mezzo", "Viaro", "Gabriel", "Foscarini","Rosso", "Pisani", "Nani")

chojnacki <- chojnacki_50[ !(chojnacki_50 %in% c("Barozzi", "Salamon","Tiepolo", "Tron","Lando","Vendramin"))]

window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1660, by = 5)
distances_window <- data.frame( total = numeric(),
                             ducali = numeric(),
                             ducali_norm = numeric(),
                             non_ducali = numeric(),
                             non_ducali_norm = numeric(),
                             ducali_vs_non = numeric(),
                             ducali_vs_non_norm = numeric(),
                             lunghi = numeric(),
                             lunghi_norm = numeric(),
                             non_lunghi = numeric(),
                             non_lunghi_norm = numeric(),
                             lunghi_vs_non = numeric(),
                             lunghi_vs_non_norm = numeric(),
                             quaranta = numeric(),
                             quaranta_norm = numeric(),
                             non_quaranta = numeric(),
                             non_quaranta_norm = numeric(),
                             quaranta_vs_non = numeric(),
                             quaranta_vs_non_norm = numeric(),
                             year = integer() )

power_periphery_timeline <- data.frame( ducali = numeric(),
                                        clique_index_ducali = numeric(),
                                        lunghi = numeric(),
                                        clique_index_lunghi = numeric(),
                                        in_quaranta = numeric(),
                                        clique_index_quaranta = numeric(),
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
  
  V(marriage_graph)$Group <- ifelse(V(marriage_graph)$name %in% lunghi, "Lunghi", ifelse(V(marriage_graph)$name %in% ducali, "Ducali", "Corti"))
  
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
  power_periphery_lunghi_index <- ( mean_non_lunghi_non_lunghi - mean_lunghi_non_lunghi ) / clique_index_lunghi
                           
  vertices_ducali <- V(marriage_graph_main)[V(marriage_graph_main)$Group == "Ducali"]
  vertices_non_ducali <- V(marriage_graph_main)[V(marriage_graph_main)$Group != "Ducali"]
  
  distance_ducali_non_ducali <- distances(marriage_graph_main, v = vertices_ducali, to = vertices_non_ducali, weights = E(marriage_graph_main)$distances)
  mean_ducali_non_ducali <- mean(distance_ducali_non_ducali)
  
  distances_ducali_ducali <- distances(marriage_graph_main, v = vertices_ducali, to = vertices_ducali, weights = E(marriage_graph_main)$distances)
  mean_ducali_ducali <- mean(distances_ducali_ducali)
  
  distances_non_ducali_non_ducali <- distances(marriage_graph_main, v = vertices_non_ducali, to = vertices_non_ducali, weights = E(marriage_graph_main)$distances)
  mean_non_ducali_non_ducali <- mean(distances_non_ducali_non_ducali)
  
  clique_index_ducali <- mean_ducali_non_ducali - mean_ducali_ducali
  power_periphery_ducali_index <- ( mean_non_ducali_non_ducali - mean_ducali_non_ducali )/clique_index_ducali
  
  vertices_quaranta <- V(marriage_graph_main)[V(marriage_graph_main)$name %in% quaranta_famiglie]
  vertices_non_quaranta <- V(marriage_graph_main)[!(V(marriage_graph_main)$name %in% quaranta_famiglie)]
  
  distance_quaranta_non_quaranta <- distances(marriage_graph_main, v = vertices_quaranta, to = vertices_non_quaranta, weights = E(marriage_graph_main)$distances)
  mean_quaranta_non_quaranta <- mean(distance_quaranta_non_quaranta)
  
  distance_quaranta_quaranta <- distances(marriage_graph_main, v = vertices_quaranta, to = vertices_quaranta, weights = E(marriage_graph_main)$distances)
  mean_quaranta_quaranta <- mean(distance_quaranta_quaranta)
  
  distance_non_quaranta_non_quaranta <- distances(marriage_graph_main, v = vertices_non_quaranta, to = vertices_non_quaranta, weights = E(marriage_graph_main)$distances)
  mean_non_quaranta_non_quaranta <- mean(distance_non_quaranta_non_quaranta)
  
  clique_index_quaranta <- mean_quaranta_non_quaranta - mean_quaranta_quaranta
  power_periphery_quaranta_index <- ( mean_non_quaranta_non_quaranta - mean_quaranta_non_quaranta )/clique_index_quaranta
  
  distances_window <- rbind(distances_window,
                         data.frame( total = average_distance,
                                     ducali = mean_ducali_ducali,
                                     ducali_norm = mean_ducali_ducali / average_distance,
                                     non_ducali = mean_non_ducali_non_ducali,
                                     non_ducali_norm = mean_non_ducali_non_ducali / average_distance,
                                     ducali_vs_non = mean_ducali_non_ducali,
                                     ducali_vs_non_norm = mean_ducali_non_ducali / average_distance,
                                     lunghi = mean_lunghi_lunghi,
                                     lunghi_norm = mean_lunghi_lunghi / average_distance,
                                     non_lunghi = mean_non_lunghi_non_lunghi,
                                     non_lunghi_norm = mean_non_lunghi_non_lunghi / average_distance,
                                     lunghi_vs_non = mean_lunghi_non_lunghi,
                                     lunghi_vs_non_norm = mean_lunghi_non_lunghi / average_distance,
                                     quaranta = mean_quaranta_quaranta,
                                     quaranta_norm = mean_quaranta_quaranta / average_distance,
                                     non_quaranta = mean_non_quaranta_non_quaranta,
                                     non_quaranta_norm = mean_non_quaranta_non_quaranta / average_distance,
                                     quaranta_vs_non = mean_quaranta_non_quaranta,
                                     quaranta_vs_non_norm = mean_quaranta_non_quaranta / average_distance,
                                     year = y))
  
  power_periphery_timeline <- rbind(power_periphery_timeline,
                                     data.frame( ducali = power_periphery_ducali_index,
                                                 clique_index_ducali = clique_index_ducali,
                                                 lunghi = power_periphery_lunghi_index,
                                                 clique_index_lunghi = clique_index_lunghi,
                                                 in_quaranta = power_periphery_quaranta_index,
                                                 clique_index_quaranta = clique_index_quaranta,
                                                 year = y))
}

library(ggplot2)

ggplot( distances_window, aes(x = year)) +
  geom_line(aes(y = ducali, color = "Ducali")) +
  geom_line(aes(y = non_ducali, color = "Non Ducali"), linetype = "dashed") +
  geom_line(aes(y = ducali_vs_non, color = "Ducali vs. Non"),linetype = "dotdash") +
  geom_line(aes(y = lunghi, color = "Lunghi")) +
  geom_line(aes(y = non_lunghi, color = "Non Lunghi"), linetype = "dashed") +
  geom_line(aes(y = lunghi_vs_non, color = "Lunghi vs. Non"), linetype = "dotdash") +
  geom_line(aes(y = quaranta, color = "Quaranta")) +
  geom_line(aes(y = non_quaranta, color = "Non Quaranta"), linetype = "dashed") +
  geom_line(aes(y = quaranta_vs_non, color = "Quaranta vs. Non"), linetype = "dotdash") +
  labs(title = "Average Shortest Path Length Over Time",
       x = "Year",
       y = "Average Shortest Path Length") +
  scale_color_manual(values = c("Ducali" = "blue", "Non Ducali" = "blue", "Ducali vs. Non" = "blue",
                                "Lunghi" = "gold","Non Lunghi" = "gold", "Lunghi vs. Non" = "gold",
                                "Quaranta" = "brown",
                                "Non Quaranta"= "brown",
                                "Quaranta vs. Non" = "brown")) +
  theme_minimal()

ggplot( distances_window, aes(x = year)) +
  geom_line(aes(y = ducali_norm, color = "Ducali")) +
  geom_line(aes(y = non_ducali_norm, color = "Non Ducali"), linetype = "dashed") +
  geom_line(aes(y = ducali_vs_non_norm, color = "Ducali vs. Non"),linetype = "dotdash") +
  geom_line(aes(y = lunghi_norm, color = "Lunghi")) +
  geom_line(aes(y = non_lunghi_norm, color = "Non Lunghi"), linetype = "dashed") +
  geom_line(aes(y = lunghi_vs_non_norm, color = "Lunghi vs. Non"), linetype = "dotdash") +
  geom_line(aes(y = quaranta_norm, color = "Quaranta")) +
  geom_line(aes(y = non_quaranta_norm, color = "Non Quaranta"), linetype = "dashed") +
  geom_line(aes(y = quaranta_vs_non_norm, color = "Quaranta vs. Non"), linetype = "dotdash") +
  labs(title = "Average Shortest Path Length Over Time",
       x = "Year",
       y = "Average Shortest Path Length") +
  scale_color_manual(values = c("Ducali" = "blue", "Non Ducali" = "blue", "Ducali vs. Non" = "blue",
                                "Lunghi" = "gold","Non Lunghi" = "gold", "Lunghi vs. Non" = "gold",
                                "Quaranta" = "brown",
                                "Non Quaranta"= "brown",
                                "Quaranta vs. Non" = "brown")) +
  theme_minimal()

ggplot( power_periphery_timeline, aes(x = year)) + 
  geom_line(aes(y = ducali, color = "Ducali")) +
  geom_line(aes(y = lunghi, color = "Lunghi")) +
  geom_line(aes(y = in_quaranta, color = "In Quaranta")) +
  labs(title = "Power-Periphery Index Over Time",
       x = "Year",
       y = "Power-Periphery Index") +
  scale_color_manual(values = c("Ducali" = "blue", "Lunghi" = "gold", "In Quaranta" = "brown")) +
  theme_minimal()

ggplot( power_periphery_timeline, aes(x = year)) + 
  geom_line(aes(y = clique_index_ducali, color = "Ducali")) +
  geom_line(aes(y = clique_index_lunghi, color = "Lunghi")) +
  geom_line(aes(y = clique_index_quaranta, color = "In Quaranta")) +
  labs(title = "Power-Periphery Index Over Time",
       x = "Year",
       y = "Power-Periphery Index") +
  scale_color_manual(values = c("Ducali" = "blue", "Lunghi" = "gold", "In Quaranta" = "brown")) +
  theme_minimal()
