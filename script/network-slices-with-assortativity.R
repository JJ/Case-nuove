library(igraph)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)
load("data/doges.years.rda")

DEPTH_IN_YEARS <- 75

lunghi <- family_labels[ family_labels$Group == "Lunghi", ]$Family
ducali <- family_labels[ family_labels$Ducale == 1,]$Family
window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1600, by = 25)
assortativity <- data.frame( curti_lunghi = numeric(),
                             ducali = numeric() )

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
  
  assortativity <- rbind(assortativity,
                         data.frame( curti_lunghi = assortativity_curti_lunghi,
                                     ducali = assortativity_ducali))
  V(marriage_graph)$color <- ifelse(V(marriage_graph)$Faction == "Lunghi", "lightgray", 
                                    ifelse( V(marriage_graph)$Ducale == TRUE,"pink","red"))
  
  V(marriage_graph)$shape <- ifelse(V(marriage_graph)$name %in% doges, "square", "circle")
  
  png(paste0("plots/marriage-network-factions-", y, ".png"), width = 1600, height = 1600)
  plot(
    marriage_graph,
    vertex.size = 5,
    vertex.label.cex = 2,
    edge.width = E(marriage_graph)$weight,
    layout = layout_with_kk,
    main = paste("Marriage Network from", y, "to", y + DEPTH_IN_YEARS)
  )
  dev.off()
}

library(ggplot2)

assortativity$Year <- window_sequence
ggplot( assortativity, aes(x = Year)) +
  geom_line(aes(y = curti_lunghi, color = "Curti vs Lunghi")) +
  geom_line(aes(y = ducali, color = "Ducali vs Non-Ducali")) +
  labs(title = "Assortativity Coefficient Over Time",
       x = "Year",
       y = "Assortativity Coefficient") +
  scale_color_manual(values = c("Curti vs Lunghi" = "blue", "Ducali vs Non-Ducali" = "red")) +
  theme_minimal() +
  ylim(-0.1, 0.3)
