library(igraph)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)


DEPTH_IN_YEARS <- 75

lunghi <- family_labels[ family_labels$Group == "Lunghi", ]$Family
window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1600, by = 25)

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
  V(marriage_graph)$Faction <- ifelse(V(marriage_graph)$name %in% lunghi, "Lunghi", "Corti")
  V(marriage_graph)$color <- ifelse(V(marriage_graph)$Faction == "Lunghi", "blue", "red")
  
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

