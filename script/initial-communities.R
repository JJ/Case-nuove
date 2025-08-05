library(dplyr)
library(igraph)

load("data/venice-marriages-filtered.rda")


first_ducale_date <- 1413

marriages_before_ducale <- venice_marriages %>%
  filter(year <= first_ducale_date)

marriage_graph_before_first_ducale <- graph_from_data_frame(
  marriages_before_ducale,
  directed = FALSE,
  vertices = unique(c(marriages_before_ducale$husband_familyname_std, marriages_before_ducale$wife_familyname_std))
)

plot(marriage_graph_before_first_ducale,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     main = "Marriages Before the Ducale (1413)"
)

save(marriage_graph_before_first_ducale, file = "data/marriage_graph_before_first_ducale.rda")

# take only the main component of the graph

main_component <- components(marriage_graph_before_first_ducale)$membership
main_component_graph <- subgraph(marriage_graph_before_first_ducale, V(marriage_graph_before_first_ducale)$name[main_component == 1])
plot(main_component_graph,
     vertex.size = 5,
     edge.arrow.size = 0.5,
     main = "Main Component of Marriages Before the Ducale (1413)"
)

# consolidate multi-edges into weights

main_component_graph <- simplify(main_component_graph, remove.multiple = TRUE, edge.attr.comb = "sum")

V(main_component_graph)$betweenness <- betweenness(main_component_graph)
V(main_component_graph)$closeness <- closeness(main_component_graph)

plot(main_component_graph,
     vertex.size = 1+V(main_component_graph)$betweenness/100,
     edge.arrow.size = 0.5,
     main = "Main Component of Marriages Before the Ducale (1413) - Betweenness Centrality"
)

plot(main_component_graph,
     vertex.size = V(main_component_graph)$closeness*1000,
     edge.arrow.size = 0.5,
     main = "Main Component of Marriages Before the Ducale (1413) - Betweenness Centrality"
)


