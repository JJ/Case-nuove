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

