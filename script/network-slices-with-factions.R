library(dplyr)
library(igraph)

noble_marriages_filtered <- read.csv("data/noble-marriages-year.csv", stringsAsFactors = FALSE)
family_labels <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)


DEPTH_IN_YEARS <- 75

results_df <- data.frame( Year = integer(),
                          Casata_doge = character(),
                          Type = character(),
                          DogeFamilyBetweennessPosition = integer(),
                          DogeFamilyClosenessPosition = integer(),
                          DogeFamilyEigenCentralityPosition = integer(),
                          TopFamilyBetweenness = character(),
                          TopFamilyCloseness = character(),
                          TopFamilyEigenCentrality = character())
# create a sequence that starts with the first year in noble_marriages_filtered and ends in 1650 with increments of 25
window_sequence <- seq(from = min(noble_marriages_filtered$year), to = 1600, by = 25)

for (y in window_sequence ) {

  marriages_window <- noble_marriages_filtered %>%
    filter(year <= y + DEPTH_IN_YEARS & year >= y)

  marriage_graph <- graph_from_data_frame(
    marriages_window,
    directed = FALSE,
    vertices = unique(c(marriages_before_ducale$husband_familyname_std, marriages_before_ducale$wife_familyname_std))
  )

}

