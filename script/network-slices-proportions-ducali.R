devtools::load_all()

load("data/venice-marriages-filtered.rda")
load("data/ducali_dogi_data.rda")
load("data/families.rda")

DEPTH_IN_YEARS <- 90

proportions_results_df <- data.frame( Year = integer(),
                          Casata_doge = character(),
                          Type = character(),
                          ProportionLunghiInFirstCommunity = numeric(),
                          ProportionDucaliInFirstCommunity = numeric())

# extract from the family.types list those whose value is "Ducali"

ducali_families <- names(family.types)[family.types == "Ducali"]
lunghi_families <- names(family.types)[family.types == "Apostoliche" | family.types == "Vecchie" | family.types == "Evangeliche"]

ducali_families_number <- length(ducali_families)
lunghi_families_number <- length(lunghi_families)

for (i in 1:nrow(ducali_dogi_data)) {
  election_year <- ducali_dogi_data$Year[i]
  casata <- ducali_dogi_data$Casata[i]
  type <- ducali_dogi_data$Type[i]

  marriage_graph_slice <- create_marriage_network(
    venice_marriages,
    election_year = election_year,
    years_before = DEPTH_IN_YEARS
  )

  # Extract and simplify the main component of the graph
  main_component_graph <- extract_main_component(marriage_graph_slice)

  # Analyze community structure
  community_analysis <- analyze_communities(
    graph = main_component_graph,
    total_families = families_in_graph
  )

  # extract the key of the first element in community_analysis$sorted_communities
  first_community <- names(community_analysis$sorted_communities)[1]
  families_in_first_community <- unname(community_analysis$communities[first_community])[[1]]


  # Intersection between ducali_families and families in the first community
  ducali_families_in_first_community <- intersect(ducali_families, families_in_first_community)
  lunghi_families_in_first_community <- intersect(lunghi_families, families_in_first_community)

  results_df <- rbind(results_df,
                           data.frame(Year = election_year,
                                      Casata_doge = casata,
                                      Type = type,
                                      ProportionLunghiInFirstCommunity = length(lunghi_families_in_first_community) / lunghi_families_number,
                                      ProportionDucaliInFirstCommunity = length(ducali_families_in_first_community) / ducali_families_number))
  }

save(proportions_results_df, file = "data/proportions_results_df.rda")
