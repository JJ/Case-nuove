devtools::load_all()

load("data/venice-marriages-filtered.rda")
load("data/ducali_dogi_data.rda")
load("data/families.rda")

DEPTH_IN_YEARS <- 90

# extract from the family.types list those whose value is "Ducali"

ducali_families <- names(family.types)[family.types == "Ducali"]
lunghi_families <- names(family.types)[family.types == "Apostoliche" | family.types == "Vecchie" | family.types == "Evangeliche"]
nuove_families <- names(family.types)[family.types == "Ducali" | family.types == "Nuove"]


first_network <- create_marriage_network(
  venice_marriages,
  election_year = ducali_dogi_data$Year[1],
  years_before = DEPTH_IN_YEARS
)

permanent_families <- V(first_network)$name

for (i in 2:nrow(ducali_dogi_data)) {
  election_year <- ducali_dogi_data$Year[i]
  casata <- ducali_dogi_data$Casata[i]

  marriage_graph_slice <- create_marriage_network(
    venice_marriages,
    election_year = election_year,
    years_before = DEPTH_IN_YEARS
  )

  permanent_families <- intersect(permanent_families, V(marriage_graph_slice)$name)
}

# for every family in permanent_families, map it to its type
#

permanent_families_types <- sapply(permanent_families, function(family) {
  if (family %in% names(family.types)) {
    return(family.types[[family]])
  } else {
    return(NA)
  }
})
permanent_families_df <- data.frame(
  Casata = permanent_families,
  Type = permanent_families_types,
  stringsAsFactors = FALSE
)


# Try after the first ducali election
first_network <- create_marriage_network(
  venice_marriages,
  election_year = ducali_dogi_data$Year[2],
  years_before = DEPTH_IN_YEARS
)

permanent_families_2 <- V(first_network)$name

for (i in 3:nrow(ducali_dogi_data)) {
  election_year <- ducali_dogi_data$Year[i]
  casata <- ducali_dogi_data$Casata[i]

  marriage_graph_slice <- create_marriage_network(
    venice_marriages,
    election_year = election_year,
    years_before = DEPTH_IN_YEARS
  )

  permanent_families_2 <- intersect(permanent_families_2, V(marriage_graph_slice)$name)
}

# for every family in permanent_families, map it to its type
#

permanent_families_types <- sapply(permanent_families_2, function(family) {
  if (family %in% names(family.types)) {
    return(family.types[[family]])
  } else {
    return(NA)
  }
})
permanent_families_2_df <- data.frame(
  Casata = permanent_families_2,
  Type = permanent_families_types,
  stringsAsFactors = FALSE
)
