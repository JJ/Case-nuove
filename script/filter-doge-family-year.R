#read data/doges.years.rda

load("data/doges.years.rda")

# filter the data from the year 1383

doges.years.after1383 <- doges.years[doges.years$Start > 1383, ]

# Leave only columns Family and Start and change Start to Year

doges.years.after1383 <- doges.years.after1383[, c("Family.doge", "Start")]
colnames(doges.years.after1383) <- c("Casata","Year")

load("data/families.rda")

doges.years.after1383$Type <- sapply(doges.years.after1383$Casata, function(family) {
  if (family %in% names(family.types)) {
    return(family.types[[family]])
  } else {
    return(NA)
  }
})


ducali_dogi_data <- doges.years.after1383[ doges.years.after1383$Year <= 1656, ]
save(ducali_dogi_data, file = "data/ducali_dogi_data.rda")
