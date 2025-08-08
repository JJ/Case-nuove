#read data/doges.years.rda

load("data/doges.years.rda")

doges.years.after1383 <- doges.years[doges.years$Start > 1383, ]

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

doges.years.after1383$Casata <- gsub("Trivisan", "Trevisan", doges.years.after1383$Casata)
doges.years.after1383$Casata <- gsub("Da Ponte", "Ponte", doges.years.after1383$Casata)
doges.years.after1383$Casata <- gsub("Molin", "Da Molin", doges.years.after1383$Casata)

ducali_dogi_data <- doges.years.after1383[ doges.years.after1383$Year <= 1656, ]
save(ducali_dogi_data, file = "data/ducali_dogi_data.rda")
