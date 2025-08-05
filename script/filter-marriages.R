load("data/venice-marriages.rda")

# eliminate all rows that have no year or no wife name. Eliminate also the marriage_number column
#

library(dplyr)
venice_marriages <- marriages.raw %>%
  filter(!is.na(year) & wife_familyname_std!="") %>%
  select(-marriage_number)

save(venice_marriages, file = "data/venice-marriages.rda")
