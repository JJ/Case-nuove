families.group.accession <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)
load("data/venice-marriages.Rda")

# for every row in marriages.raw, check if the accession date of husband_familyname_std is after the year of the marriage; indicate that in a new column

library(dplyr)
marriages.raw %>% 
  rowwise() %>%
  mutate(
    husband_accession_after_marriage = ifelse(
      husband_familyname_std %in% families.group.accession$Family,
      year < families.group.accession$Accession[match(husband_familyname_std, families.group.accession$Family)],
      NA
    ),
    wife_accession_after_marriage = ifelse(
      wife_familyname_std %in% families.group.accession$Family,
      year < families.group.accession$Accession[match(wife_familyname_std, families.group.accession$Family)],
      NA
    )
  ) -> marriages.check.accession 


# filter marriages.check.accession to keep only rows where husband_accession_after_marriage or wife_accession_after_marriage is TRUE

marriages.with.accession.issues <- marriages.check.accession %>%
  filter(husband_accession_after_marriage == TRUE | wife_accession_after_marriage == TRUE)

marriages.excluding.accession.issues <- marriages.check.accession %>%
  filter(husband_accession_after_marriage != TRUE & wife_accession_after_marriage != TRUE) %>%
  select(-husband_accession_after_marriage, -wife_accession_after_marriage)

