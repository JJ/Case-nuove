families.group.accession <- read.csv("data/family-labels-accession.csv", stringsAsFactors = FALSE)
load("data/venice-marriages.Rda")

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

marriages.with.accession.issues <- marriages.check.accession %>%
  filter(husband_accession_after_marriage == TRUE | wife_accession_after_marriage == TRUE)

marriages.excluding.accession.issues <- marriages.check.accession %>%
  filter(husband_accession_after_marriage != TRUE & wife_accession_after_marriage != TRUE) %>%
  select(-husband_accession_after_marriage, -wife_accession_after_marriage)

marriage.with.missing.accession <- marriages.check.accession %>%
  filter(is.na(husband_accession_after_marriage) | is.na(wife_accession_after_marriage)) %>% filter( !is.na(year) & wife_familyname_std!="") 

missing_families <- unique( marriage.with.missing.accession$wife_familyname_std)
