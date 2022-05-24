oldrs <- left_join(
  oldrs %>% select(-LANDSTING),
  enheteroldrs %>% select(ID, CENTRENAME, LANDSTING),
  by = c("CENTREID" = "ID")
)

oldrs <- oldrs %>%
  mutate(
    CENTRENAME = case_when(
      CENTRENAME == "Sahlgrenska Universitetssjukhuset / Sahlgrenska" ~ "Sahlgrenska Universitetssjukhuset - Sahlgrenska",
      CENTRENAME == "Sahlgrenska Universitetssjukhuset  / Molndal" ~ "Sahlgrenska Universitetssjukhuset - Molndal",
      CENTRENAME == "Sahlgrenska Universitetssjukhuset  / Ostra" ~ "Sahlgrenska Universitetssjukhuset - Ostra",
      CENTRENAME %in% c("Danderyd Web", "Danderyds sjukhus AB") ~ "Danderyds sjukhus",
      CENTRENAME %in% c("Karolinska Huddinge", "Karolinska Solna") ~ "Karolinska",
      CENTRENAME %in% c("Skanes universitetssjukhus  Lund", "Skanes universitetssjukhus Malmo") ~ "Skanes universitetssjukhus",
      CENTRENAME == "Halsoringen i Osby" ~ "VC Helsa Osby",
      TRUE ~ CENTRENAME
    )
  )

newrs <- left_join(
  newrs,
  enheternewrs %>% select(ID, ORG_UNIT_NAME, ORG_UNIT_LEVEL_NAME, PARENT1, PARENT2),
  by = c("HEALTHCAREUNIT" = "ID")
)

# sjukhusnivå
newrs <- left_join(
  newrs,
  enheternewrs %>%
    filter(DEPTH == 2) %>%
    rename(tmp_sjhnewrs = ORG_UNIT_NAME) %>%
    select(ID, tmp_sjhnewrs),
  by = c("PARENT2" = "ID")
) %>%
  mutate(sjhnewrs = case_when(
    ORG_UNIT_LEVEL_NAME %in% c("Fristaende hjartmottagning", "Vardcentral") ~ ORG_UNIT_NAME,
    TRUE ~ tmp_sjhnewrs
  ))
