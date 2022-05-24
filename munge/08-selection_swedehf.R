
flow <- c("No of posts in SwedeHF", nrow(rsdata))

# remove duplicated indexdates
rsdata <- rsdata %>%
  group_by(LopNr, shf_indexdtm) %>%
  arrange(shf_source) %>%
  slice(n()) %>%
  ungroup()

flow <- rbind(flow, c("Exclude posts with duplicated dates", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_indexdtm >= ymd("2013-01-01"), shf_indexdtm <= ymd("2019-12-31"))

flow <- rbind(flow, c("Include encounter between 2013-01-01 - 2019-12-31", nrow(rsdata)))


rsdata <- left_join(rsdata,
  pnr_bytt_ater,
  by = "LopNr"
)

rsdata <- rsdata %>%
  filter(is.na(AterPnr) & is.na(ByttPnr)) %>% # reused/changed personr
  select(-ByttPnr, -AterPnr)

flow <- rbind(flow, c("Exclude posts with reused or changed PINs", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_age >= 18 & !is.na(shf_age))

flow <- rbind(flow, c("Exclude posts < 18 years", nrow(rsdata)))

rsdata <- rsdata %>%
  filter((shf_indexdtm < sos_deathdtm | is.na(sos_deathdtm))) # enddate prior to indexdate

flow <- rbind(flow, c("Exclude posts died in hospital", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_centretype != "Primary care")

flow <- rbind(flow, c("Exclude primary care", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(shf_type == "Index") %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("Include only index visits and first post/patient", nrow(rsdata)))
