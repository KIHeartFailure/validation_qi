
# koll <- migration %>%
#  filter(Posttyp == "Utv") %>%
#  group_by(lopnr) %>%
#  slice(2) %>%
#  ungroup() %>%
#  count()

migration <- inner_join(rsdata %>%
  select(LopNr, shf_indexdtm),
migration %>%
  filter(Posttyp == "Utv"),
by = c("LopNr" = "lopnr")
) %>%
  mutate(tmp_migrationdtm = ymd(MigrationDatum)) %>%
  filter(
    tmp_migrationdtm > shf_indexdtm,
    tmp_migrationdtm <= ymd("2019-12-31")
  ) %>%
  group_by(LopNr, shf_indexdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr, shf_indexdtm, tmp_migrationdtm)

rsdata <- left_join(rsdata,
  migration,
  by = c("LopNr", "shf_indexdtm")
)

dors <- bind_rows(
  dors,
  dors2
)


rsdata <- left_join(rsdata,
  dors %>% select(LopNr, ULORSAK, DODSDAT),
  by = "LopNr"
) %>%
  mutate(sos_deathdtm = ymd(case_when(
    substr(DODSDAT, 5, 8) == "0000" ~ paste0(substr(DODSDAT, 1, 4), "0701"),
    substr(DODSDAT, 7, 8) == "00" ~ paste0(substr(DODSDAT, 1, 6), "15"),
    TRUE ~ DODSDAT
  ))) %>%
  rename(sos_deathcause = ULORSAK) %>%
  select(-DODSDAT)


rsdata <- rsdata %>%
  mutate(
    censdtm = coalesce(
      pmin(sos_deathdtm, tmp_migrationdtm, na.rm = TRUE),
      ymd("2019-12-31")
    ),
    sos_out_death = ifelse(censdtm == sos_deathdtm & !is.na(sos_deathdtm), "Yes", "No"),
    sos_outtime_death = as.numeric(censdtm - shf_indexdtm)
  )
