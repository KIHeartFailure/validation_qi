
# Treatments from the DDR -------------------------------------------------

# within 5 months prior to index

lmtmp <- left_join(
  rsdata %>%
    select(LopNr, shf_indexdtm),
  lmsel,
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 5, diff <= 14) %>%
  select(LopNr, shf_indexdtm, EDATUM, ATC)


rsdata <- create_medvar(
  atc = "^(A10BK|A10BD1[5-6]|A10BD19|A10BD2[0-1]|A10BD2[3-5]|A10BX09|A10BX11|A10BX12)",
  medname = "sglt2i", cohortdata = rsdata, meddata = lmtmp, id = "LopNr", metatime = "-5mo-14days",
  valsclass = "fac"
)

rm(lmtmp)
rm(lmsel)
