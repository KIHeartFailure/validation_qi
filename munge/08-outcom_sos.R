

# Comorbidities -----------------------------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "diabetes",
  diakod = " E1[0-4]",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "hypertension",
  diakod = " I1[0-5]",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "ihd",
  diakod = " 41[0-4]| I2[0-5]",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "mi",
  diakod = " I21| I22| I252",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "pcicabg",
  diakod = " Z951| Z955",
  opkod = " FNG| FNA| FNB| FNC| FND| FNE| FNF| FNH",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "stroketia",
  diakod = " 43[0-4]| 438| I6[0-4]| I69[0-3]| G45",
  # stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "af",
  diakod = " I48",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "peripheralartery",
  diakod = " I7[0-3]",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "copd",
  diakod = " J4[3-4]",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "renal",
  diakod = " N1[7-9]| Z491| Z492",
  opkod = " KAS00| KAS10| KAS20| DR014| DR015| DR016| DR020| DR012| DR013| DR023| DR024| TJA33| TJA35",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "liver",
  diakod = " B18| I85| I864| I982| K70| K71| K7[2-4]| K760| K76[2-9]",
  #stoptime = -5 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)
rsdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "com",
  name = "cancer3y",
  diakod = " C",
  #stoptime = -3 * 365.25,
  valsclass = "fac",
  warnings = FALSE
)

# Outcomes ----------------------------------------------------------------


rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "hosphf",
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "NPR (in)"
)

# Time since last HF hospitalization --------------------------------------

rsdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "com",
  name = "hosphf",
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE,
  comduration = TRUE, 
  meta_reg = "NPR (in)"
)
