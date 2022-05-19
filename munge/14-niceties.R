
rsdata <- rsdata %>%
  select(
    LopNr, 
    shf_source, shf_indexdtm, shf_indexhosptime, shf_indexyear, shf_type,
    contains("shf_ef"),
    contains("shf_"),
    contains("scb_"),
    censdtm,
    contains("sos_"),
    contains("qi"),
    PARTICIPATION_HF_SCHOOL, PARTICIPATION_HF_TRAINING, FOLLOWUP_UNIT
  )


rsdata <- rsdata %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    shf_centre = as.character(shf_centre), 
    shf_centreregion = as.character(shf_centreregion)
  )