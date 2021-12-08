rsdata <- rsdata %>%
  mutate(
    qi2_1ef = if_else(!is.na(shf_ef), 1, 0),
    qi2_2ecg = if_else(!is.na(shf_ekg) & !is.na(shf_qrs) & !is.na(shf_lbbb), 1, 0),
    qi2_3np = if_else(!is.na(shf_ntprobnp) | !is.na(shf_bnp), 1, 0),
    qi2_4lab = if_else((!is.na(shf_transferrin) | shf_indexdtm < ymd("2017-07-01")) &
      (!is.na(shf_ferritin) | shf_indexdtm < ymd("2017-07-01")) &
      !is.na(shf_hb) &
      !is.na(shf_potassium) &
      !is.na(shf_sodium) &
      !is.na(shf_creatinine), 1, 0),
    qi2_4rehab = case_when(
      is.na(PARTICIPATION_HF_SCHOOL) |
        is.na(PARTICIPATION_HF_TRAINING) |
        shf_indexdtm < ymd("2017-07-01") |
        shf_location == "Out-patient" ~ NA_real_,
      PARTICIPATION_HF_SCHOOL == "YES" | PARTICIPATION_HF_TRAINING == "YES" ~ 0,
      TRUE ~ 1
    ),
    qi2_5follow = case_when(
      is.na(FOLLOWUP_UNIT) |
        shf_indexdtm < ymd("2017-07-01") |
        shf_location == "Out-patient" ~ NA_real_,
      FOLLOWUP_UNIT == "NOT_PLANNED" ~ 0,
      TRUE ~ 1
    ),
    qi3_1bbl = case_when(
      is.na(shf_bbl) | is.na(shf_bblsub) | is.na(shf_ef_cat) | shf_ef_cat != "<=40%" ~ NA_real_,
      shf_bbl == "Yes" & shf_bblsub %in% c("Bisoprolol", "Carvedilol", "Metoprolol") ~ 1,
      TRUE ~ 0
    ),
    qi3_2rasarni = case_when(
      is.na(shf_rasarni) | is.na(shf_ef_cat) | shf_ef_cat != "<=40%" ~ NA_real_,
      shf_rasarni == "Yes" ~ 1,
      TRUE ~ 0
    ),
    qi3_3mra = case_when(
      is.na(shf_mra) | is.na(shf_ef_cat) | shf_ef_cat != "<=40%" ~ NA_real_,
      shf_mra == "Yes" ~ 1,
      TRUE ~ 0
    ),
    qi3_4loop = case_when(
      is.na(shf_xray) | is.na(shf_loopdiuretic) |
        !shf_xray %in% c("Pulmonary congestion", "Pulmonary congestion & cardiomegaly") ~ NA_real_,
      shf_loopdiuretic == "Yes" ~ 1,
      TRUE ~ 0
    ),
    qi4_5crt = case_when(
      is.na(shf_device) | is.na(shf_qrs) | is.na(shf_lbbb) | is.na(shf_ekg) | is.na(shf_ef) | is.na(shf_nyha) ~ NA_real_,
      shf_ef == "<30" & shf_qrs >= 150 & shf_lbbb == "Yes" &
        shf_ekg == "Sinus" & shf_nyha %in% c("II", "III") & !shf_device %in% c("CRT", "CRT & ICD") ~ 0,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1
    ),
    qi4_6icd = case_when(
      is.na(shf_device) | is.na(shf_ef) | is.na(shf_nyha) | is.na(shf_primaryetiology) |
        is.na(shf_bbl) | is.na(shf_rasarni) | is.na(shf_mra) ~ NA_real_,
      shf_ef == "<30" & shf_nyha %in% c("II", "III") &
        shf_primaryetiology == "IHD" &
        shf_bbl == "Yes" & shf_rasarni == "Yes" & shf_mra == "Yes" &
        !shf_device %in% c("ICD", "CRT & ICD") ~ 0,
      shf_device %in% c("ICD", "CRT & ICD") ~ 1
    ),
    qi5_1qol = if_else(!is.na(LIFEQUALITY_SCORE) | !is.na(LIVSKVALITET), 1, 0)
  )

rsdata <- rsdata %>%
  mutate_at(vars(starts_with("qi")), ynfac)

qivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^qi")]

