rsdata <- rsdata %>%
  mutate(
    avqi_all = 1,
    qi2_1ef = if_else(!is.na(shf_ef), 1, 0),
    qi2_2ecg = if_else(!is.na(shf_ekg) & !is.na(shf_qrs) & !is.na(shf_lbbb), 1, 0),
    qi2_3np = if_else(!is.na(shf_ntprobnp) | !is.na(shf_bnp), 1, 0),
    qi2_4lab = if_else((!is.na(shf_transferrin) | shf_indexdtm < ymd("2017-07-01")) &
      (!is.na(shf_ferritin) | shf_indexdtm < ymd("2017-07-01")) &
      !is.na(shf_hb) &
      !is.na(shf_potassium) &
      !is.na(shf_sodium) &
      !is.na(shf_creatinine), 1, 0),
    avqi_rehabfollow = if_else(shf_indexdtm < ymd("2017-07-01") |
      shf_location == "Out-patient", 0, 1),
    qi2_5rehab = case_when(
      is.na(PARTICIPATION_HF_SCHOOL) |
        is.na(PARTICIPATION_HF_TRAINING) |
        avqi_rehabfollow == 0 ~ NA_real_,
      PARTICIPATION_HF_SCHOOL == "YES" | PARTICIPATION_HF_TRAINING == "YES" ~ 0,
      TRUE ~ 1
    ),
    qi2_6follow = case_when(
      is.na(FOLLOWUP_UNIT) |
        avqi_rehabfollow == 0 ~ NA_real_,
      FOLLOWUP_UNIT == "NOT_PLANNED" ~ 0,
      TRUE ~ 1
    ),
    avqi_ef40 = if_else(is.na(shf_ef_cat) | shf_ef_cat != "<40%", 0, 1),
    qi3_1bbl = case_when(
      is.na(shf_bbl) | is.na(shf_bblsub) | avqi_ef40 == 0 ~ NA_real_,
      shf_bbl == "Yes" & shf_bblsub %in% c("Bisoprolol", "Carvedilol", "Metoprolol") ~ 1,
      TRUE ~ 0
    ),
    qi3_2rasarni = case_when(
      is.na(shf_rasarni) | avqi_ef40 == 0 ~ NA_real_,
      shf_rasarni == "Yes" ~ 1,
      TRUE ~ 0
    ),
    qi3_3mra = case_when(
      is.na(shf_mra) | avqi_ef40 == 0 ~ NA_real_,
      shf_mra == "Yes" ~ 1,
      TRUE ~ 0
    ),
    avqi_loop = if_else(is.na(shf_xray) |
      !shf_xray %in% c("Pulmonary congestion", "Pulmonary congestion & cardiomegaly"), 0, 1),
    qi3_4loop = case_when(
      is.na(shf_loopdiuretic) |
        avqi_loop == 0 ~ NA_real_,
      shf_loopdiuretic == "Yes" ~ 1,
      TRUE ~ 0
    ),
    avqi_crt = case_when(
      is.na(shf_ef) | shf_ef != "<30" ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      is.na(shf_nyha) | !shf_nyha %in% c("II", "III") ~ 0,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1,
      is.na(shf_qrs) | is.na(shf_lbbb) | is.na(shf_ekg) ~ 0,
      shf_qrs >= 150 & shf_lbbb == "Yes" &
        shf_ekg == "Sinus" ~ 1,
      TRUE ~ 0
    ),
    qi4_5crt = case_when(
      is.na(shf_device) | avqi_crt == 0 ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1,
      TRUE ~ 0
    ),
    avqi_crt_sens = case_when(
      avqi_ef40 == 0 ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1,
      is.na(shf_qrs) | is.na(shf_lbbb) ~ 0,
      shf_qrs >= 150 & shf_lbbb == "Yes" ~ 1,
      TRUE ~ 0
    ),
    qi4_5crt_sens = case_when(
      is.na(shf_device) | avqi_crt_sens == 0 ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1,
      TRUE ~ 0
    ),
    avqi_icd = case_when(
      is.na(shf_ef) | shf_ef != "<30" ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      is.na(shf_nyha) | !shf_nyha %in% c("II", "III") ~ 0,
      TRUE ~ 1
    ),
    qi4_6icd = case_when(
      is.na(shf_device) | avqi_icd == 0 ~ NA_real_,
      shf_device %in% c("ICD", "CRT & ICD") ~ 1,
      TRUE ~ 0
    ),
    avqi_icd_sens = case_when(
      avqi_ef40 == 0 ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      TRUE ~ 1
    ),
    qi4_6icd_sens = case_when(
      is.na(shf_device) | avqi_icd_sens == 0 ~ NA_real_,
      shf_device %in% c("ICD", "CRT & ICD") ~ 1,
      TRUE ~ 0
    ),
    qi5_1qol = if_else(!is.na(shf_qol), 1, 0)
  )

rsdata <- rsdata %>%
  mutate(compqi_opbased1_num = rowSums(select(
    ., qi2_1ef, qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow
  ), na.rm = T)) %>%
  mutate(
    compqi_opbased1_den =
      rowSums(!is.na(select(
        ., qi2_1ef, qi2_2ecg,
        qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow
      )))
  ) %>%
  mutate(compqi_opbased2_num = rowSums(select(
    ., qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow,
    qi3_1bbl, qi3_2rasarni, qi3_3mra, qi4_5crt, qi4_6icd
  ), na.rm = T)) %>%
  mutate(compqi_opbased2_den = rowSums(!is.na(select(
    ., qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow,
    qi3_1bbl, qi3_2rasarni, qi3_3mra, qi4_5crt, qi4_6icd
  )))) %>%
  mutate(compqi_opbased2_num_sens = rowSums(select(
    ., qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow,
    qi3_1bbl, qi3_2rasarni, qi3_3mra, qi4_5crt_sens, qi4_6icd_sens
  ), na.rm = T)) %>%
  mutate(compqi_opbased2_den_sens = rowSums(!is.na(select(
    ., qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow,
    qi3_1bbl, qi3_2rasarni, qi3_3mra, qi4_5crt_sens, qi4_6icd_sens
  )))) %>%
  mutate(compqi_all_num = rowSums(select(
    ., qi3_1bbl, qi3_2rasarni, qi3_3mra
  ))) %>%
  mutate(
    comp_qi_opbased1 = compqi_opbased1_num / compqi_opbased1_den,
    comp_qi_opbased1_cat = factor(case_when(
      comp_qi_opbased1 < .7 ~ 1,
      comp_qi_opbased1 < .8 ~ 2,
      comp_qi_opbased1 < .9 ~ 3,
      comp_qi_opbased1 >= .9 ~ 4
    ),
    levels = 1:4, labels = c("ob1 <70%", "ob1 70-<80%", "ob1 80-<90", "ob1 90-100%")
    ),
    comp_qi_opbased2 = if_else(avqi_ef40 == 1,
      compqi_opbased2_num / compqi_opbased2_den,
      NA_real_
    ),
    comp_qi_opbased2_cat = factor(case_when(
      comp_qi_opbased2 < .7 ~ 1,
      comp_qi_opbased2 < .8 ~ 2,
      comp_qi_opbased2 < .9 ~ 3,
      comp_qi_opbased2 >= .9 ~ 4
    ),
    levels = 1:4, labels = c("ob2 <70%", "ob2 70-<80%", "ob2 80-<90", "ob2 90-100%")
    ),
    comp_qi_opbased2_sens = if_else(avqi_ef40 == 1,
      compqi_opbased2_num_sens / compqi_opbased2_den_sens,
      NA_real_
    ),
    comp_qi_opbased2_cat_sens = factor(case_when(
      comp_qi_opbased2_sens < .7 ~ 1,
      comp_qi_opbased2_sens < .8 ~ 2,
      comp_qi_opbased2_sens < .9 ~ 3,
      comp_qi_opbased2_sens >= .9 ~ 4
    ),
    levels = 1:4, labels = c("ob2sens <70%", "ob2sens 70-<80%", "ob2sens 80-<90", "ob2sens 90-100%")
    ),
    comp_qi_all = factor(if_else(avqi_ef40 == 1,
      floor(compqi_all_num / 3), NA_real_
    ),
    levels = 0:1, labels = c("No", "Yes")
    )
  )

rsdata <- rsdata %>%
  mutate_at(vars(starts_with("qi")), ynfac)

qivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^qi\\d")]
compqivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^comp_qi")]
