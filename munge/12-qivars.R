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
    sens_avqi_crt = case_when(
      avqi_ef40 == 0 ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      shf_device %in% c("CRT", "CRT & ICD") ~ 1,
      is.na(shf_qrs) | is.na(shf_lbbb) ~ 0,
      shf_qrs >= 150 & shf_lbbb == "Yes" ~ 1,
      TRUE ~ 0
    ),
    sens_qi4_5crt = case_when(
      is.na(shf_device) | sens_avqi_crt == 0 ~ NA_real_,
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
    sens_avqi_icd = case_when(
      avqi_ef40 == 0 ~ 0,
      is.na(shf_durationhf) | shf_durationhf != ">6mo" ~ 0,
      TRUE ~ 1
    ),
    sens_qi4_6icd = case_when(
      is.na(shf_device) | sens_avqi_icd == 0 ~ NA_real_,
      shf_device %in% c("ICD", "CRT & ICD") ~ 1,
      TRUE ~ 0
    ),
    qi5_1qol = if_else(!is.na(shf_qol), 1, 0)
  )

rsdata <- rsdata %>%
  mutate(compqi_opbased1_num = rowSums(select(
    ., qi2_2ecg, qi2_3np, qi2_4lab, qi2_5rehab, qi2_6follow
  ), na.rm = T)) %>%
  mutate(
    compqi_opbased1_den =
      rowSums(!is.na(select(
        ., qi2_2ecg,
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
  mutate(compqi_all_num = rowSums(select(
    ., qi3_1bbl, qi3_2rasarni, qi3_3mra
  ))) %>%
  mutate(
    comp_qi_opbased1 = if_else(shf_ef_cat == ">=40%" & !is.na(shf_ef_cat),
      compqi_opbased1_num / compqi_opbased1_den * 100,
      NA_real_
    ),
    comp_qi_opbased1_cat = qifac(case_when(
      comp_qi_opbased1 < 50 ~ 0,
      comp_qi_opbased1 >= 50 ~ 1
    ), type = "ob"),
    comp_qi_opbased2 = if_else(avqi_ef40 == 1,
      compqi_opbased2_num / compqi_opbased2_den * 100,
      NA_real_
    ),
    comp_qi_opbased2_cat = qifac(case_when(
      comp_qi_opbased2 < 50 ~ 0,
      comp_qi_opbased2 >= 50 ~ 1
    ), type = "ob"),
    comp_qi_all = qifac(if_else(avqi_ef40 == 1,
      floor(compqi_all_num / 3), NA_real_
    ), type = "a")
  ) %>%
  select(-ends_with("_den"), -ends_with("_num"))

rsdata <- rsdata %>%
  mutate_at(vars(starts_with("qi")), qifac) %>%
  mutate_at(vars(starts_with("sens_qi")), qifac)

qivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^(qi\\d|comp_qi|sens_qi)")]

qivarsmeta <- tibble(
  qivar = qivars
)

qivarsmeta <- qivarsmeta %>%
  mutate(
    qishortname = case_when(
      qivar == "qi2_1ef" ~ "HF type documented",
      qivar == "qi2_2ecg" ~ "ECG documented",
      qivar == "qi2_3np" ~ "NPs measured",
      qivar == "qi2_4lab" ~ "Blood tests documented",
      qivar == "qi2_5rehab" ~ "Cardiac rehabilitation",
      qivar == "qi2_6follow" ~ "Follow-up review",
      qivar == "qi3_1bbl" ~ "Beta-blocker",
      qivar == "qi3_2rasarni" ~ "ACEi/ARB/ARNI",
      qivar == "qi3_3mra" ~ "MRA",
      qivar == "qi3_4loop" ~ "Loop diuretics",
      qivar == "qi4_5crt" ~ "CRT",
      qivar == "sens_qi4_5crt" ~ "CRT (sensitivity)",
      qivar == "qi4_6icd" ~ "ICD",
      qivar == "sens_qi4_6icd" ~ "ICD (sensitivity)",
      qivar == "qi5_1qol" ~ "HRQoL assessment",
      qivar == "comp_qi_opbased1" ~ "OB: LVEF >40%",
      qivar == "comp_qi_opbased1_cat" ~ "OB: LVEF >40%",
      qivar == "comp_qi_opbased2" ~ "OB: LVEF <=40%",
      qivar == "comp_qi_opbased2_cat" ~ "OB: LVEF <=40%",
      qivar == "comp_qi_all" ~ "AON: LVEF <=40%",
      TRUE ~ NA_character_
    ),
    qilongname = case_when(
      qivar == "qi2_1ef" ~ "Main (2.1): Documentation of HF type",
      qivar == "qi2_2ecg" ~ "Main (2.2): Documentation of ECG",
      qivar == "qi2_3np" ~ "Main (2.3): NPs measured",
      qivar == "qi2_4lab" ~ "Main (2.4): Blood tests documented",
      qivar == "qi2_5rehab" ~ "Main (2.5): Cardiac rehabilitation referral",
      qivar == "qi2_6follow" ~ "Secondary (2.1): Follow-up review",
      qivar == "qi3_1bbl" ~ "Main (3.1): Beta-blocker",
      qivar == "qi3_2rasarni" ~ "Main (3.2): ACE inhibitor, ARB or ARNI",
      qivar == "qi3_3mra" ~ "Main (3.3): MRA",
      qivar == "qi3_4loop" ~ "Main (3.5): Loop diuretic therapy",
      qivar == "qi4_5crt" ~ "Secondary (4.1): CRT",
      qivar == "sens_qi4_5crt" ~ "Secondary (4.1): CRT (sensitivity)",
      qivar == "qi4_6icd" ~ "Secondary (4.2): ICD",
      qivar == "sens_qi4_6icd" ~ "Secondary (4.2): ICD (sensitivity)",
      qivar == "qi5_1qol" ~ "Secondary (5.1): HRQoL assessment",
      qivar == "comp_qi_opbased1" ~ "Opportunity-based: LVEF >40%",
      qivar == "comp_qi_opbased1_cat" ~ "Opportunity-based above 50%: LVEF >40%",
      qivar == "comp_qi_opbased2" ~ "Opportunity-based: LVEF <=40%",
      qivar == "comp_qi_opbased2_cat" ~ "Opportunity-based above 50%: LVEF <=40%",
      qivar == "comp_qi_all" ~ "All-or-none: LVEF <=40%",
      TRUE ~ NA_character_
    ),
    noadjvars = case_when(
      qivar == "qi2_1ef" ~ "shf_ef",
      qivar == "qi2_3np" ~ "shf_ntprobnp",
      qivar == "qi2_4lab" ~ "shf_anemia, shf_gfrckdepi",
      qivar == "qi2_5rehab" ~ "shf_location",
      qivar == "qi2_6follow" ~ "shf_location",
      qivar == "qi3_1bbl" ~ "shf_ef, shf_bbl",
      qivar == "qi3_2rasarni" ~ "shf_ef, shf_rasarni",
      qivar == "qi3_3mra" ~ "shf_ef, shf_mra",
      qivar == "qi3_4loop" ~ "shf_ef, shf_diuretic",
      qivar %in% c("qi4_5crt", "sens_qi4_5crt") ~ "shf_ef, shf_device, shf_durationhf",
      qivar %in% c("qi4_6icd", "sens_qi4_6icd") ~ "shf_ef, shf_device, shf_durationhf",
      qivar == "comp_qi_all" ~ "shf_ef, shf_bbl, shf_rasarni, shf_mra",
      qivar %in% c("comp_qi_opbased2", "comp_qi_opbased2_cat") ~
      "shf_ef, shf_bbl, shf_rasarni, shf_mra, shf_ntprobnp, shf_anemia,
      shf_gfrckdepi, shf_device",
      qivar %in% c("comp_qi_opbased1", "comp_qi_opbased1_cat") ~
      "shf_ef, shf_ntprobnp, shf_anemia, shf_gfrckdepi",
      TRUE ~ NA_character_
    ),
    qivartype = case_when(
      qivar %in% c("comp_qi_opbased1_cat", "comp_qi_opbased2_cat", "comp_qi_all") ~ "compqicat",
      qivar %in% c("comp_qi_opbased1", "comp_qi_opbased2") ~ "compqicont",
      qivar %in% c("sens_qi4_5crt", "sens_qi4_6icd") ~ "qisens",
      TRUE ~ "qi"
    )
  )
