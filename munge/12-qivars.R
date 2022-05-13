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
    comp_qi_opbased1_aboveequal50 = ynfac(case_when(
      comp_qi_opbased1 < 50 ~ 0,
      comp_qi_opbased1 >= 50 ~ 1
    )),
    comp_qi_opbased2 = if_else(avqi_ef40 == 1,
      compqi_opbased2_num / compqi_opbased2_den * 100,
      NA_real_
    ),
    comp_qi_opbased2_aboveequal50 = ynfac(case_when(
      comp_qi_opbased2 < 50 ~ 0,
      comp_qi_opbased2 >= 50 ~ 1
    )),
    comp_qi_all = ynfac(if_else(avqi_ef40 == 1,
      floor(compqi_all_num / 3), NA_real_
    ))
  ) %>%
  select(-ends_with("_den"), -ends_with("_num"))

rsdata <- rsdata %>%
  mutate_at(vars(starts_with("qi")), ynfac) %>%
  mutate_at(vars(starts_with("sens_qi")), ynfac)

qivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^qi\\d")]
qisensvars <- colnames(rsdata)[str_detect(colnames(rsdata), "^sens_qi\\d")]
compqivars <- colnames(rsdata)[str_detect(colnames(rsdata), "^comp_qi")]
compqicatvars <- c("comp_qi_all", "comp_qi_opbased1_aboveequal50", "comp_qi_opbased2_aboveequal50")

qivarsmeta <- tibble(
  qivar = c(qivars, qisensvars, compqivars),
  qiname = qivar
)

qivarsmeta <- qivarsmeta %>%
  mutate(
    qiname = str_replace(qiname, "sens_", "Sensitivity "),
    qiname = str_replace(qiname, "comp_qi_", ""),
    qiname = str_replace(qiname, "_aboveequal50", ""),
    qiname = str_replace(qiname, "qi\\d_\\d", ""),
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
      qivar %in% c("comp_qi_opbased2", "comp_qi_opbased2_aboveequal50") ~
      "shf_ef, shf_bbl, shf_rasarni, shf_mra, shf_ntprobnp, shf_anemia,
      shf_gfrckdepi, shf_device",
      qivar %in% c("comp_qi_opbased1", "comp_qi_opbased1_aboveequal50") ~
      "shf_ef, shf_ntprobnp, shf_anemia, shf_gfrckdepi",
      TRUE ~ NA_character_
    )
  )
