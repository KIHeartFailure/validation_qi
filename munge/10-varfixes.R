rsdata <- rsdata %>%
  mutate(
    
    shf_sex = relevel(factor(shf_sex), ref = "Male"),
    
    shf_indexyear_cat = case_when(
      shf_indexyear >= 2013 & shf_indexyear <= 2013 ~ "2013-2016",
      shf_indexyear <= 2019 ~ "2017-2019"
    ),
    shf_age_cat = factor(case_when(
      shf_age < 70 ~ 1,
      shf_age >= 70 ~ 2
    ),
    labels = c("<70", ">=70"),
    levels = 1:2
    ),

    # Anemia
    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),
    shf_ef_cat = factor(case_when(
      shf_ef %in% c(">=50", "40-49") ~ 1,
      shf_ef %in% c("30-39", "<30") ~ 2
    ),
    labels = c(">40%", "<=40%"),
    levels = 1:2
    ),
    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 0,
      shf_smoking %in% c("Current") ~ 1
    ),
    labels = c("No", "Current smoker"),
    levels = 0:1
    ),
    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pcicabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    sos_priorhfhosp = factor(case_when(
      is.na(sos_comdur_hosphf) ~ 4,
      sos_comdur_hosphf < 365 / 2 ~ 1,
      sos_comdur_hosphf < 365 ~ 2,
      sos_comdur_hosphf >= 365 ~ 3,
    ),
    levels = 1:4,
    labels = c("Prior HFH < 6 mo", "Prior HFH 6-12 mo", "Prior HFH > 12 mo", "No prior HFH")
    ), 
    
    sos_out_hosphf_cr = create_crevent(sos_out_hosphf, sos_out_death)
  )

rsdata <- cut_surv(rsdata, sos_out_hosphf, sos_outtime_hosphf, floor(30), rename = "30d", cuttime = TRUE)
rsdata <- cut_surv(rsdata, sos_out_hosphf, sos_outtime_hosphf, floor(30.5 * 6), rename = "6mo", cuttime = TRUE)
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, floor(30), rename = "30d", cuttime = TRUE)
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, floor(30.5 * 6), rename = "6mo", cuttime = TRUE)