rsdata <- rsdata %>%
  mutate(
    shf_sex = relevel(factor(shf_sex), ref = "Male"),
    shf_age_cat = factor(case_when(
      shf_age < 75 ~ 1,
      shf_age >= 75 ~ 2
    ),
    labels = c("<75", ">=75"),
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
    labels = c(">=40%", "<40%"),
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
    shf_qrsexclpaced = factor(case_when(
      shf_ekg == "PM/Other" | is.na(shf_qrs) ~ NA_real_,
      shf_qrs < 150 ~ 1,
      shf_qrs >= 150 ~ 2
    ),
    levels = 1:2,
    labels = c("<150", "=>150")
    ),

    # outcome
    sos_out_deathcvhosphf = case_when(
      sos_out_deathcv == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  )

rsdata <- cut_surv(rsdata, sos_out_deathcvhosphf, sos_outtime_hosphf, floor(365), rename = "1yr", cuttime = FALSE)
rsdata <- cut_surv(rsdata, sos_out_hosphf, sos_outtime_hosphf, floor(365), rename = "1yr", cuttime = TRUE)
rsdata <- cut_surv(rsdata, sos_out_deathcv, sos_outtime_death, floor(365), rename = "1yr", cuttime = FALSE)
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, floor(365), rename = "1yr", cuttime = TRUE)


rsdata <- rsdata %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    shf_centre = as.character(shf_centre)
  )
