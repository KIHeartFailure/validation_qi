

# Variables for tabs/mods -------------------------------------------------

tabvars <- c(
  # demo
  "shf_sex",
  "shf_age",
  "shf_age_cat",
  "shf_indexyear_cat",
  "shf_durationhf",
  "sos_priorhfhosp",
  "shf_location",

  # clinical factors and lab measurments
  "shf_ef_cat",
  "shf_nyha",
  "shf_map",
  "shf_bpsys",
  "shf_bpdia",
  "shf_heartrate",
  "shf_bmi",
  "shf_hb",
  "shf_potassium",
  "shf_sodium",
  "shf_creatinine",
  "shf_gfrckdepi",
  "shf_ntprobnp",

  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_stroketia",
  "shf_sos_com_af",
  "sos_com_peripheralartery",
  "sos_com_copd",
  "sos_com_renal",
  "sos_com_liver",
  "shf_anemia",
  "sos_com_cancer3y",

  # treatments
  "shf_acei",
  "shf_arb",
  "shf_arni",
  "shf_rasarni",
  "shf_bbl",
  "shf_mra",
  "shf_diuretic",
  "shf_loopdiuretic",
  "shf_device",
  "shf_digoxin",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_nitrate"
)


tabvars_not_in_mod <- c(
  "shf_age_cat",
  "shf_indexyear_cat", 
  "shf_bpsys",
  "shf_bpdia",
  "shf_hb",
  "shf_creatinine"
)

impmodvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

modvars <- c(
  "shf_sex",
  "shf_age",
  "shf_indexyear",
  "shf_durationhf",
  "shf_location",
  
  # clinical factors and lab measurments
  "shf_ef_cat",
  "shf_nyha",
  
  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_stroketia",
  "shf_sos_com_af",
  "sos_com_peripheralartery",
  "sos_com_copd",
  "sos_com_renal",
  "sos_com_liver",
  "shf_anemia",
  "sos_com_cancer3y"
)
