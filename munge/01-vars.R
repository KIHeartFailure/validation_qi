

# Variables for tabs/mods -------------------------------------------------

tabvars <- c(
  # demo
  "shf_sex",
  "shf_age",
  "shf_age_cat",
  "shf_indexyear",
  "shf_indexyear_cat",
  
  # socec
  "scb_education",
  "scb_child",
  "scb_famtype",
  "scb_dispincome_cat",
  
  "shf_durationhf",
  "shf_location",

  # clinical factors and lab measurments
  "shf_ef_cat",
  "shf_ef",
  "shf_efproc",
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
  "shf_bnp",
  "shf_transferrin",
  "shf_ferritin",
  "shf_qrs", "shf_lbbb", 
  "shf_ekg", 
  
  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_mi",
  "sos_com_pcicabg",
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
  "shf_nitrate", 
  "sos_ddr_sglt2i",

  "shf_qol", 
  "PARTICIPATION_HF_SCHOOL", 
  "PARTICIPATION_HF_TRAINING", 
  "FOLLOWUP_UNIT"
)


tabvars_not_in_mod <- c(
  "shf_sos_com_ihd",
  "shf_age_cat",
  "shf_indexyear_cat", 
  "shf_bpsys",
  "shf_bpdia",
  "shf_hb",
  "shf_creatinine", 
  "shf_efproc", 
  "shf_ef_cat",
  "shf_bnp",
  "shf_transferrin",
  "shf_ferritin",
  "shf_potassium", "shf_sodium", 
  "shf_qrs", "shf_lbbb", 
  "shf_qol", 
  "shf_acei", "shf_arb", "shf_arni",
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
  "shf_nitrate", 
  "sos_ddr_sglt2i",
  "sos_priorhfhosp", 
  "shf_ekg", 
  "PARTICIPATION_HF_SCHOOL", 
  "PARTICIPATION_HF_TRAINING", 
  "FOLLOWUP_UNIT"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]