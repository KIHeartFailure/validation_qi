
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 3)
dataass <- mice::complete(imp, 6)

# check assumptions for cox models ----------------------------------------

qivarsuse <- qivarsmeta %>%
  filter(qivartype %in% c("qi", "compqicat"))

for (i in seq_along(qivarsuse$qivar)) {
  qi <- qivarsuse$qivar[i]
  modvarsuse <- modvars[!modvars %in% str_split(qivarsmeta %>% filter(qivar == qi) %>% pull(noadjvars), ", ")[[1]]]
  mod <- coxph(formula(paste0(
    "Surv(sos_outtime_death, sos_out_death == 'Yes') ~ ", qi, " + ", paste(modvarsuse, collapse = " + ")
  )), data = dataass)

  testpat <- cox.zph(mod)
  print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

  x11()
  plot(testpat[1], resid = F, ylim = c(-4, 4), main = qi)
}
