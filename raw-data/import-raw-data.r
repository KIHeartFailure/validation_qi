
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

memory.limit(size = 10000000000000)

# Patient registry from SHFDB3 v 3.2.x, prepared in 08-prep_sosdata.R -----

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))


load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_rs.RData")
save(file = "./data/rawData_rs.RData", list = c("rawData_rs"))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_enheter.RData")
save(file = "./data/rawData_enheter.RData", list = c("rawData_enheter"))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_scb.RData")
save(file = "./data/rawData_scb.RData", list = c("rawData_scb"))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_sosdors.RData")
save(file = "./data/rawData_sosdors.RData", list = c("rawData_sosdors"))

