
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

memory.limit(size = 10000000000000)

# Patient registry from SHFDB3 v 3.2.x, prepared in 08-prep_sosdata.R -----

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/patreg.RData")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))


load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_rs.RData")
save(file = "./data/rawData_rs.RData", list = c(
  "newrs",
  "oldrs"
))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_enheter.RData")
save(file = "./data/rawData_enheter.RData", list = c("enheternewrs", "enheteroldrs"))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_scb.RData")
save(file = "./data/rawData_scb.RData", list = c(
  "lisa",
  "antalbarn",
  "pnr_bytt_ater",
  "migration"
))

load(file = "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/data/rawData_sosdors.RData")
save(file = "./data/rawData_sosdors.RData", list = c("dors", "dors2"))

# Import LM from SoS -----------------------------------------------------

sospath <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/raw-data/SOS/lev3_15875_2019 Lina Benson/"

load(paste0(sospath, "RData/lm.RData"))

# Select ATC codes --------------------------------------------------------

lmsel <- lm %>%
  mutate(atcneed = stringr::str_detect(ATC, "^A10B")) %>%
  filter(
    ANTAL >= 0,
    # AR >= 2013,
    # AR <= 2018,
    atcneed
  )

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lmsel.RData", list = c("lmsel"))
