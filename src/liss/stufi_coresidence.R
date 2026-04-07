library(tidyverse)

PATH_LISS_DATA <- "data/liss/"
PATH_LISS_TMP_DATA <- "tmp/liss/"

# ------------------------------------------------------------
# Load and preprocess the core LISS panel data.
# Constructs harmonized variables used in the analysis:
#   - wave as Date
#   - gender and household position as factors
#   - parental co-residence indicator
#   - student status indicator
# ------------------------------------------------------------
panel <- read_rds(paste0(PATH_LISS_TMP_DATA, "liss_core.Rds")) %>%
  transmute(
    nomem_encr,
    wave = zoo::as.Date(zoo::as.yearmon(as.character(wave), "%Y%m")),
    geslacht = as_factor(geslacht),
    coresidence = case_when(
      positie == 5 ~ 1,                                # living with parents
      positie %in% c(1, 2, 3, 4, 6, 7) ~ 0,             # not living with parents
      positie == 9 ~ NA_real_,                          # missing/unknown
      .default = NA_real_
    ),
    positie = as_factor(positie),
    gebjaar,
    leeftijd,
    nettoink_f,
    student = case_when(
      belbezig == 7 ~ 1,                               # enrolled as student
      .default = 0
    )
  )
