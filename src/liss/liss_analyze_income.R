library(tidyverse)
library(haven)
library(tictoc)
library(stargazer)
library(modelsummary)
library(gt)
library(kableExtra)
library(plotly)

PATH_LISS_DATA <- "data/liss/"
PATH_LISS_TMP_DATA <- "tmp/liss/"

SELECTED_BIRTH_YEARS = c(1989)
EDUC_LABELS = c(NA, "basisonderwijs", "vmbo", "havo/vwo", "mbo", "hbo", "wo")

# Check for tmp files 

if (!file.exists(paste0(PATH_LISS_DATA, "liss_core.Rds"))) {
  stop("liss_core.Rds tmp file does not exist, run liss_construct_files.R")
}

# 55 seconds
panel <- readRDS(paste0("PATH_LISS_TMP_DATA", "liss_core.Rds")) %>% 
  transmute(
    nomem_encr,
    wave = zoo::as.Date(zoo::as.yearmon(as.character(wave), "%Y%m")),
    geslacht = as_factor(geslacht),
    leeftijd,
    gebjaar = case_when(
      gebjaar == 1 ~ NA,
      .default = gebjaar
    ),
    nettoink_f = as.numeric(nettoink_f),
    oplcat = case_when(
      oplcat == 1 ~ "basisonderwijs",
      oplcat == 2 ~ "vmbo",
      oplcat == 3 ~ "havo/vwo",
      oplcat == 4 ~ "mbo",
      oplcat == 5 ~ "hbo",
      oplcat == 6 ~ "wo"
    )
  )

highest_education <- panel %>% 
  filter(gebjaar %in% SELECTED_BIRTH_YEARS) %>% 
  group_by(nomem_encr) %>% 
  summarize(oplcat = EDUC_LABELS[match(TRUE, EDUC_LABELS %in% oplcat)]) %>% 
  ungroup()

educ_per_cohort <- panel %>% 
  mutate(decade = round(gebjaar / 10) * 10) %>% 
  group_by(decade, nomem_encr) %>% 
  summarize(oplcat = EDUC_LABELS[match(TRUE, EDUC_LABELS %in% oplcat)]) %>%
  count(oplcat) %>% 
  mutate(perc = n / sum(n)) %>% 
  ungroup() %>%
  select(-n) %>% 
  pivot_wider(names_from = oplcat, values_from = perc, values_fill = 0, names_sort = TRUE)

gt::gt(educ_per_cohort) %>% 
  gt::fmt_number(decimals = 0, sep_mark = "") %>% 
  gt::fmt_percent(columns = -1, decimals = 0) %>% 
  gt::tab_header("Education levels by birthyear decade")

respondents_start <- panel %>% 
  filter(gebjaar == 1982) %>%
  filter(wave == as.Date("2008-01-01")) %>% 
  pull(nomem_encr)

respondents_end <- panel %>% 
  filter(gebjaar == 1982) %>%
  filter(wave == as.Date("2025-07-01")) %>% 
  pull(nomem_encr)

respondents_both <- intersect(respondents_start, respondents_end)

cohort <- panel %>% 
  filter(nomem_encr %in% respondents_both) %>% 
  pull(nomem_encr) %>% 
  unique()

panel %>% 
  filter(nomem_encr %in% sample(cohort, 16)) %>% 
  ggplot(aes(x = wave, y = nettoink_f)) +
  geom_line() + 
  theme_minimal() +
  facet_wrap(~nomem_encr)

average_income_growth <- panel %>% 
  filter(gebjaar %in% SELECTED_BIRTH_YEARS) %>% 
  select(-oplcat) %>% 
  left_join(highest_education) %>% 
  group_by(wave, oplcat) %>% 
  summarize(
    nettoink = median(nettoink, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>% 
  mutate(quarter = zoo::as.yearqtr(wave)) %>% 
  group_by(quarter, oplcat) %>% 
  summarize(
    nettoink = mean(nettoink, na.rm = TRUE),
    n = sum(n)
  ) %>% 
  ungroup() 

ggplot(average_income_growth, aes(x = quarter, y = nettoink, color = oplcat, group = oplcat)) +
  geom_line(linewidth = 1) + 
  theme_minimal() #+
  labs(
    title = paste0("Income growth of people born in ", SELECTED_BIRTH_YEARS, " by level of education (source: LISS Panel)"),
    x = "Year",
    y = "Net income (Euro)"
  )


  

# ggplot(average_income_growth, aes(x = quarter)) +
#   geom_line(aes(y = nettoink_hbo, color = "HBO"), size = 1) +
#   geom_line(aes(y = nettoink_wo, color = "WO"), size = 1) +
#   geom_line(aes(y = nettoink_mbo, color = "MBO"), size = 1) +
#   scale_color_manual(name = "Highest education in 2025", 
#                      breaks = c("WO", "HBO", "MBO"),
#                      values = c("HBO" = "#009E73", "WO" = "#0072B2", "MBO" = "#E69F00")) +
#   theme_minimal() +
#   labs(
#     title = paste0("Income growth of people born in ", SELECTED_BIRTH_YEARS, " by level of education (source: LISS Panel)"),
#     x = "Year",
#     y = "Net income (Euro)"
#   )

gender_inequality <- panel %>% 
  filter(gebjaar %in% SELECTED_BIRTH_YEARS) %>% 
  group_by(wave, geslacht) %>% 
  summarize(
    nettoink = median(nettoink, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>% 
  mutate(quarter = zoo::as.yearqtr(wave)) %>% 
  group_by(quarter, geslacht) %>% 
  summarize(
    nettoink = mean(nettoink, na.rm = TRUE),
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = geslacht, values_from = c(nettoink, n))

ggplot(gender_inequality %>% filter(year(quarter) >= 2020), aes(x = quarter)) +
  geom_line(aes(y = nettoink_Male, color = "Male"), size = 1) +
  geom_line(aes(y = nettoink_Female, color = "Female"), size = 1) +
  # geom_line(aes(y = nettoink_Other, color = "Other"), size = 1) +
  scale_color_manual(name = "Gender", 
                     breaks = c("Female", "Male", "Other"),
                     values = c("Female" = "#009E73", "Male" = "#0072B2", "Other" = "#E69F00")) +
  theme_minimal() +
  labs(
    title = paste0("Income growth of people born in ", SELECTED_BIRTH_YEARS, " by gender (source: LISS Panel)"),
    x = "Year",
    y = "Net income (Euro)"
  )