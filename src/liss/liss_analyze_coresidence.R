library(tidyverse)

PATH_LISS_DATA <- "data/liss/"
PATH_LISS_TMP_DATA <- "tmp/liss/"

# ------------------------------------------------------------
# Return the most common (modal) value in a vector.
# Optionally removes missing values before computing the mode.
# ------------------------------------------------------------
most_common <- function(x, na.rm = TRUE) {
  if (na.rm)
    x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ------------------------------------------------------------
# Identify respondents who are observed in all specified waves
# and belong to a given birth cohort.
#
# This is used to construct balanced panels around key
# educational transition periods.
# ------------------------------------------------------------
find_respondents_in_waves <- function(panel, gebjaar, waves) {
  # TODO: Check continous appearance in waves
  return(
    panel %>% 
      # Restrict to a specific birth year
      filter(gebjaar == .env$gebjaar) %>% 
      # Keep only the waves of interest
      filter(wave %in% as.Date(waves)) %>% 
      # One record per person-wave
      distinct(nomem_encr, wave) %>% 
      # Count how many of the required waves are observed
      count(nomem_encr) %>% 
      # Keep respondents observed in all waves
      filter(n == length(waves)) %>% 
      # Return respondent identifiers
      pull(nomem_encr)
  )
}

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

START_PRE_REFORM <- "2014-09-01"
END_PRE_REFORM <- "2023-09-01"

# ------------------------------------------------------------
# Identify students from the pre-reform cohort:
# born in 1997 and observed at the start and end of the
# relevant study period.
# ------------------------------------------------------------
respondents_pre <- find_respondents_in_waves(
  panel,
  gebjaar = 1997,
  waves = c(START_PRE_REFORM, END_PRE_REFORM)
)

START_POST_REFORM <- "2015-09-01"
END_POST_REFORM <- "2024-09-01"

# ------------------------------------------------------------
# Identify students from the post-reform cohort:
# born in 1998 and observed at the start and end of the
# relevant study period.
# ------------------------------------------------------------
respondents_post <- find_respondents_in_waves(
  panel,
  gebjaar = 1998,
  waves = c(START_POST_REFORM, END_POST_REFORM)
)

# ------------------------------------------------------------
# Construct indicators for highest completed education.
# Used later for stratification or descriptive checks.
# ------------------------------------------------------------
highest_educ <- read_rds(paste0(PATH_LISS_TMP_DATA, "liss_highest_educ.Rds")) %>%
  group_by(nomem_encr) %>%
  summarize(
    wo  = any(highest_educ == 26),                     # university degree
    hbo = any(highest_educ == 20) & !any(highest_educ == 26)
  )

# ------------------------------------------------------------
# Build analysis dataset:
#   - Assign respondents to pre/post cohorts
#   - Construct time relative to study start (in months)
#   - Keep a balanced window around study entry
# ------------------------------------------------------------
df <- panel %>% 
  mutate(
    group = case_when(
      nomem_encr %in% respondents_pre  ~ 0,            # pre-reform cohort
      nomem_encr %in% respondents_post ~ 1             # post-reform cohort
    ),
    month_relative = case_when(
      group == 0 ~ 12 * (year(wave) - 2014) + (month(wave) - 9),
      group == 1 ~ 12 * (year(wave) - 2015) + (month(wave) - 9),
      .default = NA_real_
    )
  ) %>% 
  # Keep only treated cohorts and the desired time window
  filter(group %in% c(0, 1)) %>% 
  filter(month_relative >= 0 & month_relative <= 130) %>% 
  select(-wave) %>% 
  transmute(
    nomem_encr,
    month_relative,
    group,
    student,
    coresidence,
    income = nettoink_f
  ) %>% 
  arrange(nomem_encr, month_relative)

# ------------------------------------------------------------
# Plot individual income trajectories for random respondents
# as a sanity check on panel construction.
# ------------------------------------------------------------
df %>% 
  filter(nomem_encr %in% sample(df$nomem_encr, 16)) %>% 
  ggplot(aes(x = month_relative, y = income)) +
  geom_line() + 
  theme_minimal() +
  facet_wrap(~nomem_encr)

# ------------------------------------------------------------
# Compute average outcomes by cohort and relative month.
# ------------------------------------------------------------
average_paths <- 
  df %>% 
  group_by(month_relative, group) %>% 
  summarize(
    income_mean = mean(income, na.rm = TRUE),
    coresidence = mean(coresidence, na.rm = TRUE),
    student     = mean(student, na.rm = TRUE)
  ) %>% 
  ungroup() 

# ------------------------------------------------------------
# Plot average income trajectories by cohort.
# ------------------------------------------------------------
average_paths %>% 
  ggplot(aes(x = month_relative, y = income_mean,
             group = group, color = group)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Income development before and after the study loan reform"
  )

# ------------------------------------------------------------
# Plot average parental co-residence rates by cohort.
# ------------------------------------------------------------
average_paths %>% 
  ggplot(aes(x = month_relative, y = coresidence,
             group = group, color = as.factor(group))) +
  geom_line(size = 1.3) +
  theme_minimal()

# ------------------------------------------------------------
# Plot average student enrollment rates by cohort.
# ------------------------------------------------------------
average_paths %>% 
  ggplot(aes(x = month_relative, y = student,
             group = group, color = as.factor(group))) +
  geom_line(size = 1.3) +
  theme_minimal()
