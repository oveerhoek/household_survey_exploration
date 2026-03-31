library(tidyverse)
library(haven)
library(stargazer)
library(naniar)
library(modelsummary)

theme_set(theme_minimal())

SOEP_PATH <- "data/soep/SOEP-CORE.v40.1eu_Stata/Stata_DE/soepdata/"

pgen <- haven::read_dta(paste0(SOEP_PATH, "pgen.dta"))
pequiv <- haven::read_dta(paste0(SOEP_PATH, "pequiv.dta"))
bioparen <- haven::read_dta(paste0(SOEP_PATH, "bioparen.dta"))
pbrutto <- haven::read_dta(paste0(SOEP_PATH, "pbrutto.dta"))
bioedu <- haven::read_dta(paste0(SOEP_PATH, "bioedu.dta"))
ppathl <- haven::read_dta(paste0(SOEP_PATH, "ppathl.dta"))

age <- pequiv %>%
  select(pid, syear, d11101) %>%
  transmute(
    pid,
    syear,
    age = case_when(
      d11101 < 0 ~ NA,
      .default = d11101
    )
  )

# age %>%
#   group_by(syear) %>%
#   summarize(
#     n = n(),
#     n_missing = sum(is.na(age)),
#     age_min = min(age, na.rm = TRUE),
#     age_max = max(age, na.rm = TRUE),
#     age_median = median(age, na.rm = TRUE),
#     age_mean = mean(age, na.rm = TRUE),
#     age_sd = sd(age, na.rm = TRUE)
#   ) %>%
#   knitr::kable()

income <- pequiv %>%
  transmute(
    pid,
    syear,
    income = case_when(
      i11110 < 0 ~ NA,
      .default = log(i11110 + 1)
    )
  )

# income %>%
#   group_by(syear) %>%
#   summarize(
#     n = n(),
#     n_missing = sum(is.na(income)),
#     income_min = min(income, na.rm = TRUE),
#     income_max = max(income, na.rm = TRUE),
#     income_median = median(income, na.rm = TRUE),
#     income_mean = mean(income, na.rm = TRUE),
#     income_sd = sd(income, na.rm = TRUE)
#   ) %>%
#   knitr::kable()

grant <- pequiv %>%
  transmute(
    pid,
    syear,
    grant = case_when(
      istuy < 0 ~ NA,
      .default = istuy
    )
  )

# grant %>%
#   filter(grant > 0) %>%
#   group_by(syear) %>%
#   summarize(grant = mean(grant, na.rm = TRUE)) %>%
#   ungroup() %>%
# 
#   ggplot(aes(x = syear, y = grant)) +
#   geom_line() +
#   labs(
#     title = "Grant of people who received more than 0 EUR",
#     x = "Year",
#     y = "Average grant received"
#   ) +
#   theme_minimal()

coresidence <- pbrutto %>%
  transmute(
    pid,
    syear,
    smonth = case_when(
      imonth < 0 ~ NA,
      .default = imonth
    ),
    coresidence = case_when(
      stell_h %in% c(20, 21) ~ 1,
      stell_h == 99 ~ NA,
      stell_h < 0 ~ NA,
      .default = 0
    )
  )

# coresidence %>%
#   group_by(syear) %>%
#   summarize(
#     coresidence_perc = mean(coresidence, na.rm = TRUE) * 100
#   ) %>%
#   ungroup() %>%
# 
#   ggplot(aes(x = syear, y = coresidence_perc)) +
#   geom_line() +
#   labs(
#     x = "Year",
#     y = "Percentage parental coresidence"
#   ) +
#   theme_minimal()

student <- pgen %>%
  transmute(
    pid,
    syear,
    student = case_when(
      pgstib == 11 ~ 1,
      pgstib < 0 ~ NA,
      .default = 0
    )
  )

# student %>%
#   group_by(syear) %>%
#   summarize(
#     student_perc = mean(student, na.rm = TRUE) * 100
#   ) %>%
#   ungroup() %>%
# 
#   ggplot(aes(x = syear, y = student_perc)) +
#   geom_line() +
#   labs(
#     x = "Year",
#     y = "Percentage student enrolment"
#   ) +
#   theme_minimal()

parents <- bioparen %>%
  transmute(
    pid,
    mnr1,
    fnr1
  )

parental_income <- pgen %>%
  transmute(
    pid, syear
  ) %>%
  left_join(parents, by = "pid") %>%

  # mother income
  left_join(
    income %>% rename(mnr1 = pid, income_mother = income),
    by = c("mnr1", "syear")
  ) %>%

  # father income
  left_join(
    income %>% rename(fnr1 = pid, income_father = income),
    by = c("fnr1", "syear")
  ) %>%

  mutate(
    parental_income = coalesce(income_mother, 0) + coalesce(income_father, 0)
  )

parental_income <- parental_income %>%
  transmute(
    pid,
    syear,
    parental_income
  )

educ <- pequiv %>%
  select(pid, syear, d11109) %>%
  transmute(
    pid,
    syear,
    educ = case_when(
      d11109 < 0 ~ NA,
      .default = as.numeric(d11109)
    )
  )

df <-
  full_join(age, coresidence) %>%
  full_join(grant) %>%
  full_join(income) %>%
  full_join(student) %>%
  full_join(parental_income) %>%
  full_join(educ) %>%
  transmute(
    pid = as.numeric(pid),
    syear = as.numeric(syear),
    smonth = as.numeric(smonth),
    income = as.numeric(income), # Y: outcome variable
    coresidence = as.numeric(coresidence), # X: independent variable
    grant = as.numeric(grant), # IV
    student = as.numeric(student),
    age = as.numeric(age),
    parental_income = as.numeric(parental_income),
    educ = as.numeric(educ)
  )

write_csv(df, "tmp/soep/bafög_coresidence.csv")

# any(duplicated(df))
# any(duplicated(df %>% select(pid, syear)))

# df %>%
#   group_by(syear) %>%
#   summarize(
#     missing_income = sum(is.na(income)) / n() * 100,
#     missing_coresidence = sum(is.na(coresidence)) / n() * 100,
#     missing_grant = sum(is.na(grant)) / n() * 100,
#     missing_student = sum(is.na(student)) / n() * 100,
#     missing_age = sum(is.na(age)) / n() * 100,
#     missing_income_parent = sum(is.na(parental_income)) / n() * 100,
#     missing_educ = sum(is.na(educ)) / n() * 100,
#   ) %>%
#   ungroup() %>%
#   knitr::kable()


# students_wide <- df %>%
#   select(pid, syear, student) %>%
#   filter(syear %in% 2014:2018) %>%
#   pivot_wider(names_from = syear, values_from = student, names_prefix = "student_")
# 
# students_pre_reform <- students_wide %>%
#   filter(student_2014 == 0 & student_2015 == 1) %>%
#   pull(pid)
# 
# students_post_reform <- students_wide %>%
#   filter(student_2015 == 0 & student_2016 == 1) %>%
#   pull(pid)

# df %>%
#   filter(pid %in% students_pre_reform) %>%
#   filter(syear %in% 2015:2020) %>%
#   group_by(syear) %>%
#   summarize(n = n(), across(income:age, \(x) mean(x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   knitr::kable()

# naniar::gg_miss_var(df)

# df %>%
#   filter(grant > 0 & syear == 2015) %>%
#   n_distinct()

# Take age for simplicity
# Calculate parental coresidence in "college years" (18-22)

# coresidence_proportion <- df %>%
#   filter(age >= 18 & age <= 22) %>%
#   group_by(pid) %>%
#   summarize(coresidence_proportion = mean(coresidence, na.rm = TRUE)) %>%
#   ungroup()

# hist(coresidence_proportion$coresidence_proportion)

# later_income <- df %>%
#   # Calcaulate income percentile to compare years
#   group_by(syear) %>%
#   mutate(income_p = percent_rank(income)) %>%
#   ungroup() %>%
#   # Calculate later earnings as average over ages 28-32
#   filter(age >= 28 & age <= 32) %>%
#   group_by(pid) %>%
#   summarize(later_income = mean(income_p, na.rm = TRUE)) %>%
#   ungroup()

# hist(later_income$later_income)

# association_df <- full_join(coresidence_proportion, later_income)

# naniar::gg_miss_var(association_df)

# ggplot(association_df, aes(x = coresidence_proportion, y = later_income)) +
#   geom_point() +
#   geom_smooth()

# df_did <- df %>%
#   filter(syear %in% c(2014, 2018)) %>%
#   filter(student == 1)


# cor(df_did$grant, df_did$parental_income, use = "complete.obs")

# ggplot(df_did, aes(parental_income, grant)) +
#   geom_smooth(method = "loess") +
#   labs(
#     x = "Parental Income (€)",
#     y = "Average amount of BAföG Received"
#   ) +
#   theme_minimal() +
#   xlim(0, 100000)
# 
# 
# df_did <- df_did %>%
#   transmute(
#     y = coresidence,
#     treated = ifelse(parental_income < 25000, 1, 0),
#     post = ifelse(syear == 2018, 1, 0)
#   )
# 
# did <- lm(y ~ treated * post, data = df_did)
# stargazer(did, type = "text")
# 
# df %>%
#   group_by(age) %>%
#   summarize(
#     n = n(),
#     coresidence = mean(coresidence, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
# 
#   ggplot(aes(x = age, y = coresidence)) +
#   geom_line() +
#   labs(
#     x = "Age (years)",
#     y = "Proportion of people living with their parents"
#   ) +
#   scale_y_continuous(labels = scales::percent)
# 
# df %>%
#   filter(age >= 18 & age <= 25) %>%
#   group_by(syear) %>%
#   summarize(
#     coresidence = mean(coresidence, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
# 
#   ggplot(aes(x = syear, y = coresidence)) +
#   geom_line() +
#   labs(
#     x = "Age (years)",
#     title = "Proportion of people aged 18-25 living with their parents, by year"
#   ) +
#   scale_y_continuous(labels = scales::percent)
# 
# df %>%
#   filter(age == 30) %>%
#   group_by(syear) %>%
#   summarize(
#     n = n(),
#     income = mean(income, na.rm = TRUE),
#     income = sd(income, na.rm = TRUE)
#   )
# 
# late_variables <- df %>%
#   filter(age == 30) %>%
#   select(pid, income, educ) %>%
#   transmute(
#     pid,
#     w = income,
#     S = educ
#   )
# 
# early_variables <- df %>%
#   filter(age >= 18 & age < 30) %>%
#   select(pid, coresidence, parental_income) %>%
#   group_by(pid) %>%
#   summarize(coresidence = mean(coresidence, na.rm = TRUE),
#             parental_income = mean(parental_income, na.rm = TRUE)) %>%
#   ungroup()
# 
# df_naive <- full_join(late_variables, early_variables)
# 
# naniar::gg_miss_var(df_naive)
# 
# df_naive <- drop_na(df_naive)
# 
# modelsummary::modelsummary(
#   models = list(
#     "Naive 1" = lm(income ~ coresidence, data = df_naive),
#     "Naive 2" = lm(income ~ coresidence + educ, data = df_naive),
#     "Naive 3" = lm(income ~ coresidence + educ + parental_income, data = df_naive)
#   ),
#   stars = c('*' = .1, '**' = .05, '***' = .01)
# )
# 
# df_mincer <- df %>%
#   filter(age >= 18 & age < 65) %>%
#   transmute(
#     lw = income,
#     S = educ,
#     Exp = pmax(0, age - S - 6),
#     Exp2 = Exp ** 2
#   )
# 
# naniar::gg_miss_upset(df_mincer)
# 
# df_mincer <- drop_na(df_mincer)
# 
# datasummary_skim(df_mincer)
# 
# model_mincer <- lm(lw ~ S + Exp + Exp2, data = df_mincer)
# modelsummary(
#   model_mincer,
#   stars = c('*' = .1, '**' = .05, '***' = .01)
# )
