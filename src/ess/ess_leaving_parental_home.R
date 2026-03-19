library(tidyverse)

df_raw <- read_csv("data/ess/ESS9e03_2.csv")

df <- df_raw %>%
  mutate(
    country = cntry,
    year_left_parents = case_when(lvpntyr %in% c(0, 1111, 7777, 8888, 9999) ~ NA,
                                  .default = lvpntyr),
    coresidence = case_when(lvpntyr == 0 ~ 1, .default = 0),
    age = case_when(agea == 999 ~ NA,
                    .default = agea),
    year_born = case_when(yrbrn %in% c(7777, 8888, 9999) ~ NA,
                          .default = yrbrn),
    age_left_parents = year_left_parents - year_born,
    generation = case_when(
      year_born >= 2013                 ~ "Generation Alpha",
      year_born >= 1997 & year_born <= 2012 ~ "Generation Z",
      year_born >= 1981 & year_born <= 1996 ~ "Millennial",
      year_born >= 1965 & year_born <= 1980 ~ "Generation X",
      year_born >= 1946 & year_born <= 1964 ~ "Baby Boomer",
      year_born < 1946                   ~ "Silent Generation",
      .default = NA
    )
  )

coresidence_country <- df %>% 
  group_by(country) %>% 
  summarize(coresidence = mean(coresidence, na.rm = TRUE))

coresidence_birth_year <- df %>% 
  group_by(year_born) %>% 
  summarize(coresidence = mean(coresidence, na.rm = TRUE))

left_parents_countries <- df %>%
  group_by(country) %>%
  summarize(age_left_parents = mean(age_left_parents, na.rm = TRUE), n())

left_parents_generations <- df %>% 
  filter(country == "NL") %>% 
  group_by(generation) %>% 
  summarize(age_left_parents = mean(age_left_parents, na.rm = TRUE), n())

left_parents_birth_year <- df %>% 
  group_by(year_born) %>% 
  summarize(age_left_parents = mean(age_left_parents, na.rm = TRUE))

table(df$year_left_parents)
table(df$age)
