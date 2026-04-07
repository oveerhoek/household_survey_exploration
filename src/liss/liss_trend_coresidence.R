library(tidyverse)
library(haven)

panel <- read_rds("liss_core.Rds") %>%
  transmute(
    nomem_encr,
    wave = zoo::as.Date(zoo::as.yearmon(as.character(wave), "%Y%m")),
    year = year(wave),
    geslacht = haven::as_factor(geslacht),
    # housemate = case_when(
    #   positie == 6 ~ 1,
    #   .default = NA
    # )
    coresidence = case_when(
      positie == 5 ~ 1,                                # living with parents
      positie %in% c(1, 2, 3, 4, 6, 7) ~ 0,             # not living with parents
      positie == 9 ~ NA_real_,                          # missing/unknown
      .default = NA_real_
    ),
    positie_f = as_factor(positie),
    positie,
    gebjaar,
    leeftijd,
    student = case_when(
      belbezig == 7 ~ 1,                               # enrolled as student
      .default = 0
    )
  )

# Check: Are there composition shifts in classification over time?

panel %>% 
  group_by(wave, positie_f) %>% 
  summarize(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = wave, y = share, fill = positie_f)) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent) 

panel %>%
  group_by(wave, coresidence) %>% 
  summarize(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = wave, y = share, fill = as_factor(coresidence))) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent)

panel %>%
  filter(leeftijd >= 16 & leeftijd <= 25) %>% 
  group_by(leeftijd, coresidence) %>% 
  summarize(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = leeftijd, y = share, fill = as_factor(coresidence))) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent)


positie_by_leeftijd <- panel %>% 
  group_by(leeftijd, positie_f) %>% 
  summarize(n = n()) %>% 
  mutate(share = n / sum(n)) %>% 
  ungroup() %>% 
  filter(leeftijd < 80)

ggplot(positie_by_leeftijd,
       aes(x = leeftijd, y = share, fill = positie_f)) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent)


students <- read_rds("liss_highest_educ.Rds") %>% 
  group_by(nomem_encr) %>%
  summarize(
    wo  = any(highest_educ == 26),                     # university degree
    hbo = any(highest_educ == 20) & !any(highest_educ == 26)
  ) %>% 
  filter(wo == TRUE | hbo == TRUE) %>% 
  pull(nomem_encr)

panel %>%
  filter(leeftijd >= 16, leeftijd <= 25) %>%
  mutate(tertiary_diploma = nomem_encr %in% students) %>%
  count(leeftijd, tertiary_diploma, coresidence, name = "n") %>%
  group_by(leeftijd, tertiary_diploma) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>% 
  
  ggplot(aes(x = leeftijd, y = share, fill = as_factor(coresidence))) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~tertiary_diploma)

panel %>%
  filter(leeftijd >= 16, leeftijd <= 25) %>%
  mutate(tertiary_diploma = nomem_encr %in% students) %>%
  count(year, tertiary_diploma, coresidence, name = "n") %>%
  group_by(year, tertiary_diploma) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>% 
  
  ggplot(aes(x = year, y = share, fill = as_factor(coresidence))) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~tertiary_diploma)

panel %>%
  filter(leeftijd >= 18, leeftijd <= 30) %>%
  mutate(tertiary_diploma = nomem_encr %in% students) %>%
  mutate(tertiary_diploma = factor(tertiary_diploma,
                                   levels = c(TRUE, FALSE),
                                   labels = c("Had HBO/WO diploma in 2025",
                                              "Did not have HBO/WO diploma in 2025"))) %>% 
  count(year, tertiary_diploma, positie_f, name = "n") %>%
  group_by(year, tertiary_diploma) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>% 
  
  ggplot(aes(x = year, y = share, fill = positie_f)) +
  geom_area(color = "white", size = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~tertiary_diploma) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Share",
    title = "Household position composition over time"
  )

trend <- panel %>% 
  filter(nomem_encr %in% students) %>%
  filter(student == 1) %>%
  filter(leeftijd >= 18 & leeftijd <= 23) %>%
  group_by(wave) %>% 
  summarize(
    share_coresidence = mean(coresidence, na.rm = TRUE),
    n_students = n_distinct(nomem_encr)
  ) %>% 
  ungroup()

ggplot(trend, aes(x = wave, y = share_coresidence)) + geom_line() +
  theme_minimal() +
  labs(
    title = "Trend in parental co-residence over time",
    y = "Share residing with parents (%)",
    x = "Time"
  )

# +
# labs(
#   x = "Age",
#   y = "Share of respondents",
#   fill = "Household position",
#   title = "Household position composition over age"
# ) +
# theme_minimal()
