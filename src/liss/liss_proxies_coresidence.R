library(tidyverse)
library(haven)

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

housing <- read_rds("liss_housing.Rds")

df <- read_rds("liss_core.Rds") %>%
  transmute(
    nomem_encr,
    wave = zoo::as.Date(zoo::as.yearmon(as.character(wave), "%Y%m")),
    year = year(wave),
    geslacht = haven::as_factor(geslacht),
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
  ) %>% 
  group_by(nomem_encr, year) %>% 
  summarize(
    geslacht = most_common(geslacht, na.rm = TRUE),
    coresidence = mean(coresidence, na.rm = TRUE),
    positie_f = most_common(positie_f, na.rm = TRUE),
    positie = most_common(positie, na.rm = TRUE),
    gebjaar = most_common(gebjaar, na.rm = TRUE),
    leeftijd = most_common(leeftijd, na.rm = TRUE),
    student = mean(student, na.rm = TRUE)
  ) %>% 
  ungroup()

df <- full_join(df, read_rds("liss_housing.Rds")) %>% 
  arrange(nomem_encr, year)

psych::cor.plot(
  df %>% select(coresidence, renter)
)

df %>% 
  group_by(year) %>% 
  summarize(
    n = sum(!is.na(renter)),
    renter = mean(renter, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, y = renter)) +
  geom_area() +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  )

housing %>% 
  group_by(year) %>% 
  summarize(
    n = sum(!is.na(renter)),
    renter = mean(renter, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  ggplot(aes(x = year, y = renter)) +
  geom_area() +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  )
