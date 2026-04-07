library(tidyverse)

panel <- read_rds("tmp/liss/liss_core.Rds") %>% 
  transmute(
    nomem_encr,
    wave = zoo::as.Date(zoo::as.yearmon(as.character(wave), "%Y%m")),
    gebjaar,
    leeftijd,
    brutoink = case_when(
      brutoink %in% c(-15, -13) ~ NA,
      .default = brutoink
    ),
    oplcat = haven::as_factor(oplcat)
  )

earnings_by_educ <- panel %>% 
  filter(oplcat %in% c("wo (university)", "hbo (higher vocational education, US: college)")) %>% 
  filter(year(wave) == 2024) %>% 
  filter(brutoink > 0) %>% 
  group_by(leeftijd, oplcat) %>% 
  summarize(n = n(), brutoink = mean(brutoink, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(n > 100)

ggplot(earnings_by_educ, aes(x = leeftijd, y = brutoink, color = oplcat)) +
  geom_line() +
  theme_minimal()
  
  