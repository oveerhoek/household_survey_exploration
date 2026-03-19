library(tidyverse)
library(haven)


SOEP_PATH <- "data/soep/SOEP-CORE.v40.1eu_Stata/Stata_DE/soepdata/"

#SOEP_PATH <- "data/soep/soep-is.2023_stata_en/soep-is.2023_stata_en/"

pgen <- haven::read_dta(paste0(SOEP_PATH, "pgen.dta")) %>% 
  #select(cid, pid, syear, pgisced, pgemplst, pglabnet, pgstib)
  select(pid, syear)

labelled::look_for(pgen, "year")

pgen %>% 
  group_by(syear) %>% 
  summarize(n = n_distinct(pid)) %>% 
  ungroup()

pgen %>%
  group_by(pid) %>%
  summarise(n_years = n(), mean_years = mean(n_years)) %>%
  ungroup()

# pequiv <- haven::read_dta(paste0(SOEP_PATH, "pequiv.dta"))

ppathl <- haven::read_dta(paste0(SOEP_PATH, "ppathl.dta")) %>% 
  select(pid, syear, gebjahr, sex)

df <- full_join(pgen, ppathl, by = c("pid", "syear"))

# labelled::look_for(pgen, "pgstib")

summary(df)

df %>% 
  mutate(pglabnet = ifelse(pglabnet < 0, NA, pglabnet)) %>% 
  group_by(syear) %>% 
  summarize(income = mean(pglabnet, na.rm = TRUE), n = n()) %>% 
  ungroup() 
