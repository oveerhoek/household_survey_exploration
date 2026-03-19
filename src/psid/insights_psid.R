library(tidyverse)
library(haven)

df_original <- read_csv("ind2023er.csv")

df <- df_original %>% 
  transmute(
    id = ER30002,
    income = ER30012
  )
