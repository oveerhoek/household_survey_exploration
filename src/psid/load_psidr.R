library(tidyverse)
library(psidR)

install.packages('devtools')
install_github("psidR",username="floswald")

build.psid(datadr = "PSID Raw", small = TRUE)

example(build.panel)
