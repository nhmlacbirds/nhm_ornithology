library(tidyverse)
library(readxl)

sal <- read_excel("salvage2023.xlsx")
sal$species <- tolower(sal$Species)

sallist <- table(sal$species)

write.csv(sallist, "salvagelist.csv")
