library(tidyverse)
library(readxl)

obp <- read_excel("OBP.xlsx")

obp$year <- format(as.Date(obp$Date, format="%d/%m/%Y"),"%Y")
obp$yr <- as.numeric(obp$year)

table(obp$yr)

table(obp$yr, obp$Outcome)
