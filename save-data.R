library(tidyverse)

# extract table from csv file
dat <- read.csv("NBC funds.csv")

#save RDA file
save(dat, file = "fundlinks.rda")