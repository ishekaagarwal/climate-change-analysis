library(foreign)
library(readxl)
library(devtools)
library(plotrix)
library(fs)
library(reshape2)
library(rstanarm)
library(scales)
library(stringr)
library(broom)
library(infer)
library(ggridges)
library(tidyverse)

# creating directory and reading in the data 
dir.create("raw-data")

dataset <- read.spss("raw-data/CCAM_SPSS_Data.sav", to.data.frame=TRUE) 

dataset2 <- read.csv("raw-data/PCS_2015.csv")

#Processing the data for the first ggplot

main <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "Northeast")

write_rds(main, "final_project/main.rds")

#Processing the data for the second ggplot

main2 <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "West")

write_rds(main2, "final_project/main2.rds")
