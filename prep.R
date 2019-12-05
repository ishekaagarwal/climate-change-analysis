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
library(usmap)
library(ggridges)
library(tidyr)
library(dplyr)
library(data.table)
library(tidyverse)

# creating directory and reading in the data 
dir.create("raw-data")

dataset <- read.spss("raw-data/CCAM_SPSS_Data.sav", to.data.frame=TRUE) 

dataset2 <- read.csv("raw-data/PCS_2015.csv")

dataset3 <- read.csv("raw-data/data_no_of_extreme_heat_days.csv")

dataset4 <- read.csv("raw-data/data_no_of_hospitalizations.csv")

dataset5 <- read.csv("raw-data/data_prediction.csv")

#Processing the data for how corcerned people living in northeast are 

main <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "Northeast") %>% 
  group_by(worry) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(n = n * 100/ total) %>% 
  select(-total) %>% 
  ggplot(aes(x = worry, y = n)) + 
  geom_col(fill = "white", color = "red") + 
  coord_flip() + 
  labs(title = "How concerned people living in northeastern region of the United States are about climate change?",
       y = "Percentage of people",
       x = "Level of concerns")

write_rds(main, "final_project/main.rds")

#Processing the data for how corcerned people living in west are

main2 <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "West") %>% 
  group_by(worry) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(n = n * 100/ total) %>% 
  select(-total) %>% 
  ggplot(aes(x = worry, y = n)) + 
  geom_col(fill = "white", color = "red") + 
  coord_flip() + 
  labs(title = "How concerned people living in western region of the United States are about climate change?",
       y = "Percentage of people",
       x = "Level of concerns")

write_rds(main2, "final_project/main2.rds")

#Processing the data for how corcerned people living in south are

main3 <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "South") %>% 
  group_by(worry) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(n = n * 100/ total) %>% 
  select(-total) %>% 
  ggplot(aes(x = worry, y = n)) + 
  geom_col(fill = "white", color = "red") + 
  coord_flip() + 
  labs(title = "How concerned people living in the southern region of the United States are about climate change?",
       y = "Percentage of people",
       x = "Level of concerns")

write_rds(main3, "final_project/main3.rds")

#Processing the data for how corcerned people living in midwest are

main4 <- dataset %>%
  drop_na() %>%
  select(region4, worry) %>%
  group_by(worry) %>%
  filter(region4 == "Midwest") %>% 
  group_by(worry) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(n = n * 100/ total) %>% 
  select(-total) %>% 
  ggplot(aes(x = worry, y = n)) + 
  geom_col(fill = "white", color = "red") + 
  coord_flip() + 
  labs(title = "How concerned people living in midwest of the United States are about climate change?",
       y = "Percentage of people",
       x = "Level of concerns")

write_rds(main4, "final_project/main4.rds")


# Processing the data for number of extreme heat days and events by state

    ## For 2013

heat_2013 <- dataset3 %>%
  filter(Year == "2013") 

heat_days_2013 <- plot_usmap(data = heat_2013, values = "Value", 
                             regions = "states", size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "Rel. Prob.") +
  labs(title = "Extreme Heat Days and Events in the United States in 2013",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2013, "final_project/heat_2013.rds")


  ## For 2014


heat_2014 <- dataset3 %>%
  filter(Year == "2014") 
heat_days_2014 <- plot_usmap(data = heat_2014, values = "Value", regions = "states", 
             size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "Rel. Prob.") +
  labs(title = "Extreme Heat Days and Events in the United States in 2014",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2014, "final_project/heat_2014.rds")


  ## For 2015 

heat_2015 <- dataset3 %>%
  filter(Year == "2015")

heat_days_2015 <- plot_usmap(data = heat_2015, values = "Value", regions = "states", 
             size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "Rel. Prob.") +
  labs(title = "Extreme Heat Days and Events in the United States in 2015",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2015, "final_project/heat_2015.rds")


  ## For 2016

heat_2016 <- dataset3 %>%
  filter(Year == "2016") 

heat_days_2016 <- plot_usmap(data = heat_2016, values = "Value", regions = "states", 
             size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "Rel. Prob.") +
  labs(title = "Extreme Heat Days and Events in the United States in 2016",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2016, "final_project/heat_2016.rds")


# Creating heat maps for number of hospitalizations 
  ##For 2013

names(dataset4)[1]<- "fips"

hospitalization_2013 <- dataset4 %>%
  filter(Year == "2013") 

no_hospitalization_2013 <- plot_usmap(data = hospitalization_2013, 
             values = "Value", 
             regions = "state", 
             size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Density of hospitalizations") +
  labs(title = "Heat Stress Hospitalizations in the United States in 2013",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(no_hospitalization_2013, "final_project/hospitalization2013.rds")

  ##For 2014

hospitalization_2014 <- dataset4 %>%
  filter(Year == "2014") 

no_hospitalization_2014 <- plot_usmap(data = hospitalization_2014, 
                                      values = "Value", 
                                      regions = "state", 
                                      size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Density of hospitalizations") +
  labs(title = "Heat Stress Hospitalizations in the United States in 2014",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(no_hospitalization_2014, "final_project/hospitalization2014.rds")

  ## For 2015

hospitalization_2015 <- dataset4 %>%
  filter(Year == "2015") 

no_hospitalization_2015 <- plot_usmap(data = hospitalization_2015, 
                                      values = "Value", 
                                      regions = "state", 
                                      size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Density of hospitalizations") +
  labs(title = "Heat Stress Hospitalizations in the United States in 2015",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(no_hospitalization_2015, "final_project/hospitalization2015.rds")

  ## For 2016

hospitalization_2016 <- dataset4 %>%
  filter(Year == "2016") 

no_hospitalization_2016 <- plot_usmap(data = hospitalization_2016, 
                                      values = "Value", 
                                      regions = "state", 
                                      size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Density of hospitalizations") +
  labs(title = "Heat Stress Hospitalizations in the United States in 2016",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(no_hospitalization_2016, "final_project/hospitalization2016.rds")

  ## For 2017

hospitalization_2017 <- dataset4 %>%
  filter(Year == "2017") 

no_hospitalization_2017 <- plot_usmap(data = hospitalization_2017, 
                                      values = "Value", 
                                      regions = "state", 
                                      size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "blue", 
                        name = "Density of hospitalizations") +
  labs(title = "Heat Stress Hospitalizations in the United States in 2017",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(no_hospitalization_2017, "final_project/hospitalization2017.rds")


# Creating heat maps for future projection of extreme heat day events 
  ## For 2020

prediction_2020 <- dataset5 %>%
  filter(Year == "2020") 

heat_prediction_2020 <- plot_usmap(data = prediction_2020, 
                                      values = "Value", 
                                      regions = "state", 
                                      size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "orange", 
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2020",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2020, "final_project/prediction2020.rds")

  ## For 2025

prediction_2025 <- dataset5 %>%
  filter(Year == "2025") 

heat_prediction_2025 <- plot_usmap(data = prediction_2025, 
                                   values = "Value", 
                                   regions = "state", 
                                   size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "orange", 
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2025",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2025, "final_project/prediction2025.rds")

  ## For 2030

prediction_2030 <- dataset5 %>%
  filter(Year == "2030") 

heat_prediction_2030 <- plot_usmap(data = prediction_2030, 
                                   values = "Value", 
                                   regions = "state", 
                                   size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "white", high = "orange", 
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2030",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2030, "final_project/prediction2030.rds")




  #merge(x, y, by.x = "fips", by.y = "FIPS", all.x = T)

  #run regression on number of heat days vs. population size 

  #run regression on number of heat days by county vs urban status 


