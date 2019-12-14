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
library(foreign)
library(haven)
library(tidyverse)

# creating directory and reading in the data 

dir.create("raw-data")

dataset <- read.spss("raw-data/CCAM_SPSS_Data.sav", to.data.frame=TRUE) 

dataset3 <- read.csv("raw-data/data_no_of_extreme_heat_days.csv")

dataset5 <- read.csv("raw-data/data_prediction.csv")

dataset7 <- read_dta("raw-data/NBER_county_population.dta")

dataset8 <- read.csv("raw-data/YCOM_2019_Data.csv")


# Processing the data for number of extreme heat days and events by state

    ## For 2013

heat_2013 <- dataset3 %>%
  filter(Year == "2013") 

heat_days_2013 <- plot_usmap(data = heat_2013, values = "Value", 
                             exclude = c("AK", "HI"),
                             regions = "states", size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "No. of heat day events") +
  labs(title = "Extreme Heat Days and Events in the United States in 2013",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2013, "final_project/heat_2013.rds")


  ## For 2014


heat_2014 <- dataset3 %>%
  filter(Year == "2014") 

heat_days_2014 <- plot_usmap(data = heat_2014, values = "Value", 
                             exclude = c("AK", "HI"),
                             regions = "states", size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", 
                        name = "No. of heat day events") +
  labs(title = "Extreme Heat Days and Events in the United States in 2014",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2014, "final_project/heat_2014.rds")


  ## For 2015 

heat_2015 <- dataset3 %>%
  filter(Year == "2015")

heat_days_2015 <- plot_usmap(data = heat_2015, values = "Value", 
                             exclude = c("AK", "HI"),
                             regions = "states", size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "No. of heat day events") +
  labs(title = "Extreme Heat Days and Events in the United States in 2015",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2015, "final_project/heat_2015.rds")


  ## For 2016

heat_2016 <- dataset3 %>%
  filter(Year == "2016") 

heat_days_2016 <- plot_usmap(data = heat_2016, values = "Value",
                             exclude = c("AK", "HI"),
                             regions = "states", size = 0.05) +
  theme(panel.background = element_rect(color = "black", fill = "white")) +
  scale_fill_continuous(low = "yellow", high = "red", name = "No. of heat day events") +
  labs(title = "Extreme Heat Days and Events in the United States in 2016",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_days_2016, "final_project/heat_2016.rds")


# Creating heat maps for future projection of extreme heat day events 
  ## For 2020

names(dataset5)[6] <- "value"
names(dataset5)[3] <- "fips"

prediction2020 <- dataset5 %>%
  filter(value != "No Data") %>%
  filter(Year == 2020) %>% 
  mutate(value = as.numeric(value))

heat_prediction_2020 <- plot_usmap(data = prediction2020, 
                   values = "value",
                   exclude = c("AK", "HI"),
                   regions = "state", 
                   size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) + 
  scale_fill_continuous(low = "white", high = "blue4",
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2020",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2020, "final_project/prediction-2020.rds")


  ## For 2025

names(dataset5)[6] <- "value"
names(dataset5)[3] <- "fips"

prediction2025 <- dataset5 %>%
  filter(value != "No Data") %>%
  filter(Year == 2025) %>% 
  mutate(value = as.numeric(value))

heat_prediction_2025 <- plot_usmap(data = prediction2025, 
                   values = "value",
                   exclude = c("AK", "HI"),
                   regions = "state", 
                   size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) + 
  scale_fill_continuous(low = "white", high = "blue4",
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2025",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2025, "final_project/prediction-2025.rds")


  ## For 2030

names(dataset5)[6] <- "value"
names(dataset5)[3] <- "fips"

prediction2030 <- dataset5 %>%
  filter(value != "No Data") %>%
  filter(Year == 2030) %>% 
  mutate(value = as.numeric(value))

heat_prediction_2030 <- plot_usmap(data = prediction2030, 
                   values = "value",
                   exclude = c("AK", "HI"),
                   regions = "state", 
                   size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) + 
  scale_fill_continuous(low = "white", high = "blue4",
                        name = "Projected no. of future events") +
  labs(title = "Future Projection of Extreme Heat Days and Events in the United States in 2030",
       caption = "Data collected from National Environmental Public Health Tracking Network.")

write_rds(heat_prediction_2030, "final_project/prediction-2030.rds")


# Assigning the dataset8(Yale Climate Change opinion maps to beliefs)
# To narrow down the dataset, I am only selcting GeoName, GeoID, worried,
# futuregen and CO2limits 
# I filtered the states names only from GeoName as I don't want to map in counties


beliefs <- dataset8 %>%
  select(GeoName, GEOID, worried, futuregen, CO2limits) %>%
  filter(GeoName %in% c("Alabama", "Alaska", "Arizona", "Arizona", "Arkansas",
                        "California", "Colorado", "Connecticut", "Delaware", 
                        "District of Columbia", "Florida", "Georgia",
                        "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                        "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                        "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                        "New Jersey", "New Mexico", "New York", "North Carolina",
                        "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                        "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                        "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                        "West Virginia", "Wisconsin", "Wyoming")) 


# Assigning dataset7(population) dataset to a new object called population
# I am only selecting areaname and pop2009. Pop2009 is the most recent 
# population dataset I could find with matched fips code
# Then I filtered the areaname as I only want 51 states
# I use distinct function because District of Columbia appeared
# twice in my datatset 

population <- dataset7 %>%
  select(pop2009, areaname) %>%
  filter(areaname %in% c("Alabama", "Alaska", "Arizona", "Arizona", "Arkansas",
                         "California", "Colorado", "Connecticut", "Delaware", 
                         "District of Columbia", "Florida", "Georgia",
                         "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                         "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                         "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                         "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                         "New Jersey", "New Mexico", "New York", "North Carolina",
                         "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                         "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                         "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                         "West Virginia", "Wisconsin", "Wyoming")) %>%
  distinct()


# To clean the dataset and organize it in a way R likes/my functions work
# I change the name of the columns in population and beliefs dataset


names(population)[2] <- "states"
names(beliefs)[1] <- "states" 
names(beliefs)[2] <- "fips"

# Now I am merging beliefs and population datasets to create a new dataset
# called merged_pop_beliefs because I want both beliefs and population 
# to be on the same datatset to create proper models

merged_pop_beliefs <- merge(beliefs, population, by = "states")

# I assign my recently created dataset merged_pop_beliefs to analysis 
# to make some changes and organize the dataset
# I create a new variable of mine using mutate function called avg_concern

analysis <- merged_pop_beliefs %>%
  group_by(states) %>%
  mutate(avg_concern = (worried + futuregen + CO2limits)/3) 

# Avg_concern variable shows how worried people are about global warming, 
# how they think that global warming is going to affect their future generations 
# and how strongly they support setting strict limits on existing coal-fired power plants 


# To map how concerned people are in average and how supportive they are about setting
# limits on coal-fired power plants, I use the function of plot_usmap
# I set my values equal to avg_concern because that's the variable I want to map 
# Theme, scale_fill_continuous and labs functions are used to make the map
# pretty and comprehensible.


plot_beliefs <- plot_usmap(data = analysis, 
                           values = "avg_concern", 
                           regions = "states", 
                           size = 0.05) +
  theme(panel.background = element_rect(color = "black", 
                                        fill = "white")) + 
  scale_fill_continuous(low = "white", high = "blue4", 
                        name = "Percentage") + 
  labs(title = "Concerns about Global Warming in the United States",
       subtitle = "Measure of how worried people are about consequences of global warming and their support for climate change actions",
       caption = "Howe, P., Mildenberger, M., Marlon, J., & Leiserowitz, A. (2015), Geographic variation in opinions on climate change at state and local scales in the USA.")

write_rds(plot_beliefs, "final_project/plot_beliefs.rds")

  ## Now I want to see what variables affect people's beliefs of climate change.
  ## The first variable that came to my mind was population
  ## So I found data of state's population from the census 
  ## I filtered the state names so that I can match the state codes of 
  ## population dataset with the state codes of the climate change opinion dataset
  ## I create a gt table to show the coefficient of correlation and intercept
  ## for 5th and 95th confidence intervals

model1 <- lm(data = analysis, formula = avg_concern ~ pop2009) %>%
  tidy(conf.int = TRUE, conf.level = 0.9) %>% 
  mutate(term = c("Intercept", "Population")) %>% 
  select(term, estimate, conf.low, conf.high) %>%
  gt() %>% 
  tab_header(title = "Population affecting climate change beliefs") %>% 
  tab_source_note(md("NBER County Population data from 2009")) %>% 
  cols_label(term = "", estimate = "Coefficient", 
             conf.low = "5th percentile", conf.high = "95th percentile") %>%
  fmt_number(columns = 2:4, decimals = 10)

write_rds(model1, "final_project/model.rds")

  ## I create a ggplot to run regression of population on climate change 
  ## Geom_smooth is used to create a line of regression on my plot

regression_data <- analysis %>% 
  ggplot(aes(x = as.numeric(log(pop2009)), y = as.numeric(avg_concern))) + 
  geom_point() +
  geom_smooth(data = analysis, method = "lm", se = FALSE) + 
  labs(x = "Population",
       y = "People's average concern of climate change",
       title = "How population affects climate change beliefs?",
       subtitle = "Regression of population on average concern of climate change",
       caption = "Used NBER County Population data")

write_rds(regression_data, "final_project/regression.rds")



