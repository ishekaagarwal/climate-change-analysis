#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(janitor)
library(ggplot2)
library(shinythemes)
library(purrr)
library(usmap)
library(tidyverse)
library(readr)


heat_2013 <- read_rds("heat_2013.rds")
heat_2014 <- read_rds("heat_2014.rds")
heat_2015 <- read_rds("heat_2015.rds")
heat_2016 <- read_rds("heat_2016.rds")

prediction2020 <- read_rds("prediction-2020.rds")
prediction2025 <- read_rds("prediction-2025.rds")
prediction2030 <- read_rds("prediction-2030.rds")

plotbeliefs <- read_rds("plot_beliefs.rds")

model2 <- read_rds("model.rds")

regression_data <- read_rds("regression.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
    
    navbarPage(
       title = "Perspective on Historical and Future Consequences of Climate Change ",
        tabPanel(
            title = "About the Project",
            h5("I am Isheka Agarwal, a freshman at Harvard College. I am interested in Data Science, Applied Math and Psychology."),
            br(),
            h4("Background of the Project:"),
            h5("For my data analysis class (GOV 1005), I made this final project on Perspective on Historical and Future Consequences of Climate Change. This project analyzes the historical consequences of climate change, represents future projection for consequences of climate change and various opinions of people living in various regions in the United States. This project also creates a model to analyze what affects people's concern about climate change."),
            h5("The historical consequences of climate change are indicated by extreme heat days and events by counties in the United States and measured by number of extreme heat days occuring from 2013-16. The extreme heat days are counted when the daily maximum temperature goes beyond a relative threshold of 90th Percentile. In order to see how extreme heat days affect people living in these regions, I analyzed the heat stress hospitalizations in each of the states, mapping the number of hospitalizations for heat stress across the United States from 2013-17. Then I analyzed relative data to predict the future projection of extreme heat across the United States within the next decade and how it will impact people's lives."),
            h5("I collected data from National Environmental Public Health Tracking Network to make my analysis on abovementioned areas."),
            h5("Additionally, I accessed data from the Climate Change in the American Mind (CCAM) research project to analyze the climate change beliefs of people who are living in four major regions across the United States: Northeast, South, Midwest and West. Then I created some models using linear regression to analyze what affects people's beliefs about climate change."), 
            h5("Github repository for this project: https://github.com/ishekaagarwal/final-project-isheka"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h6("Accessed from:
               Centers for Disease Control and Prevention. Environmental Health Tracking Program. Extreme heat days and events. https://ephtracking.cdc.gov/DataExplorer.
               Yale Program on Climate Change Communication (YPCCC) & George Mason University Center for Climate Change Communication (Mason 4C). (2019). Climate Change in the American Mind: National survey data on public opinion (2008-2017) [Data file and codebook]. doi: 10.17605/OSF.IO/W36GN")
            ),
        
        tabPanel(
            title = "Effects of Climate Change",
            h3("Trends of extreme heat days and events in the US from 2013 to 2016"),
            br(),
            br(),
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "heat_year",
                                 label = "Select a year", 
                                 choices = c("2013", "2014", "2015", "2016"))
                ),
            mainPanel(
                plotOutput("heatchart")
            )), 
            br(),
            br(),
            h4("In 2013, it can be analyzed that extreme high heat day events occurred in some of the counties in the South, which are represented by the dark red color. In 2014, the extreme heat day events spread across the midwest and northeast region of the United States. There is a similar increasing trend of extreme heat day events from 2013 to 2016. This shows how extreme heat day events are increasing due to climate change in the United States."),
            br(),
            br(),
            h3("Future projection of heat days and events in the US for the next decade (2020-2030)"), 
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "prediction_year",
                                 label = "Select a year", 
                                 choices = c("2020", "2025", "2030"))
                ),
                mainPanel(
                    plotOutput("predictionchart")
                ),
            h4("The future projection of heat days and events compared to the past heat day events show that the numbers highly escalated. The range for past heat day events was from (low) zero to (high) fifteen but the range for projected number of future events are from (low) fifty to (high) one hundred and fifty. This projected map shows what negative and enormous impact climate change has.")
            )
    ),  
    tabPanel(
        title = "Climate Change beliefs",
        h3("Concerns about global warming in the United States"),
        plotOutput("plotbeliefs"), 
        br(),
        br(),
        h4("This map represents data collected from Climate Change in the American Mind (CCAM) research project. It maps the average concern variable that I created from this dataset. Average concern variable is the average results of how worried people are about global warming, how they think that global warming is going to affect their future generations and how strongly they support setting strict limits on existing coal-fired power plants. It can be analyzed from the map that people living in the northeast and west region are very concerned about climate change. The rest of the concern variable is mostly and evenly distributed all over the United States. However, this analysis arises a concern that people need to be more aware of climate change and be worried about its consequences."),
        tabPanel(
        title = "Models",
        h3("Does population affect people's opinions of climate change living in these states?"),
        mainPanel(
            plotOutput("regression"),
            br(),
            br(),
            h4("I wanted to explore if population affects climate change. So I used population data of 2009 in the United State and merged it with my climate change belief dataset. Using this merged dataset, I created a model to run regression of population on climate change beliefs. From this scatterplot, it can be analyzed that there is a very weak but positive correlation between population and climate change concerns. Therefore, it can be concluded that there is no strong evidence that population affects people’s climate change concerns."),
            br(),
            br(),
            br(),
            gt_output("model2"),
            h4("I ran a single variable regression model to find the coefficient of correlation between population and climate change beliefs. Looking at these values, it can be concluded that there is no correlation between population and people’s opinions about climate change."),
            br()
        )
  
    )
    
)))

# Define server logic required to draw a histogram


server <- function(input, output) {

#Output for historical heat day event map in the US

    output$heatchart <- renderPlot({
        if(input$heat_year == "2013"){
            heat_2013
        }
        else if(input$heat_year == "2014"){
            heat_2014
        }
        else if(input$heat_year == "2015"){
            heat_2015
        }
        else if(input$heat_year == "2016"){
            heat_2016
        }
    })
    
# Output for prediction map
   
        output$predictionchart <- renderPlot({
        if(input$prediction_year == "2020"){
            prediction2020
        }
        else if(input$prediction_year == "2025"){
            prediction2025
        }
        else if(input$prediction_year == "2030"){
            prediction2030
        }
    })
 
# Output for average concerns map
   
    output$plotbeliefs <- renderPlot({
        plotbeliefs
    })
    
# Regression
# Output for model
    
    output$regression <- renderPlot({
        regression_data
    })

    output$model2 <- render_gt({
       model2
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
