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

main <- read_rds("main.rds")
main2 <- read_rds("main2.rds")
main3 <- read_rds("main3.rds")
main4 <- read_rds("main4.rds")

heat_2013 <- read_rds("heat_2013.rds")
heat_2014 <- read_rds("heat_2014.rds")
heat_2015 <- read_rds("heat_2015.rds")
heat_2016 <- read_rds("heat_2016.rds")

hospitalization_2013 <- read_rds("hospitalization2013.rds")
hospitalization_2014 <- read_rds("hospitalization2014.rds")
hospitalization_2015 <- read_rds("hospitalization2015.rds")
hospitalization_2016 <- read_rds("hospitalization2016.rds")
hospitalization_2017 <- read_rds("hospitalization2017.rds")

prediction_2020 <- read_rds("prediction2020.rds")
prediction_2025 <- read_rds("prediction2025.rds")
prediction_2030 <- read_rds("prediction2030.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
    
    navbarPage(
       title = "Perspective on Historical and Future Consequences of Climate Change ",
        tabPanel(
            title = "About the Project",
            h5("I am Isheka Agarwal, a freshman at Harvard College. I am interested in Data Science, Applied Math and Psychology."),
            br(),
            h4("Background of the Project:"),
            h5("For my data analysis class (GOV 1005), I made this final project. This project analyzes the historical consequences of climate change, represents future projection for consequences of climate change and various opinions of people living in various regions in the United States. This project also creates a model to analyze what affects people's concern about climate change."),
            h5("The historical consequences of climate change are indicated by extreme heat days and events by counties in the United States and measured by number of extreme heat days occuring from 2013-16. The extreme heat days are counted when the daily maximum temperature goes beyond a relative threshold of 90th Percentile. In order to see how extreme heat days affect people living in these regions, I analyzed the heat stress hospitalizations in each of the states, mapping the number of hospitalizations for heat stress across the United States from 2013-17. Then I analyzed relative data to predict the future projection of extreme heat across the United States within the next decade and how it will impact people's lives."),
            h5("I collected data from National Environmental Public Health Tracking Network to make my analysis on abovementioned areas."),
            h5("Additionally, I accessed data from the Climate Change in the American Mind (CCAM) research project to analyze the climate change beliefs of people who are living in four major regions across the United States: Northeast, South, Midwest and West. Then I created some models using linear regression to analyze what affects people's beliefs about climate change."), 
            h5("Github repository for this project: https://github.com/ishekaagarwal/final-project-isheka"),
            h6("Accessed from:
               Centers for Disease Control and Prevention. Environmental Health Tracking Program. Extreme heat days and events. https://ephtracking.cdc.gov/DataExplorer.
               Yale Program on Climate Change Communication (YPCCC) & George Mason University Center for Climate Change Communication (Mason 4C). (2019). Climate Change in the American Mind: National survey data on public opinion (2008-2017) [Data file and codebook]. doi: 10.17605/OSF.IO/W36GN")
            ),
        
        tabPanel(
            title = "Consequences of Climate Change",
            h5("Heat Maps of Heat Day Events, Hospitalizations and Future Prediction"),
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
            br(),
            br(),
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "hospitalization_year",
                                 label = "Select a year", 
                                 choices = c("2013", "2014", "2015", "2016", "2017"))
                ),
                mainPanel(
                    plotOutput("hospitalizationchart")
                )
            ), 
            br(),
            br(),
            br(),
            br(),
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "prediction_year",
                                 label = "Select a year", 
                                 choices = c("2020", "2025", "2030"))
                ),
                mainPanel(
                    plotOutput("predictionchart")
                )
            )
    ),  tabPanel(
        title = "Climate Change Beliefs",
        h5("People's beliefs about climate change in four different regions in the United States"),
        br(),
        br(),
        sidebarLayout(
            sidebarPanel(
                radioButtons(inputId = "region_type",
                             label = "US Demographical region", 
                             choices = c("Northeast", "West", "South", "Midwest"))
            ),
            mainPanel(
                plotOutput("regionchart")
            )
        )
    )
))

# Define server logic required to draw a histogram


server <- function(input, output) {
    
    output$regionchart <- renderPlot({
        if(input$region_type == "Northeast"){
            main
        }
        else if(input$region_type == "West"){
            main2
        }
        else if(input$region_type == "South"){
            main3
        }
        else if(input$region_type == "Midwest"){
            main4
        }
    }) 
    
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
    
    
    output$hospitalizationchart <- renderPlot({
        if(input$hospitalization_year == "2013"){
            hospitalization_2013
        }
        else if(input$hospitalization_year == "2014"){
            hospitalization_2014
        }
        else if(input$hospitalization_year == "2015"){
            hospitalization_2015
        }
        else if(input$hospitalization_year == "2016"){
            hospitalization_2016
        }
        else if(input$hospitalization_year == "2017"){
            hospitalization_2017
        }
    })
    
    output$predictionchart <- renderPlot({
        if(input$prediction_year == "2020"){
            prediction_2020
        }
        else if(input$prediction_year == "2025"){
            prediction_2025
        }
        else if(input$prediction_year == "2030"){
            prediction_2030
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
