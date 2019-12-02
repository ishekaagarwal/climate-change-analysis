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
library(tidyverse)
library(readr)

main <- read_rds("main.rds")
main2 <- read_rds("main2.rds")
main3 <- read_rds("main3.rds")
main4 <- read_rds("main4.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(
       title = "How Climate Change Affects Mental Illness?",
        tabPanel(
            title = "Introduction",
            h6("By Isheka Agarwal"),
            h3("About the project:"),
            p("Climate change and mental illness are two of my major interests. Since the junior year of my high school, I have been very passionate about sustainability which crosses the path of climate change. A lot of people has a lot of assumptions about climate change. Some people are very concerned about it and some people think it is a hoax. Therefore, I thought about doing my Gov 1005 final project on how climate change affects mental illness. Firstly, I am analyzing how geographical locations affect the opinions of climate change. Then I am going to analyze the trends between climate change and mental illness.")
        ),
        
        tabPanel(
            title = "Graphics",
            h3("How living in different regions affect people's perspective on climate change?"),
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
)))

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
}

# Run the application 
shinyApp(ui = ui, server = server)
