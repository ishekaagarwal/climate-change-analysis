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
library(purrr)
library(tidyverse)
library(readr)

main <- read_rds("main.rds")
main2 <- read_rds("main2.rds")
    
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(
        "How Climate Change Affects Mental Illness?",
        tabPanel(
            title = "Introduction",
            h6("By Isheka Agarwal"),
            h3("About the project:"),
            p("Climate change and mental illness are two of my major interests. Since the junior year of my high school, I have been very passionate about sustainability which crosses the path of climate change. A lot of people has a lot of assumptions about climate change. Some people are very concerned about it and some people think it is a hoax. Therefore, I thought about doing my Gov 1005 final project on how climate change affects mental illness. Firstly, I am analyzing how geographical locations affect the opinions of climate change. Then I am going to analyze the trends between climate change and mental illness.")
        ),
        
        tabPanel(
            title = "Graphics",
            plotOutput("distPlot"),
            plotOutput("number2")
            )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        main %>% 
            group_by(worry) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(total = sum(n)) %>% 
            mutate(n = n * 100/ total) %>% 
            select(-total) %>% 
            ggplot(aes(x = worry, y = n)) + 
            geom_col() + 
            coord_flip()
    })
    
    output$number2 <- renderPlot({
        main2 %>% 
            group_by(worry) %>% 
            count() %>% 
            ungroup() %>% 
            mutate(total = sum(n)) %>% 
            mutate(n = n * 100/ total) %>% 
            select(-total) %>% 
            ggplot(aes(x = worry, y = n)) + 
            geom_col() + 
            coord_flip()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
