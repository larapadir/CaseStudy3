#DS501 CaseStudy 3
library(shiny)
library(dplyr)
library(tidyverse)
library(rsconnect)

stuData <- read.csv(file = "forestfires.csv")
stuData = stuData %>% mutate(month=recode(month, 
                         'jan'= "1",
                         'feb' ="2",
                         'mar' ="3",
                         'apr'="4",
                         'may'="5",
                         'jun'="6",
                         'jul'="7",
                         'aug'="8",
                         'sep'="9",
                         'oct'="10",
                         'nov'="11",
                         'dec'="12"))

stuData = stuData %>% mutate(day=recode(day, 
                                        `sun`="1",
                                        `mon`="2",
                                        `tue`="3",
                                        `wed`="4",
                                        `thu`="5",
                                        `fri`="6",
                                        `sat`="7"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel("Regression Model of Forest Fires in Portugal"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ycord", label = h3("Outcome Variable"),
                  choices = list("X" = "X",
                                 "Y" = "Y",
                                 "Month" = "month",
                                 "Day" = "day",
                                 "DMC" = "DMC",
                                 "DC" = "DC",
                                 "ISI" = "ISI",
                                 "temp" = "temp",
                                 "RH" = "RH",
                                 "Wind" = "wind",
                                 "Rain" = "rain",
                                 "Area" = "area"), selected = 1),
      
      selectInput("xcord", label = h3("Input Variable"),
                  choices = list("X" = "X",
                                 "Y" = "Y",
                                 "Month" = "month",
                                 "Day" = "day",
                                 "DMC" = "DMC",
                                 "DC" = "DC",
                                 "ISI" = "ISI",
                                 "Temp" = "temp",
                                 "RH" = "RH",
                                 "Wind" = "wind",
                                 "Rain" = "rain",
                                 "Area" = "area"), selected = 1),
      
      checkboxGroupInput("show_vars", "Columns of Forest Fire Data to show:",
                         c("X" = "X",
                          "Y" = "Y",
                          "Month" = "month",
                          "Day" = "day",
                          "DMC" = "DMC",
                          "DC" = "DC",
                          "ISI" = "ISI",
                          "Temp" = "temp",
                          "RH" = "RH",
                          "Wind" = "wind",
                          "Rain" = "rain",
                          "Area" = "area"), selected = names(stuData)),
   
      
      
      selectInput("ptColor", label = h3("Pick a Color For the Points"),
                  choices = list("Black" = "black",
                                 "Red" = "red",
                                 "Orange" = "orange",
                                 "Yellow" = "yellow",
                                 "Green" = "green",
                                 "Blue" = "blue",
                                 "Purple" = "purple"), selected = 1),
      
      
      selectInput("lmColor", label = h3("Pick a Color For the Linear Model"),
                  choices = list("Red" = "red",
                                "Black" = "black",
                                 "Orange" = "orange",
                                 "Yellow" = "yellow",
                                 "Green" = "green",
                                 "Blue" = "blue",
                                 "Purple" = "purple"), selected = 1),

      selectInput("wrColor", label = h3("Pick a Color For the Weighted Regression"),
                  choices = list("Blue" = "blue",
                                 "Black" = "black",
                                "Red" = "red",
                                 "Orange" = "orange",
                                 "Yellow" = "yellow",
                                 "Green" = "green",
                                 "Purple" = "purple"), selected = 1),
      downloadButton("downloadData", "Download")
    ),

    
    mainPanel(

      tabsetPanel(type = "tabs",
                  tabPanel("Data Overview", verbatimTextOutput("overview")),
                  tabPanel("Scatterplot", plotOutput("scatterplot")), #main scatterplot
                  tabPanel("Distribution", #plot x and y distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), #summary of the regression output
                  tabPanel("Data Preview", DT::dataTableOutput('mytable')), #create my datatable
                  div(img(src='forest.jpeg'), height = 100, width = 100)
        
                         
      )
   
    )
  )))
