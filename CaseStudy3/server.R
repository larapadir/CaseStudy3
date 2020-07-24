#DS501 CaseStudy 3
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rsconnect)

stuData <- read.csv(file = "forestfires.csv")
stuData = stuData %>% mutate(month=recode(month, 
                                `jan`="1",
                                `feb`="2",
                                `mar`="3",
                                `apr`="4",
                                `may`="5",
                                `jun`="6",
                                `jul`="7",
                                `aug`="8",
                                `sep`="9",
                                `oct`="10",
                                `nov`="11",
                                `dec`="12"))

stuData = stuData %>% mutate(day=recode(day, 
                                          `sun`="1",
                                          `mon`="2",
                                          `tue`="3",
                                          `wed`="4",
                                          `thu`="5",
                                          `fri`="6",
                                          `sat`="7"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$overview <- renderText({
   "This data was used to predict the burned area of forest fires in the northwest region of Portugal.The data was best used to predict small fires, and it was unclear how many outliers there where when collecting such data.
    The 'Scatterplot' tab allows for a user to choose which two columns from the dataset to compare and see the relationship between. The 'Distribution' tab displays two histograms of the chosen dependant and independant variables.
    The 'Summary' tab outputs the linear regression model data.Lastly, the 'Data Preview' tab allows for the user to see the data that is being used for the scatterplot, histograms, and summary.
    The user can use the checkboxs on the left hand side to view certain columns at a time.The user may change the color of the points and lines on the plot if they choose.The user may also download the forest fire data if they choose as well.
    
    Description of each attribute in the data:
      X - x-axis spatial coordinate within the Montesinho park map: 1 to 9
      Y - y-axis spatial coordinate within the Montesinho park map: 2 to 9
      Month - Month of the year: 'Jan' to 'Dec'(January represented as 1 through December represented as 12)
      Day - Day of the week: 'Sun' to 'Sat' (Sunday represented as 1 through Saturday represented as 7)
      FFMC - FFMC index from the FWI system: 18.7 to 96.20
      DMC - DMC index from the FWI system: 1.1 to 291.3
      DC - DC index from the FWI system: 7.9 to 860.6
      ISI - ISI index from the FWI system: 0.0 to 56.10
      Temp - Temperature in Celsius degrees: 2.2 to 33.30
      RH - Relative humidity in %: 15.0 to 100
      Wind - Wind speed in km/h: 0.40 to 9.40
      Rain - Outside rain in mm/m2 : 0.0 to 6.4
      Area - The burned area of the forest (in ha): 0.00 to 1090.84
    
    Regression looks at the relationship of how an input variable effects the outcome. It specifies the relationship between a numerical dependant value and numerical independant values.
    Regression is used to predict values. Regression was the most suitable algorithm for the forest fire dataset.The model finds the best fit regression line by finding the best intercept and coefficient of 'x' values.
    When it uses the model for prediction, it predicts the 'y' value per value of 'x'. The model aims to forecast 'y' where the difference between the predicted value and real value is at it lowest."
    
  })

  # Regression output
  output$summary <- renderPrint({
    x <- input$xcord
    y <- input$ycord
      
    lobf <- lm(stuData[,y] ~ stuData[,x])
    names(lobf$coefficients) <- c("Intercept", input$var2)
    summary(lobf)
  })

  # Scatterplot output
  output$scatterplot <- renderPlot({
    x <- input$xcord
    y <- input$ycord

    plot(stuData[,x], stuData[,y], main="Scatterplot", xlab=x, ylab=y, pch=19, col=input$ptColor)
    abline(lm(stuData[,y] ~ stuData[,x]), col=input$lmColor)
    lines(lowess(stuData[,x],stuData[,y]), col=input$wrColor)
    
    }, height=400)
  
  
  # Histogram 1
  output$distribution1 <- renderPlot({
    x <- input$xcord
    y <- input$ycord
   stuData <- transform(stuData, month = as.numeric(month), 
              day = as.numeric(day))
    
    hist(stuData[,y], main="Histogram of Outcome", xlab=y,
                    col = input$ptColor)
  }, height=300, width=300)
  
  # Histogram 2
  output$distribution2 <- renderPlot({
    x <- input$xcord
    y <-input$ycord
    stuData <- transform(stuData, month = as.numeric(month), 
                         day = as.numeric(day))
    
    hist(stuData[,x], xlab= x,
         main = "Histogram of Input Variable",
        col = input$ptColor)
         
  }, height=300, width=300)
  
  
  # Data output with specific columns to display
  stu2 = stuData[sample(nrow(stuData), 100), ]
  output$mytable = DT::renderDataTable({
    DT::datatable(stu2[, input$show_vars, drop = FALSE])
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("forestfiresData.csv", sep = "")
    },
    content = function(file) {
      write.csv(stuData, file, row.names = FALSE)
    })
  
  
})
