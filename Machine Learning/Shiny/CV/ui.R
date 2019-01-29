#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Logistic Regression"),
  theme = shinytheme("united"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "seed",
                   label = "set seed:",
                   min = 0,
                   max =10000,
                   value = 123),
      sliderInput(inputId = "split",
                   label = "Split:",
                   min = 0.01,
                   max = 0.99,
                  value = 0.8,
                  step = 0.01)),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("cm"),
      plotOutput("train_plot"),
      plotOutput("test_plot")
      
    )
  )
)
)