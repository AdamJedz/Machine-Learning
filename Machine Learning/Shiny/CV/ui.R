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
shinyUI(navbarPage("Adamiec",
  
  # Application title
  
  theme = shinytheme("united"),
  tabPanel("Logistic Regression",
  # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "split",
                     label = "Split:",
                     min = 0.01,
                     max = 0.99,
                    value = 0.8,
                    step = 0.01)),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
        tabPanel("Training Set",plotOutput("train_plot")),
        tabPanel("Test Set", verbatimTextOutput("cm"),
        plotOutput("test_plot"))
      
        )
      )
    )
  ),
  tabPanel("SVM",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "method",
                           label = "Kernel:",
                           choices = c("linear", "polynomial", "radial"),
                           selected = "linear")
             ),
             mainPanel(
               plotOutput("svm_train_plot")
             )
           )
    
  )
))
