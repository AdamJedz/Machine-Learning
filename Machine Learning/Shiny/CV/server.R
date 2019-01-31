#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(caTools)
library(caret)
library(e1071)

dataset <- read_csv("Social_Network_Ads.csv") %>% 
  select(-'User ID', - Gender) %>% 
  mutate(Purchased = as.factor(Purchased))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  s <- reactive({as.numeric(input$split)})
  
  split <- reactive({sample.split(dataset$Purchased, SplitRatio = s())})
  
  training_set <- reactive({subset(dataset, split() == T) %>% 
      mutate_if(is.integer, scale) %>% 
      mutate_if(is.matrix, as.numeric)})
  
  test_set <- reactive({subset(dataset, split() == F) %>% 
      mutate_if(is.integer, scale) %>% 
      mutate_if(is.matrix, as.numeric)})
  
  output$training <- renderTable(training_set())
  
  
  
  classifier <- reactive({glm(formula = Purchased ~., 
                    data = training_set(),
                    family = binomial)})
  
  prob_pred <- reactive({predict(classifier(), type = 'response', newdata = test_set() %>% select(-Purchased))})
  y_pred <- reactive({ifelse(prob_pred() > .5, 1, 0)})
  output$prob <- renderText(y_pred())
  
  X1 <- reactive({seq(min(training_set()[, 1]) - 1, max(training_set()[, 1]) + 1, by = 0.01)})
  X2 <- reactive({seq(min(training_set()[, 2]) - 1, max(training_set()[, 2]) + 1, by = 0.01)})
  grid_set <- reactive({expand.grid(X1(), X2()) %>% 
      rename('Age' = "Var1", 'EstimatedSalary' = "Var2")})
  prob_grid = reactive({predict(classifier(), newdata = grid_set(), type = "response")})
  y_grid = reactive({ifelse(prob_grid() > .5, 1, 0)})
  
  
  output$train_plot <- renderPlot(ggplot()+
                                    geom_point(data = grid_set(), aes(x = Age, y = EstimatedSalary), color = ifelse(y_grid() == 1, 'springgreen3', 'tomato'))+
                                    geom_point(data = training_set(), aes(x = Age, y = EstimatedSalary), pch = 21, fill = ifelse(training_set() %>% pull(Purchased) == 1,  'green4', 'red3'), color = "black")+
                                    labs(title = "Training data plot"))
  output$test_plot <- renderPlot(ggplot()+
                                    geom_point(data = grid_set(), aes(x = Age, y = EstimatedSalary), color = ifelse(y_grid() == 1, 'springgreen3', 'tomato'))+
                                    geom_point(data = test_set(), aes(x = Age, y = EstimatedSalary), pch = 21, fill = ifelse(test_set() %>% pull(Purchased) == 1,  'green4', 'red3'), color = "black")+
                                    labs(title = "Test data plot"))
  cm <- reactive(({table("Actual" = test_set() %>% pull(Purchased), "Predicted" = y_pred())}))
  output$cm <- renderPrint(cm())
  
  svm_classifier <- reactive({svm(formula = Purchased ~., 
                    data = training_set(),
                    type = "C-classification",
                    kernel = input$method)})
  svm_y_pred <- reactive({predict(svm_classifier(), test_set())})

   svm_y_grid = reactive({predict(svm_classifier(), newdata = grid_set())})
  output$svm_train_plot <- renderPlot(ggplot()+
                                    geom_point(data = grid_set(), aes(x = Age, y = EstimatedSalary), color = ifelse(svm_y_grid() == 1, 'springgreen3', 'tomato'))+
                                    geom_point(data = training_set(), aes(x = Age, y = EstimatedSalary), pch = 21, fill = ifelse(training_set() %>% pull(Purchased) == 1,  'green4', 'red3'), color = "black")+
                                    labs(title = "Training data plot"))
})
