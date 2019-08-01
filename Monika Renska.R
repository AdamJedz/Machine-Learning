library(shiny)
library(ISLR)
library(tidyverse)


#Bosotn to jeden z wbudowanych zbiorów danych do biblioteki MASS

college <- College

ui <- fluidPage(
  radioButtons("private",
               "Is private?",
               c("Yes" = "Yes",
                 "No" = "No")),
  plotOutput("plot1"),
  selectInput("var", "Variable",
              choices = college %>% select(-Private) %>% colnames()),
  plotOutput("plot2")
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    college %>% 
      filter(Private == input$private) %>% 
      ggplot(aes(x = Apps, y = PhD)) +
      geom_point(color = "brown4") +
      labs(title = "Number of applications received vs percentage of faculties with PhD's") +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5))
    
  })
  
  output$plot2 <- renderPlot({
    college %>% 
      ggplot(aes_string(x = "Private", y = input$var)) +
      geom_boxplot(aes(fill = Private)) + 
      labs(title = paste0("Comparison of ", input$var, " for private and public colleges.")) +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5))
  })
  
}

shinyApp(ui = ui, server = server)