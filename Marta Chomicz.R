library(shiny)
library(tidyverse)
library(ISLR)

#Auto to jeden z wbudowanych zbiorów danych do biblioteki ISLR. Ta aplikacja pokazuje zależność pomięczy spalaniem benzyny, a przyspieszeniem oraz pórównuje wszystkie zmienne dla każdego z krajów.

auto <- Auto %>% 
  mutate(origin = factor(origin, levels = c(1,2,3), labels = c("USA", "EUR", "JAP")))

ui <- fluidPage(
  radioButtons("country",
               "Country",
               c("USA" = "USA",
                 "Europe" = "EUR",
                 "Japan" = "JAP")),
  plotOutput("plot1"),
  selectInput("var", "Variable",
              choices = auto %>% select(-c(origin, name, year)) %>% colnames()),
  plotOutput("plot2")
)


server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    auto %>% 
      filter(origin == input$country) %>% 
      ggplot(aes(x = mpg, y = acceleration)) +
      geom_point(color = "chartreuse4") +
      labs(title = "Miles per galon vs acceleration",
           subtitle = paste0("For ", input$country)) +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5))
    
  })
  
  output$plot2 <- renderPlot({
    auto %>% 
      ggplot(aes_string(x = "origin", y = input$var)) +
      geom_boxplot(aes(fill = origin)) + 
      labs(title = paste0("Comparison of ", input$var, " for all countries.")) +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5))
  })
  
}

shinyApp(ui = ui, server = server)