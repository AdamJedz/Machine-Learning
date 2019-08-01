library(shiny)
library(tidyverse)
library(shinythemes)

sn <- read_csv("Social_Network_Ads.csv") %>% 
  mutate(Gender = as.factor(Gender)) %>% 
  select(-c(`User ID`, Purchased))

ui <- shinyUI(navbarPage("Adamiec",
              
              theme = shinytheme("united"),
              tabPanel("Zaliczenie",
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("gender", "Gender",
                               c("Male" = "Male",
                                 "Female" = "Female")),
                    selectInput("category", "Category", 
                              c("Age" = "Age",
                                "Salary" = "EstimatedSalary"))
                ),
                mainPanel(
                  
                  plotOutput("plot1")
                )
              )),
              tabPanel("Inny wykres",
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("range", "Zakres", min = min(sn$Age) - 5, max = max(sn$Age) + 5, value = c(min(sn$Age) - 5, max(sn$Age) + 5))
                  ),
                  mainPanel(
                    plotOutput("plot2")
                  )
                ))))

  
  



server <- function(input, output) {
  data <- reactive({sn %>% filter(Gender == input$gender)})
  #if(input$category == "age") {
    #p <- data %>% ggplot() + geom_boxplot(aes(y = Age)))
  #}
 
  output$plot1 <- renderPlot({
    color <- if(input$gender == "Male"){"dodgerblue4"}else{"deeppink3"}
    y_axis <- input$category
    data() %>% ggplot(aes_string(x = "Gender", y = y_axis)) + geom_jitter(color = color) + geom_boxplot(fill = color, color = color, alpha = .3) +
      labs(title = paste0("Distribution of ", input$category),
           subtitle = paste0("For all ", input$gender, "s")) +
      theme(plot.title = element_text(hjust = .5),
            plot.subtitle = element_text(hjust = .5))})
  
   output$plot2 <- renderPlot({
     sn %>% ggplot(aes(x = Age, y = EstimatedSalary, color = Gender)) + geom_point() + facet_grid(Gender~.) + coord_cartesian(xlim = c(input$range[1], input$range[2])) +
       scale_color_manual(values = c("deeppink3", "dodgerblue4")) + labs(title = "Wszystkie obserwacje z podziałem na płeć") + theme(plot.title = element_text(hjust = .5))

  })

  
}

shinyApp(ui = ui, server = server)