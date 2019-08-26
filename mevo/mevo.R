library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)

mevo <- read_csv("./mevo.csv")%>% 
  mutate(start_district = str_replace_all(start_district, "<b3>", "l"),
         start_district = str_replace_all(start_district, "<f1>", "n"),
         start_district = str_replace_all(start_district, "<8c>", "S"),
         start_district = str_replace_all(start_district, "<f3>", "o"),
         start_district = str_replace_all(start_district, "<9c>", "s"),
         start_district = str_replace_all(start_district, "<b9>", "a"),
         start_district = str_replace_all(start_district, "<af>", "Z"),
         start_district = str_replace_all(start_district, "<ea>", "e"),
         start_district = str_replace_all(start_district, "<9f>", "z"),
         start_district = str_replace_all(start_district, "<bf>", "z"),
         end_district = str_replace_all(end_district, "<b3>", "l"),
         end_district = str_replace_all(end_district, "<f1>", "n"),
         end_district = str_replace_all(end_district, "<8c>", "S"),
         end_district = str_replace_all(end_district, "<f3>", "o"),
         end_district = str_replace_all(end_district, "<9c>", "s"),
         end_district = str_replace_all(end_district, "<b9>", "a"),
         end_district = str_replace_all(end_district, "<af>", "Z"),
         end_district = str_replace_all(end_district, "<ea>", "e"),
         end_district = str_replace_all(end_district, "<9f>", "z"),
         end_district = str_replace_all(end_district, "<bf>", "z")) 


ui <- dashboardPage(
  dashboardHeader(
    title = "Wypożyczenia MEVO"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa wypożyczeń", tabName = "map"),
      menuItem("Statystyki", tabName = "stats")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        column(width = 4,
               box(width = NULL,
                   selectInput(inputId = "district", 
                               label = "Dzielnica początkowa",
                               choices = mevo$start_district %>% unique() %>% sort())),
               box(width = NULL,
                   checkboxInput(inputId = "foreign",
                                 label = "Przejazdy międzydzielnicowe",
                                 value = T)),
               box(width = NULL,
                   radioButtons(inputId = "pora",
                                label = "Pora dnia",
                                choices = mevo$pora_dnia %>% unique())),
               box(width = NULL,
                   selectInput(inputId = "day",
                               label = "Dzień",
                               choices = mevo$dzien_tygodnia %>% unique()))),
        column(width = 8,
               box(width = NULL,
                   leafletOutput("my_map", height = 800)))
      )
    )
  )
)

server <- function(input, output, session) {
  output$my_map <- renderLeaflet({
    mevo %>% 
      filter(
        start_district == input$district,
        if(input$foreign) {start_district != end_district}else{start_district == end_district},
        pora_dnia == input$pora,
        dzien_tygodnia == input$day
      ) %>% 
      leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(lng = ~lng.x, lat = ~lat.x, color = "green", radius = 2.5, opacity = .25, label = ~station.x) %>% 
      addCircleMarkers(lng = ~lng.y, lat = ~lat.y, color = "deeppink", radius = 1, opacity = .25, label = ~station.y)
  })
}

shinyApp(ui, server)