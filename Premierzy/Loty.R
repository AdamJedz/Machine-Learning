library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(maps)

data <- read_csv("./joined.csv")
airports <- read_csv("./airports.csv")%>% 
  select(ident, latitude_deg, longitude_deg, iso_country, city = municipality)

ui <- dashboardPage(
  dashboardHeader(
    title = "Loty Premierów RP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wykaz połączeń krajowych", tabName = "loty_pl"),
      menuItem("Wykaz połączeń Europejskich", tabName = "loty_eu"),
      menuItem("Ulubione maszyny", tabName = "maszyny"),
      menuItem("Statystyki", tabName = "statystyki")
    )),
    dashboardBody(
      tabItems(
        # Połączenia
        
        tabItem(
          tabName = "loty_pl",
          column(width = 4,
                 box(width = NULL, head = 100, title = "Premier",
                     radioButtons(inputId = "premier_1", label = NULL,
                                  choices = c("Donald Tusk" = "DT",
                                              "Ewa Kopacz" = "EK",
                                              "Beata Szydło" = "BS",
                                              "Mateusz Morawiecki" = "MM")))),
          column(width = 8,
                 box(width = NULL, plotOutput("polaczenia_pl", height = 800))
          )
        ),
        tabItem(
          tabName = "loty_eu",
          column(width = 4,
                 box(width = NULL, head = 100, title = "Premier",
                     radioButtons(inputId = "premier_2", label = NULL,
                                  choices = c("Donald Tusk" = "DT",
                                              "Ewa Kopacz" = "EK",
                                              "Beata Szydło" = "BS",
                                              "Mateusz Morawiecki" = "MM")))),
          column(width = 8,
                 box(width = NULL, plotOutput("polaczenia_eu", height = 800))
          )
        ),
        tabItem(
          tabName = "maszyny",
          column(width = 4,
                 box(width = NULL, head = 100, title = "Premier",
                     radioButtons(inputId = "premier_3", label = NULL,
                                  choices = c("Donald Tusk" = "DT",
                                              "Ewa Kopacz" = "EK",
                                              "Beata Szydło" = "BS",
                                              "Mateusz Morawiecki" = "MM")))),
          column(width = 8,
                 box(width = NULL, 
                     plotOutput("samoloty", height = 800)))
        ),
        tabItem(
          tabName = "statystyki",
          column(width = 4,
                 box(width = NULL, head = 100, title = "Premier",
                     radioButtons(inputId = "premier_4", label = NULL,
                                  choices = c("Donald Tusk" = "DT",
                                              "Ewa Kopacz" = "EK",
                                              "Beata Szydło" = "BS",
                                              "Mateusz Morawiecki" = "MM")))),
          column(width = 8,
                 fluidRow(
                   infoBoxOutput("ilosc"),
                   infoBoxOutput("ratio"),
                   infoBoxOutput("okres"),
                   infoBoxOutput("foreign")
                 ))
        )
      )
    )
  )
  

server <- function(input, output) {
  
  
  output$polaczenia_pl <- renderPlot({
    data %>%
      filter(kto == input$premier_1, iso_country_z == "PL", iso_country_do == "PL") %>%
      count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>%
      ggplot() +
      # kontury Polski
      geom_polygon(data = map_data("world") %>%
                     filter(region == "Poland"),
                   aes(long, lat, group=group),
                   color = "gray30", fill = "gray90") +
      geom_segment(aes(x = longitude_deg_z, xend = longitude_deg_do,
                       y = latitude_deg_z, yend = latitude_deg_do, alpha = n),  color = "red", show.legend = F)+
      geom_point(data =airports %>% 
                   filter(city %in% (data %>% 
                            filter(kto == input$premier_1, iso_country_z == "PL", iso_country_do == "PL") %>% 
                            count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_z)) | city %in% (data %>% 
                            filter(kto == input$premier_1, iso_country_z == "PL", iso_country_do == "PL") %>% 
                            count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_do)), 
                          iso_country == "PL"),
                 aes(longitude_deg, latitude_deg)) +
      geom_text( data = airports %>% filter(city %in% (data %>%
    filter(kto == input$premier_1, iso_country_z == "PL", iso_country_do == "PL") %>%
      count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_z)) | city %in% (data %>% 
                                                                                                                                  filter(kto == input$premier_1, iso_country_z == "PL", iso_country_do == "PL") %>% 
                                                                                                                                  count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_do)), 
    iso_country == "PL") %>%
                   group_by(city) %>% top_n(1, ident),
                 aes(longitude_deg, latitude_deg, label = city), size = 4, vjust = -.5) +
      theme(axis.text = element_blank(), panel.grid = element_blank(), plot.title = element_text(hjust = .5)) +
      labs(title= "Wykaz połączeń lotniczych po Polsce", x = "", y ="")
  })
  
  output$polaczenia_eu <- renderPlot({
    data %>% 
      filter(kto == input$premier_2, 
             iso_country_do != "PL", 
             between(latitude_deg_z, 35, 70), 
             between(latitude_deg_do, 35, 70),
             between(longitude_deg_z, -10, 30),
             between(longitude_deg_do, -10, 30)) %>% 
      count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% 
      ggplot() +
      borders("world", xlim = c(-10, 25), ylim = c(39, 70), fill = "gray90")+
      geom_segment(aes(x = longitude_deg_z, xend = longitude_deg_do,
                       y = latitude_deg_z, yend = latitude_deg_do, alpha = n),  color = "red", show.legend = F)+
      geom_point(data =airports %>% 
                   filter(between(latitude_deg, 35, 70), between(longitude_deg, -10, 30),city %in% (data %>% filter(kto == input$premier_2, 
                                                                                                                                                                          iso_country_do != "PL", 
                                                                                                                                                                          between(latitude_deg_z, 35, 70), 
                                                                                                                                                                          between(latitude_deg_do, 35, 70),
                                                                                                                                                                          between(longitude_deg_z, -10, 30),
                                                                                                                                                                          between(longitude_deg_do, -10, 30)) %>% 
                                                                                                                                                                   count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_do))),
                 aes(longitude_deg, latitude_deg)) +
      geom_text( data = airports %>% filter(between(latitude_deg, 35, 70), between(longitude_deg, -10, 30),city %in% (data %>%
                                                         filter(kto == input$premier_2,
                                                                iso_country_do != "PL",
                                                                between(latitude_deg_z, 35, 70),
                                                                between(latitude_deg_do, 35, 70),
                                                                between(longitude_deg_z, -10, 30),
                                                                between(longitude_deg_do, -10, 30)) %>%
                                                         count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_z)) | city %in% (data %>%
                                                                                                                                                                                     filter(kto == input$premier_2,
                                                                                                                                                                                            iso_country_do != "PL",
                                                                                                                                                                                            between(latitude_deg_z, 35, 70),
                                                                                                                                                                                            between(latitude_deg_do, 35, 70),
                                                                                                                                                                                            between(longitude_deg_z, -10, 30),
                                                                                                                                                                                            between(longitude_deg_do, -10, 30)) %>%
                                                                                                                                                                                     count(latitude_deg_z, longitude_deg_z, city_z, latitude_deg_do, longitude_deg_do, city_do) %>% pull(city_do))) %>%
                   group_by(city) %>% top_n(1, ident),
                 aes(longitude_deg, latitude_deg, label = city), size = 4, vjust = -.5) +
      theme(axis.text = element_blank(), panel.grid = element_blank(), plot.title = element_text(hjust = .5)) +
      labs(title = "Wykaz połączeń lotniczych po Europie", x = "", y = "")
  })
  
  output$samoloty <- renderPlot({
    data %>%
      filter(kto == input$premier_3) %>%
      count(pojazd) %>%
      ggplot(aes(x = reorder(pojazd, -n), y = n)) +
      geom_col(aes(fill = pojazd), show.legend = F) + scale_fill_manual(values = c("EMB" = "deepskyblue4", "Bell" = "firebrick4", "CASA" = "cadetblue3", "DT" = "cornflowerblue", "Gulfstream" = "darkcyan", "Inny" = "blueviolet", "MI8" = "darkslateblue", "Śmigłowiec W-3" = "deepskyblue2", "TU" = "goldenrod3", "YK" = "darkorange3"))+
      labs(title = "Ilość lotów poszczególną maszyną",
           x = "Nazwa pojazdu",
           y = "Ilość lotów") +
      theme(plot.title = element_text(hjust = .5),
            axis.text.x = element_text(size = 20))
  })
  
  output$ilosc <- renderInfoBox({
    infoBox(
      title = "Ilość podróży",
      value = data %>% filter(kto == input$premier_4) %>% nrow(), icon = icon("transfer", lib = "glyphicon")
    )
  })
  
  output$okres <- renderInfoBox({
    infoBox(
      title = "Okres urzędowania",
      value = paste0(case_when(input$premier_4 == "DT" ~ 2502,
                        input$premier_4 == "EK" ~ 420,
                        input$premier_4 == "BS" ~ 756,
                        TRUE ~ 622), " dni"), color = "red", icon = icon("calendar", lib = "glyphicon")
    )
  })
  
  output$ratio <- renderInfoBox({
    infoBox(
      title = "Ilość podróży / dzień",
      value = round((data %>% filter(kto == input$premier_4) %>% nrow()) / (case_when(input$premier_4 == "DT" ~ 2502,
                                                                                     input$premier_4 == "EK" ~ 420,
                                                                                     input$premier_4 == "BS" ~ 756,
                                                                                     TRUE ~ 622)) ,2),
      color = "green", fill = T
    )
  })
  
  output$foreign <- renderInfoBox({
    infoBox(
      title = "Procent podróży zagranicznych",
      value = round((data %>% filter(kto == input$premier_4, iso_country_do != "PL" | iso_country_z != "PL") %>% nrow())/ (data %>% filter(kto == input$premier_4) %>% nrow()), 2),
      color = "yellow", fill = T, icon = icon("euro", lib = "glyphicon")
    )
  })
}




shinyApp(ui = ui, server = server)