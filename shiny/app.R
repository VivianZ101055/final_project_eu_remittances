
library(shiny)
library(shinythemes)
library(plotly)
library(readxl)
library(janitor)
library(rvest)
library(magrittr)
library(base)
library(reshape2)
library(leaflet)
library(sp)
library(maptools)
library(maps)

library(tidyverse)

wrld_simpl_data %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1,
              color = "blue",
              label = ~paste0("Country: ", full_data$country_name, "\n",
                              "Total Remittances: ", full_data$remittances_in_usd),
              highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)) %>%
  colorNumeric(palette = "Blues",
               domain = log(full_data$remittances_in_usd))

load(url("http://spatial.nhh.no/R/etc/TM_WORLD_BORDERS_SIMPL-0.2.RData"))

# source("data.R")

data(world.cities)

data(wrld_simpl)

world.cities$country.etc[world.cities$country.etc == "Czechia"] <- "Czech Republic"

world.cities$country.etc[world.cities$country.etc == "Slovakia"] <- "Slovak Republic"

eu_capitals <- world.cities %>%
  filter(capital == 1) %>%
  mutate(country = as.character(country.etc)) %>%
  inner_join(eumemberinfo, by = c("country" = "Name")) %>%
  select(country, long, lat) %>%
  filter(long != 33.38)

# To prepare for the next step, creating a list of EU member countries.

eumemberinfo$Name[eumemberinfo$Name == "Slovak Republic"] <- "Slovakia"

eunames <- eumemberinfo %>%
  pull(Name)

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames),]

# australia.map < - world.map[world.map$NAME == "Australia",]
# plot(australia.map)

#-----------------------------------------------------------------
#--------------------------Shiny ui-------------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(tags$b("EU Remittance Flows"),
             
             tabPanel("About",
                      imageOutput("global_remittance", width = "100%", 
                                  height = "100%"),
                      h1(tags$b("EU Remittance Flows"), align = "center"),
                      p(tags$em("Models of Remittance Inflows and Outflows over the last 40 years"),
                        align = "center"),
                      p("Welcome! This webpage looks at remittance flows in the EU from the 1980s to present.",
                        align = "center"),
                      h2(tags$b("Contact")),
                      p("Hi! I am Vivian Zhang, a first year at Harvard College studying Economics with a secondary in Government!"),
                      p("You can reach me at vivianzhang@college.harvard.edu."),
                      p("The code for this project can be accessed from",
                        a(href="https://github.com/VivianZ101055", "my github repository.")),
                      br(),
                      hr("Acknowledgements:"),
                      p("Thank you to Preceptor David Kane and all the members of GOV 1005 for introducing me to data science and helping me on this project!")
             ),
             
             tabPanel("Graphs",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Interactive Graphs")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Remittances in USD",
                                     plotlyOutput("distPlot"))
                            ),
                          tabsetPanel(
                            tabPanel("Remittances as a Percentage of GDP")
                          ),
                          tabsetPanel(
                            tabPanel("Case Study: UK")
                          )
                          )
                        )
                      ),
  
  navbarMenu("Remittance Maps",
             tabPanel("Inflows",
                      leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                       minZoom = 3,
                                                       maxZoom = 6)) %>%
                        addProviderTiles("CartoDB") %>%
                        setView(30, 55, 3) %>%
                        addCircleMarkers(lng = eu_capitals$long,
                                   lat = eu_capitals$lat,
                                   label = eu_capitals$country,
                                   radius = 5,
                                   color = "blue") %>%
                        addPolygons(weight = 1,
                                    color = "blue",
                                    label = ~paste0("Country: ", full_data$country_name, "\n",
                                                    "Total Remittances: ", full_data$remittances_in_usd),
                                    highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)) %>%
                        colorNumeric(palette = "Blues",
                                     domain = log(full_data$remittances_in_usd)) %>%
                        setMaxBounds(lng1 = 15, lat1 = 35,
                                  lng2 = 20, lat2 = 70)),
             tabPanel("Outflows",
                      leaflet() %>%
                        addTiles() %>%
                        setView(30, 55, 3)))
  ))

#-----------------------------------------------
#--------------------Server---------------------

server <- function(input, output){
  
  output$global_remittance <- renderImage({
    list(src = "shiny_files/remittances.png",
         height = 300,
         width = 600,
         style = "display: block; margin-left: auto; margin-right:auto")
  }, deleteFile = FALSE
  )
  
  output$distPlot <- renderPlotly({
    p <- ggplot(mydata, aes(x = year, y = sum_remittance_usd)) +
      geom_line(color = "light blue") +
      geom_point(aes(text = 
                       paste0("Year: ", year, "\n", 
                              "Total Remittances: $",
                              round(sum_remittance_usd/1000000000,0),
                              "B", sep="")), color = "dark blue") +
      labs(title = "Total remittances flowing into the EU, by year",
           x = "Year",
           y = "Total Remittances (in Billions of USD)") +
      scale_x_continuous(limits = c(1980, 2018),
                         breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010,
                                    2015),
                         labels = c("1980", "1985", "1990", "1995",
                                    "2000", "2005", "2010",
                                    "2015")) +
      scale_y_continuous(limits = c(0, 120000000000),
                         breaks = c(0, 10000000000, 20000000000, 30000000000,
                                    40000000000, 50000000000, 60000000000,
                                    70000000000, 80000000000, 90000000000,
                                    100000000000, 110000000000, 120000000000),
                         labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80",
                                    "90", "100", "110", "120")) +
      theme_classic()
    
    # Generates the plot with text hovering feature
    
    ggplotly(p, tooltip = "text")
    
    })
}

shinyApp(ui = ui, server = server)
