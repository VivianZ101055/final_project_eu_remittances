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
library(base)
library(tidyverse)

data(wrld_simpl)

full_data <- readRDS("full_data.rds")

full_data$year <- as.numeric(full_data$year)

mydata <- readRDS("mydata.rds")

eunames <- readRDS("eunames.rds")

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames), ]

# Reordering full_data to match natural order from world_simpl, our Large
# Spatial Polygons Dataframe.

# target_order <- wrld_simpl_data@data$NAME
#   
# full_data <- full_data[match(target_order, full_data$country_name), ]

bins <- c(0,100,500,1000,3000,5000,7000,9000,Inf)

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
                      p("Welcome! This webpage looks at remittance flows in the EU from the 2000 to 2018.",
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
                          ))
                        )
                      ),

             navbarMenu("Remittance Maps",
                        tabPanel("Inflows",
                                 sidebarLayout(sidebarPanel(
                                   selectInput("myyearinflows", label = "Year",
                                               choices = c(2000:2018), 
                                               selected = 2010)),
                                   mainPanel(leafletOutput("inflows", 
                                                           width = 900, 
                                                           height = 600)))),
                        tabPanel("Outflows",
                                 sidebarLayout(sidebarPanel(
                                   selectInput("myyearoutflows", label = "Year",
                                               choices = c(2000:2018), 
                                               selected = 2010)),
                                   mainPanel(leafletOutput("outflows", 
                                                           width = 900, 
                                                           height = 600))))
                        )))

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
    p <- ggplot(mydata, aes(x = as.numeric(year), y = sum_remittance_usd)) +
      geom_line(color = "light blue") +
      geom_point(aes(text = paste0("Year: ", year, "\n",
                              "Total Remittances: $",
                              round(sum_remittance_usd,0),
                              " Mln", sep="")), color = "dark blue") +
      labs(title = "Total remittances flowing into the EU, by year",
           x = "Year",
           y = "Total Remittances (in Millions of USD)") +
      scale_x_continuous(limits = c(1980, 2018),
                         breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010,
                                    2015),
                         labels = c("1980", "1985", "1990", "1995",
                                    "2000", "2005", "2010",
                                    "2015")) +
      scale_y_continuous(limits = c(0, 150000),
                         breaks = c(0, 10000, 20000, 30000,
                                    40000, 50000, 60000,
                                    70000, 80000, 90000,
                                    100000, 110000, 120000, 130000,
                                    140000, 150000),
                         labels = c("0", "10000", "20000", "30000", "40000",
                                    "50000", "60000", "70000", "80000",
                                    "90000", "100000", "110000", "120000",
                                    "130000", "140000", "150000")) +
      theme_classic()

    # Generates the plot with text hovering feature

    ggplotly(p, tooltip = "text")

    })

  
  map_data_react <- reactive({
    
    full_data %>% dplyr::filter(year == input$myyearinflows) %>%
      na.omit()
    
  })
  
  output$inflows <- renderLeaflet({
    
    pal_val <- map_data_react()
    
    pal <- colorBin(palette = "YlOrRd", domain = pal_val$remittances_in_usd,
                    bins = bins)
    
    leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                      minZoom = 3.2,
                                                      maxZoom = 6)) %>%
      addProviderTiles("CartoDB") %>%
      setView(30, 55, 3) %>%
      setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70) %>%
      addPolygons(
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  fillColor = ~pal(as.numeric(pal_val$remittances_in_usd)),
                  fillOpacity = 1,
                  label = ~paste0("Country: ", 
                                  wrld_simpl_data@data$NAME, ", ", 
                                  "Total Remittances: $", 
                                  round(pal_val$remittances_in_usd,0),
                                  " Mln", sep=""),
                  highlight = highlightOptions(weight = 3, color = "white", 
                                               bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
                title = "Remittances in Millions of USD", 
                position = "bottomright")
    
  })
  
  map_data <- reactive({
    
    full_data %>% dplyr::filter(year == input$myyearoutflows) %>%
      na.omit()
    
  })
  
  output$outflows <- renderLeaflet({
    
    pal_val <- map_data()
    
    pal <- colorBin(palette = "YlOrRd", domain = pal_val$remittances_outflows,
                    bins = bins)
    
    leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                      minZoom = 3.2,
                                                      maxZoom = 6)) %>%
      addProviderTiles("CartoDB") %>%
      setView(30, 55, 3) %>%
      setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70) %>%
      addPolygons(
        weight = 2,
        opacity = 1,
        color = "black",
        fillColor = ~pal(as.numeric(pal_val$remittances_outflows)),
        fillOpacity = 1,
        label = ~paste0("Country: ", 
                        wrld_simpl_data@data$NAME, ", ", 
                        "Total Remittances: $", 
                        round(pal_val$remittances_outflows,0),
                        " Mln", sep=""),
        highlight = highlightOptions(weight = 3, color = "white", 
                                     bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~full_data$remittances_outflows, 
                opacity = 0.7,
                title = "Remittances in Millions of USD", 
                position = "bottomright")
    
  })
  
}

shinyApp(ui = ui, server = server)