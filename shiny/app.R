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

mydata <- readRDS("mydata.rds")

eunames <- readRDS("eunames.rds")

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames), ]

# Reordering full_data to match natural order from world_simpl, our Large
# Spatial Polygons Dataframe.

target_order <- wrld_simpl_data@data$NAME

full_data <- full_data[match(target_order, full_data$country_name), ]

# bins <- c(0,10,20,50,100,200,500,1000,5000,10000)
# pal <- colorBin("YlOrRd", domain = full_data$remittances_in_usd, bins = bins)

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
                          ))
                        )
                      ),
  
  navbarMenu("Remittance Maps",
             # sidebarLayout(
             #     h3("Interactive Map for Remittance Inflows in Europe"),
             #     p("The World Bank sourced this data from the IMF Balance of Statistics database and data from central banks and national statistical agencies."),
             #     p("All amounts you see below are in US Dollars.")
             #   ),
             tabPanel("Inflows",
                      # h4("This is a map of inflows."),
                      
                      sidebarPanel(
                        sliderInput("year",label = strong("Year"),
                                    min = 1980, max = 2018, value = 2000,
                                    sep = ""),
                        mainPanel(
                          leafletOutput("inflows")
                        ))))))
                       # sliderInput("year_inflows", label = strong("Select Year"), min = 1980, max = 2018,
                                  #  value = 2000, sep = ""))),
                      # subset<-reactive({full_data %>% filter(year_inflows %in% input$year_inflows)}),
                      # leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                      #                                                     minZoom = 3,
                      #                                                     maxZoom = 6)) %>%
                      #     addProviderTiles("CartoDB") %>%
                      #     setView(30, 55, 3) %>%
                      #     addPolygons(weight = 2,
                      #                 opacity = 1,
                      #                 fillColor = ~pal(full_data$remittances_in_usd),
                      #                 fillOpacity = 1,
                      #                 color = "black",
                      #                 label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
                      #                                 "Total Remittances: $", round(subset()$full_data$remittances_in_usd,0), " Mln", sep=""),
                      #                 labelOptions = labelOptions(
                      #                   style = list("font-weight" = "normal", padding = "3px 8px"),
                      #                   textsize = "12px",
                      #                   direction = "auto"
                      #                 ),
                      #                 highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
                      #     addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
                      #               title = "Remittances in Millions of USD", position = "bottomright") %>%
                      #     setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)),
             
             # tabPanel("Outflows",
             #          leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
             #                                                            minZoom = 3,
             #                                                            maxZoom = 6)) %>%
             #            addProviderTiles("CartoDB") %>%
             #            setView(30, 55, 3) %>%
             #            addPolygons(weight = 2,
             #                        opacity = 1,
             #                        fillColor = ~pal(full_data$remittances_outflows),
             #                        fillOpacity = 1,
             #                        color = "black",
             #                        label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
             #                                        "Total Remittances: $", round(full_data$remittances_outflows,0), " Mln", sep=""),
             #                        labelOptions = labelOptions(
             #                          style = list("font-weight" = "normal", padding = "3px 8px"),
             #                          textsize = "12px",
             #                          direction = "auto"
             #                        ),
             #                        highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
             #            addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
             #                      title = "Remittances in Millions of USD", position = "bottomright") %>%
             #            setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)))))
             #          
                  
                      # fluidRow(
                      #   column(4,
                      #          
                      #          # Copy the line below to make a slider bar 
                      #          sliderInput("full_data$year", label = h3("Select Year"), min = min(full_data$year), 
                      #                      max = max(full_data$year), value = 2000)
                      #   )),
                      # hr(),
                      # 
                      # fluidRow(
                      #   column(4, verbatimTextOutput("value")),
                      #   column(4, verbatimTextOutput("range"))
                      # 

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
  
  # subset<-reactive({full_data %>% filter(year %in% input$year)})
  
  # output$evicmap <- renderLeaflet({
  #   x <- as.numeric(input$evicmapvar)
  #   labels <- sprintf(
  #     "<strong>%s</strong><br/>",
  #     # %g percent eviction rate
  #     sf$n, sf[[x]]
  #   ) %>% 
  #     lapply(htmltools::HTML)
  #   pal_val <- sf[[x]]
  #   
  #   pal<- colorNumeric("viridis", domain = pal_val)
  #   leaflet(sf) %>%
  #     addProviderTiles(providers$Stamen.Toner) %>%
  #     addPolygons(color = ~pal(pal_val),
  #                 fillColor = ~pal(pal_val),
  #                 highlight = highlightOptions(weight = 5,
  #                                              fillOpacity = 0.7,
  #                                              bringToFront = TRUE),
  #                 label = labels) %>% 
  #     addLegend(pal = pal, values = pal_val)
  # })
  
  output$inflows <- renderLeaflet({
    x <- as.numeric(input$year)
    
    pal_val <- full_data %>%
      filter(year == x)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d percent eviction rate",
      wrld_simpl_data@data$NAME, pal_val$remittances_in_usd
    ) %>%
      lapply(htmltools::HTML)
    
    # bins <- c(0,10,20,50,100,200,500,1000,5000,10000)
    pal <- colorNumeric("YlOrRd", domain = pal_val$remittances_in_usd)
    
    leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                      minZoom = 3,
                                                      maxZoom = 6)) %>%
      addProviderTiles("CartoDB") %>%
      setView(30, 55, 3) %>%
      addPolygons(weight = 2,
                  opacity = 1,
                  fillColor = ~pal(pal_val$remittances_in_usd),
                  fillOpacity = 1,
                  color = ~pal(pal_val$remittances_in_usd),
                  label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
                                  "Total Remittances: $", 
                                  round(pal_val$remittances_in_usd,0), 
                                  " Mln", sep=""),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"
                  ),
                  highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = pal_val$remittances_in_usd, opacity = 0.7,
                title = "Remittances in Millions of USD", position = "bottomright") %>%
      setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)})
  
  # output$value <- renderPrint({ full_data$year })
  
  # output$inflows <- renderLeaflet({
  #   
  #   full_data$year <- as.numeric(input$year_inflows)
  #   
  #   leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
  #                                                     minZoom = 3,
  #                                                     maxZoom = 6)) %>%
  #     addProviderTiles("CartoDB") %>%
  #     setView(30, 55, 3) %>%
  #     addPolygons(weight = 2,
  #                 opacity = 1,
  #                 fillColor = ~pal(full_data$remittances_in_usd),
  #                 fillOpacity = 1,
  #                 color = "black",
  #                 label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
  #                                 "Total Remittances: $", round(full_data$remittances_in_usd,0), " Mln", sep=""),
  #                 labelOptions = labelOptions(
  #                   style = list("font-weight" = "normal", padding = "3px 8px"),
  #                   textsize = "12px",
  #                   direction = "auto"
  #                 ),
  #                 highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
  #     addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
  #               title = "Remittances in Millions of USD", position = "bottomright") %>%
  #     setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)
  # })
  
}

shinyApp(ui = ui, server = server)