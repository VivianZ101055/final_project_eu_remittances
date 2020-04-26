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

eunames <- readRDS("eunames.rds")

mychoices <- readRDS("mychoices.rds")

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames), ]

bins <- c(0,100,500,1000,3000,5000,7000,10000,15000,25000,30000)

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
                      titlePanel(
                        textOutput("graph_inflow_title")
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("mycountry", label = "Country",
                                      choices = mychoices, selected = "Austria")
                          # h3("Interactive Graphs"),
                          # p("To learn more about the remittance data, hover over points on the graph.")
                        ),
                        mainPanel(
                          plotlyOutput("distPlot")
                        ))),

             navbarMenu("Remittance Maps",
                        tabPanel("Inflows",
                                 titlePanel(
                                   textOutput("inflow_map_title")),
                                 sidebarLayout(sidebarPanel(
                                   sliderInput("myyearinflows", label = "Year",
                                               min = 2000, max = 2018,
                                               value = 2018)),
                                   htmlOutput("inflow_map_description")),
                                 mainPanel(leafletOutput("inflows", 
                                                         width = 900, 
                                                         height = 600))
                                   ),
                        tabPanel("Outflows",
                                 titlePanel(
                                   textOutput("outflow_map_title")),
                                 sidebarLayout(sidebarPanel(
                                   sliderInput("myyearoutflows", label = "Year",
                                               min = 2000, max = 2018,
                                               value = 2018)),
                                   htmlOutput("outflow_map_description")),
                                 mainPanel(leafletOutput("outflows", 
                                                         width = 900, 
                                                         height = 600))
                        ))))

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
  
  #---------------------------------------------------------------------------
  #------------Inflow map title-----------------------------------------------
  
  output$graph_inflow_title <- renderText({
    paste("Yearly Remittance Flows in ", input$mycountry)
  })
  
  graph_react <- reactive({
    
    full_data %>% dplyr::filter(country_name == input$mycountry) %>%
      na.omit()
    
  })

  output$distPlot <- renderPlotly({
    
    mydata <- graph_react()
    
    p <- ggplot(mydata, aes(x = as.numeric(year), y = remittances_in_usd)) +
      geom_line(color = "light blue") +
      geom_point(aes(text = paste0("Year: ", year, "\n",
                              "Total Remittances: $",
                              round(remittances_in_usd,0),
                              " Mln", sep="")), color = "dark blue") +
      labs(title = paste("Remittance Inflows:", mydata$country_name),
           x = "Year",
           y = "Total Remittances (in Millions of USD)") +
      scale_x_continuous(limits = c(2000, 2018),
                         breaks = c(2000,2002,2004,2006,2008,2010,2012,2014,
                                    2016,2018),
                         labels = c("2000","2002","2004","2006","2008","2010",
                                    "2012","2014","2016","2018")) +
      theme_classic()

    # Generates the plot with text hovering feature

    ggplotly(p, tooltip = "text")

    })
  
  #---------------------------------------------------------------------------
  #------------Inflow map title-----------------------------------------------

  output$inflow_map_title <- renderText({
    paste("Total Amount of Remittance Inflows in ", input$myyearinflows)
  })

  #---------------------------------------------------------------------------
  #------------Inflow map description-----------------------------------------
  
  output$inflow_map_description <- renderText({
    HTML("<p><b>Description</b></br></p>
         <p>Select a year, then hover over an individual country to learn about that country's quantity of remittance inflows. Please note that all values are in USD.</p>")})
  
  map_data_react <- reactive({
    
    full_data %>% dplyr::filter(year == input$myyearinflows) %>%
      na.omit()
    
  })
  
  output$inflows <- renderLeaflet({
    
    pal_val <- map_data_react()
    
    pal <- colorBin(palette = "viridis", domain = pal_val$remittances_in_usd,
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
  
  #---------------------------------------------------------------------------
  #------------Outflow map title----------------------------------------------
  
  output$outflow_map_title <- renderText({
    paste("Total Amount of Remittance Outflows in ", input$myyearoutflows)
  })
  
  #---------------------------------------------------------------------------
  #------------Outflow map description----------------------------------------
  
  output$outflow_map_description <- renderText({
    HTML("<p><b>Description</b></br></p>
         <p>Select a year, then hover over an individual country to learn about that country's quantity of remittance outflows. Please note that all values are in USD.</p>")})
  
  
  map_data <- reactive({
    
    full_data %>% dplyr::filter(year == input$myyearoutflows) %>%
      na.omit()
    
  })
  
  output$outflows <- renderLeaflet({
    
    pal_val <- map_data()
    
    pal <- colorBin(palette = "viridis", domain = pal_val$remittances_outflows,
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