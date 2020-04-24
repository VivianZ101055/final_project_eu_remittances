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

data(wrld_simpl)

## PART ONE: DATA WRANGLING

#---------------------------------------------------------------------
#----------------------EU Facts Scraped from Online-------------------

eumemberinfo <- read_html('https://en.wikipedia.org/wiki/Member_state_of_the_European_Union', skip = 0) %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table()

eumemberinfo <- eumemberinfo[[1]]

# Removing Wikipedia annotations

eumemberinfo$Name <- str_remove_all(eumemberinfo$Name, "\\[.*\\]")

# Select only the first four numbers to get the accession year

eumemberinfo$Accession <- substr(eumemberinfo$Accession, 0, 4)

# Changing the names because Eastern European countries are labeled
# differently on the two datasets

eumemberinfo$Name[eumemberinfo$Name == "Czechia"] <- "Czech Republic"

eumemberinfo$Name[eumemberinfo$Name == "Slovakia"] <- "Slovak Republic"

name <- eumemberinfo %>%
  select(Name)

names(name) <- c("sender")

#-------------------------------------------------------------------
#----------------------Remittances in USD---------------------------

# Load remittances in USD in a given country, in alphabetical order

# FLAG

usd_inflows <- read_excel("data/april2020_remittanceinflows.xlsx", skip = 0)

# Trying to get rid of the x in front of the data
# Filter to only show between 1980 to present
# Repeatedly getting the error that argument "pattern" is missing, with no
# default
# Convert USD to Euros - maybe later

usd_inflows <- usd_inflows %>%
  pivot_longer(
    cols = `1980`:`2018`,
    names_to = "year",
    names_prefix = "yr"
  ) %>%
  clean_names() %>%
  rename("country_name" = migrant_remittance_inflows_us_million)

usd_inflows <- usd_inflows[,-c(2:3)]

# join this with the eumemberinfo to only look at member states

full_data <- usd_inflows %>%
  inner_join(eumemberinfo, by = c("country_name" = "Name")) %>%
  rename(remittances_in_usd = value) %>%
  clean_names %>%
  filter(country_name %in% name$sender)

# Load remittances as percentage of GDP for a given country

# FLAG

percent_gdp <- read_excel("data/remittance_percentgdp.xls", skip = 2)

percent_gdp <- percent_gdp %>%
  pivot_longer(
    cols = `1980`:`2018`,
    names_to = "year",
    names_prefix = "yr") %>%
  rename("remittances_percent_gdp" = value) %>%
  clean_names %>%
  filter(country_name %in% name$sender)

percent_gdp <- percent_gdp[,-c(2:20)]

# Join this with the previous data and select only the relevant columns

full_data <- full_data %>%
  inner_join(percent_gdp, by = c("country_name"="country_name", "year"="year"), suffix = c("_usd", "_gdp")) %>%
  select(country_name, accession, year, remittances_in_usd, remittances_percent_gdp)

# FLAG

remittance_outflows <- read_excel("data/remittance_outflow_april2020.xlsx", skip = 0)

remittance_outflows <- remittance_outflows %>%
  pivot_longer(
    cols = `1980`:`2018`,
    names_to = "year",
    names_prefix = "yr"
  ) %>%
  clean_names() %>%
  rename("country_name" = migrant_remittance_outflows_us_million)

remittance_outflows <- remittance_outflows[,-c(2)]

full_data <- full_data %>%
  inner_join(remittance_outflows, by = c("country_name"="country_name", "year"="year")) %>%
  rename(remittances_outflows = value) %>%
  select(country_name, accession, year, remittances_in_usd, remittances_percent_gdp, remittances_outflows)

# Setup for ggplot of Remittances in USD

mydata <- full_data %>%
  na.omit %>%
  group_by(year) %>%
  summarize(sum_remittance_usd = sum(remittances_in_usd)) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year)))

# full_data <- full_data %>%
#   filter(year == 2000)

# For naming consistency

full_data$country_name <- gsub('Slovak Republic', 'Slovakia', full_data$country_name)

eumemberinfo$Name[eumemberinfo$Name == "Slovak Republic"] <- "Slovakia"

eunames <- eumemberinfo %>%
  pull(Name)

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames), ]

# Reordering full_data to match natural order from world_simpl, our Large
# Spatial Polygons Dataframe.

target_order <- wrld_simpl_data@data$NAME

# require(gdata)
# full_data <- reorder.factor(full_data$country_name, new.order=target_order)

full_data <- full_data[match(target_order, full_data$country_name), ]

bins <- c(0,10,20,50,100,200,500,1000,5000,10000)
pal <- colorBin("YlOrRd", domain = full_data$remittances_in_usd, bins = bins)

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
                      h4("This is a map of inflows."),
                      
                      sidebarPanel(
                        sliderInput("year_inflows", "Select Year", min = 1980, max = 2018,
                                    value = 2000, sep = ""),
                        mainPanel(
                          leafletOutput("inflows")
                        )
                    
                      ))),
                      
                  
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
                      # ),
      
                   
                      # leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                      #                                                   minZoom = 3,
                      #                                                   maxZoom = 6)) %>%
                      #   addProviderTiles("CartoDB") %>%
                      #   setView(30, 55, 3) %>%
                      #   addPolygons(weight = 2,
                      #               opacity = 1,
                      #               fillColor = ~pal(full_data$remittances_in_usd),
                      #               fillOpacity = 1,
                      #               color = "black",
                      #               label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
                      #                               "Total Remittances: $", round(full_data$remittances_in_usd,0), " Mln", sep=""),
                      #               labelOptions = labelOptions(
                      #                 style = list("font-weight" = "normal", padding = "3px 8px"),
                      #                 textsize = "12px",
                      #                 direction = "auto"
                      #               ),
                      #               highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
                      #   addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
                      #             title = "Remittances in Millions of USD", position = "bottomright") %>%
                      #   setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)),
  #            tabPanel("Outflows",
  #                     leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
  #                                                                       minZoom = 3,
  #                                                                       maxZoom = 6)) %>%
  #                       addProviderTiles("CartoDB") %>%
  #                       setView(30, 55, 3) %>%
  #                       addPolygons(weight = 2,
  #                                   opacity = 1,
  #                                   fillColor = ~pal(full_data$remittances_outflows),
  #                                   fillOpacity = 1,
  #                                   color = "black",
  #                                   label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
  #                                                   "Total Remittances: $", round(full_data$remittances_outflows,0), " Mln", sep=""),
  #                                   labelOptions = labelOptions(
  #                                     style = list("font-weight" = "normal", padding = "3px 8px"),
  #                                     textsize = "12px",
  #                                     direction = "auto"
  #                                   ),
  #                                   highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
  #                       addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7, 
  #                                 title = "Remittances in Millions of USD", position = "bottomright") %>%
  #                       setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)))
  # ))

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
  
  # output$value <- renderPrint({ full_data$year })
  
  output$inflows <- renderLeaflet({
    
    full_data$year <- as.numeric(input$year_inflows)
    
    leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                      minZoom = 3,
                                                      maxZoom = 6)) %>%
      addProviderTiles("CartoDB") %>%
      setView(30, 55, 3) %>%
      addPolygons(weight = 2,
                  opacity = 1,
                  fillColor = ~pal(full_data$remittances_in_usd),
                  fillOpacity = 1,
                  color = "black",
                  label = ~paste0("Country: ", wrld_simpl_data@data$NAME, ", ",
                                  "Total Remittances: $", round(full_data$remittances_in_usd,0), " Mln", sep=""),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "12px",
                    direction = "auto"
                  ),
                  highlight = highlightOptions(weight = 3, color = "white", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~full_data$remittances_in_usd, opacity = 0.7,
                title = "Remittances in Millions of USD", position = "bottomright") %>%
      setMaxBounds(lng1 = 15, lat1 = 35, lng2 = 20, lat2 = 70)
  })
  
}

shinyApp(ui = ui, server = server)
