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
library(gt)
library(broom)
library(base)
library(vembedr)
library(tidyverse)

data(wrld_simpl)

full_data <- readRDS("full_data.rds")

full_data$year <- as.numeric(full_data$year)

# full_data <- full_data %>% 
#   mutate(gdp = 100 * remittances_in_usd / remittances_percent_gdp / 100000)

# gdp_on_percent <- rstanarm::stan_glm(remittances_percent_gdp ~ gdp, data = full_data, refresh = 0)

full_data <- full_data %>% 
  filter(!is.na(remittances_in_usd), !is.na(remittances_percent_gdp)) %>% 
  mutate(gdp = 100 * remittances_in_usd / remittances_percent_gdp / 100000) %>%
  mutate(log_remittances_percent_gdp = log(remittances_percent_gdp), log_gdp = log(gdp))

gdp_on_percent <- lm(remittances_percent_gdp ~ log_gdp, data = full_data)

predicted <- data.frame(remittances_percent_pred = predict(gdp_on_percent, full_data), gdp = full_data$gdp)

eunames <- readRDS("eunames.rds")

mychoices <- readRDS("mychoices.rds")

# Considers only the polygons for the EU countries.

wrld_simpl_data <- wrld_simpl[which(wrld_simpl@data$NAME %in% eunames), ]

bins <- c(0,100,500,1000,3000,5000,7000,10000,15000,25000,30000)

regression <- lm(log_remittances_percent_gdp ~ log_gdp, data = full_data)

my_table <- regression %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high) %>%
  mutate_at(vars(estimate, conf.low, conf.high),
            list(~ round(., 2))) %>%
  gt() %>%
  tab_header(title = "Relationship Between GDP and Reliance on Remittance Inflows") %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate",
    conf.low = "Lower bound",
    conf.high = "Upper bound") %>%
  tab_spanner(
    label = "Both GDP and Remittance Inflow as a Percentage of GDP are logged",
    columns = vars(
      term, estimate, conf.low, conf.high
    ))

#-----------------------------------------------------------------
#--------------------------Shiny ui-------------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage("EU Remittance Flows",
             tabPanel("Flows",
                      h1("European Union Remittance Flows",
                         align = "center"),
                      h4(tags$em("Models of Remittance Inflows and Outflows over 
                                the last 18 years"),
                        align = "center"),
                      br(),
                      p("Remittances are transfers of money from migrant 
                      workers, generally to their home countries. According to
                      the UN, 1/9 people around the world are to some extent 
                      supported by remittances. Remittance trends in Europe are 
                      particularly interesting from 2000-2018. During this period,
                      the divide between Eastern and Western Europe became smaller. 
                      The European Union became a more integrated supranational 
                      organization and had to deal with the financial crisis of 2008.
                      This webpage is a visualization tool to compare remittance
                      inflows and outflows of EU member states during this eventful
                      period of 18 years."),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("mycountry", label = "Country",
                                      choices = mychoices, selected = "Austria"),
                          htmlOutput("graph_description")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Country Inflows",
                                      plotlyOutput("distPlot"))),
                          br(),
                          tabsetPanel(
                            tabPanel("Country Outflows",
                                      plotlyOutput("distOutflows"))),
                          br()
                          ))),

             navbarMenu("Remittance Maps",
                        tabPanel("Inflows",
                                 titlePanel(
                                   textOutput("inflow_map_title")),
                                 sidebarLayout(sidebarPanel(
                                   sliderInput("myyearinflows", label = "Year",
                                               min = 2000, max = 2018,
                                               value = 2018, sep = "")),
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
                                               value = 2018, sep = "")),
                                   htmlOutput("outflow_map_description")),
                                 mainPanel(leafletOutput("outflows", 
                                                         width = 900, 
                                                         height = 600))
                        )),
             tabPanel("Regressions",
                      h1("Modeling for Explanation",
                         align = "center"),
                      p(tags$em("Visualizing the Correlation Between Bilateral 
                                 Trade and Remittance Flows"),
                        align = "center"),
                      sidebarLayout(
                        sidebarPanel(
                          h4("Description"),
                          p("Regression Model 1 shows the general 
                            relationship between the size of the economy and 
                            reliance on remittances, based on data from 2000 to 
                            2018 in EU member states. Since this the data does 
                            not fit a linear model, I used a logarithmic model. 
                            Smaller economies tend to have higher 
                            reliance on remittances. When we get to medium 
                            economies, the correlation between remittance inflows 
                            and GDP becomes noticeably less strong. There is less 
                            of a jump in correlation when we move from medium to 
                            large economies than when we move from small to medium 
                            economies."),
                          p("Regression Model 2 uses the log scale on both the 
                            GDP and remittance inflow as a percentage of GDP 
                            variables. It tells us that countries with larger 
                            GDPs also tend to have larger inflows of remittances.
                            If we look at the Table to Understand Regression 
                            Model 2, we can say that a country with a 1% higher 
                            GDP tends to have a 0.37% smaller value for 
                            remittance inflows as a percentage of GDP."),
                          p("Obviously, there are many confounding variables and 
                            I am not making any causal claims in my project.")
                          ),
                        mainPanel(
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Regression Method 1",
                                       plotlyOutput("inflow_outflow"),
                                       br())),
                            tabsetPanel(
                              tabPanel("Regression Method 2",
                                       plotlyOutput("log_plot"),
                                       br())),
                            tabsetPanel(
                              tabPanel("Table to understand Regression Method 2",
                                       gt_output(outputId = "table"))))))),
             tabPanel("About",
                      imageOutput("global_remittance", width = "100%",
                                  height = "100%"),
                      h1(tags$b("EU Remittance Flows"), align = "center"),
                      p(tags$em("Models of Remittance Inflows and Outflows over 
                                the last 18 years"),
                        align = "center"),
                      h4(tags$b("About Me")),
                      p("Hi! I am Vivian Zhang, a first year at Harvard College 
                        studying Economics with a secondary in Computer Science! 
                        My email is vivianzhang@college.harvard.edu"),
                      p("View my LinkedIn",
                        a(href="https://www.linkedin.com/in/vivian-zhang-101055/",
                          "here.")),
                      p("The code for this project can be accessed from",
                        a(href="https://github.com/VivianZ101055", "my github 
                          repository.")),
                      h4(tags$b("Data Source")),
                      p("My data came from the World Bank's Remittance Data
                        sets, found",
                        a(href="https://www.worldbank.org/en/topic/migrationremittancesdiasporaissues/brief/migration-remittances-data", 
                          "here.")),
                      p("Some of my data also came from the World Bank Open
                        Data website, found",
                        a(href = "https://data.worldbank.org/", "here.")),
                      h4(tags$b("Acknowledgements:")),
                      p("Thank you to Preceptor David Kane, my Teaching Fellow
                      (June Hwang), and all the members of GOV 1005 for 
                      introducing me to data science and and R."),
                      fluidRow(column(4), column(4, 
                                                 embed_url("https://youtu.be/o6nmz1-PBmI"),
                                                 column(4)))
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
  
  #---------------------------------------------------------------------------
  #------------Inflow map title-----------------------------------------------
  
  output$graph_description <- renderText({
    HTML("<p><b>Description</b></br></p>
         <p>In the dropdown menu, select a country. Hover over points 
         on the graphs to see inflow and outflow quantities. A vertical red line 
         represents the year the country joined the European Union as a member 
         state. If the graph does not contain a vertical line, the state joined 
         the EU prior to the year 2000.</p>")})
  
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
      geom_vline(xintercept = as.numeric(mydata$accession), color = "red") +
      labs(title = paste("Remittance Inflows:", mydata$country_name),
           x = "Year",
           y = "Total Remittances (in Millions of USD)",
           caption = "Footnote") +
      scale_x_continuous(limits = c(2000, 2018),
                         breaks = c(2000,2002,2004,2006,2008,2010,2012,2014,
                                    2016,2018),
                         labels = c("2000","2002","2004","2006","2008","2010",
                                    "2012","2014","2016","2018")) +
      theme_classic()

    # Generates the plot with text hovering feature

    ggplotly(p, tooltip = "text")

    })
  
  output$distOutflows <- renderPlotly({
    
    mydata <- graph_react()
    
    p <- ggplot(mydata, aes(x = as.numeric(year), y = remittances_outflows)) +
      geom_line(color = "light blue") +
      geom_point(aes(text = paste0("Year: ", year, "\n",
                                   "Total Remittances: $",
                                   round(remittances_outflows,0),
                                   " Mln", sep="")), color = "dark blue") +
      geom_vline(xintercept = as.numeric(mydata$accession), color = "red") +
      labs(title = paste("Remittance Outflows:", mydata$country_name),
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
         <p>Select a year, then hover over an individual country to learn about 
         that country's quantity of remittance inflows. Please note that all 
         values are in USD. I deliberately kept the currency in USD, since the dataset
         did not specify the rates the World Bank used to convert rates from
         Euros and other European currencies to USD.</p>")})
  
  map_data_react <- reactive({
    
    full_data %>% filter(year == input$myyearinflows) %>%
      na.omit()
    
  })
  
  output$inflows <- renderLeaflet({
    
    pal_val <- map_data_react()
    
    pal <- colorBin(palette = "viridis", domain = pal_val$remittances_in_usd,
                    bins = bins)
    
    leaflet(wrld_simpl_data, options = leafletOptions(dragging = TRUE,
                                                      minZoom = 3.48,
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
                                                      minZoom = 3.45,
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

  output$inflow_outflow <- renderPlotly({
    
    myplot1 <- ggplot(full_data, aes(x = gdp, y = remittances_percent_gdp)) +
      geom_point(color = "blue", size = 0.5) + 
      geom_line(color = "red", data = predicted, 
                aes(x = gdp, y = remittances_percent_pred)) +
      labs(title = "Relationship Between the Size of the Economy and \n Reliance on Remittances",
           subtitle = "Observing the Correlation Between the Remittance Inflows and GDP",
           x = "GDP in Billions of US Dollars",
           y = "Remittance Inflow as a Percentage of GDP") +
      theme_classic()
    
  })
  
  output$log_plot <- renderPlotly({
    myplot2 <- ggplot(full_data, aes(x = log_gdp, y = log_remittances_percent_gdp)) +
      geom_point(color = "blue", size = 0.5) +
      geom_smooth(method = "glm", se = FALSE, color = "black") +
      labs(title = "Relationship Between the Size of the Economy and \n Reliance on Remittances",
           subtitle = "Observing the Correlation Between the Remittance Inflows and GDP",
           x = "Log of GDP in Billions of USD",
           y = "Log of Remittance Inflow as a Percentage of GDP") +
      theme_classic()
  })
  
  output$table <- render_gt(
    expr = my_table,
    height = px(600),
    width = px(700)
  )
  
  
}

shinyApp(ui = ui, server = server)