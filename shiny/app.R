
library(shiny)
library(shinythemes)
library(plotly)

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
             tabPanel("Inflows"),
             tabPanel("Outflows"))
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
      geom_line() +
      geom_point(aes(text = 
                       paste0("Year: ", year, "\n", 
                              "Total Remittances: $",
                              round(sum_remittance_usd/1000000000,0),
                              "B", sep=""))) +
      labs(title = "Total remittances flowing into the EU, by year",
           # subtitle = "Plotting Remittances in USD, from 1980-2018",
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

# to enable shiny toolkit, use fluidPage

# ui <- fluidPage(
#     plotlyOutput("distPlot")
# )

# defining server for shinyApp function

# server <- function(input, output) {
#     output$distPlot <- renderPlotly({
#         ggplot(mydata, aes(x = year, y = sum_remittance_usd)) +
#             geom_line() +
#             geom_point() +
#             labs(title = "Total remittances flowing into the EU, by year",
#                  # subtitle = "Plotting Remittances in USD, from 1980-2018",
#                  # caption = "Source: World Bank",
#                  x = "Year",
#                  y = "Total Remittances (in Billions of USD)") +
#             scale_x_continuous(limits = c(1980, 2018),
#                                breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010,
#                                           2015),
#                                labels = c("1980", "1985", "1990", "1995",
#                                           "2000", "2005", "2010",
#                                           "2015")) +
#             scale_y_continuous(limits = c(0, 120000000000),
#                                breaks = c(0, 10000000000, 20000000000, 30000000000,
#                                           40000000000, 50000000000, 60000000000,
#                                           70000000000, 80000000000, 90000000000,
#                                           100000000000, 110000000000, 120000000000),
#                                labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80",
#                                           "90", "100", "110", "120")) +
#             theme_classic() %>%
#             add_trace(hovertemplate = "Year: $%{x}", "Total Remittances: $%{y}")
#     })
# }

shinyApp(ui = ui, server = server)
