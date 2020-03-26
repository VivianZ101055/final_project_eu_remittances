
library(shiny)

# to enable shiny toolkit, use fluidPage

ui <- fluidPage(
    plotlyOutput("distPlot")
)

# defining server for shinyApp function

server <- function(input, output) {
    output$distPlot <- renderPlotly({
        ggplot(mydata, aes(x = year, y = sum_remittance_usd)) +
            geom_line() +
            geom_point() +
            labs(title = "Total remittances flowing into the EU, by year",
                 # subtitle = "Plotting Remittances in USD, from 1980-2018",
                 # caption = "Source: World Bank",
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
            theme_classic() %>%
            add_trace(hovertemplate = "Year: $%{x}", "Total Remittances: $%{y}")
    })
}

shinyApp(ui = ui, server = server)
