library(shiny)
library(markdown)

# Define UI for application that draws a histogram
ui <- navbarPage("Housing Explorations in US Areas of Interest",
                 tabPanel("Explore",
                          fluidPage(
                              titlePanel("Housing Price Comparisons"),
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("measure", "Measure",
                                                  c("Zillow Home Value Index" = "mean_zhvi",
                                                    "Median Sales Price" = "mean_sales_price",
                                                    "Rental Values" = "mean_zri",
                                                    "Rent List Prices" = "mean_rental_price"))),
                                  mainPanel(
                                       plotOutput("line_plot")
                                  )
                              )
                          )
                 ),
                 tabPanel("Income and Housing Price Comparisons"),
                 tabPanel("About", includeMarkdown("about.md")))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$line_plot <- renderPlot({
        all_housing_data %>% 
            filter(!is.na(input$measure)) %>% 
            ggplot(aes(x = date, y = mean_sale_price, group = metro, color = metro)) +
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
