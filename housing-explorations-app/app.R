library(shiny)
library(markdown)
library(tidyverse)

all_housing_data <- read_csv("all_housing_data.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Housing Explorations in US Areas of Interest",
  tabPanel("Explore",
           fluidPage(
             titlePanel("Housing Price Comparisons"),
             sidebarLayout(sidebarPanel(selectInput(
               "measure",
               "Measure",
               c(
                 "Zillow Home Value Index" = "mean_zhvi",
                 "Median Sales Price" = "mean_sale_price",
                 "Rental Values" = "mean_zri",
                 "Rent List Prices" = "mean_rental_price"
               )
             )),
             mainPanel(plotOutput("line_plot")))
           )),
  tabPanel("Income and Housing Price Comparisons"),
  tabPanel("About", includeMarkdown("about.md"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$line_plot <- renderPlot({
    type <- case_when(
      input$measure == "mean_zhvi" ~ "ZHVI",
      input$measure == "mean_sale_price" ~ "Sale Price",
      input$measure == "mean_zri" ~ "ZRI",
      input$measure == "mean_rental_price" ~ "Rental Price",
    )
    
    all_housing_data$metro <- factor(all_housing_data$metro,
                                     levels = c("New York-Newark-Jersey City", 
                                                "San Francisco-Oakland-Hayward", 
                                                "Seattle-Tacoma-Bellevue"),
                                     labels = c("New York Area",
                                                "San Francisco Area",
                                                "Seattle Area"))
    
    all_housing_data %>%
      filter(value_type == input$measure) %>% 
      drop_na() %>% 
      ggplot(aes(x = date,
                 y = mean_value,
                 group = metro,
                 color = metro)) +
      geom_line() +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = (paste("Average", type, "By Month")),
           x = "Year",
           y = paste("Mean", type),
           color = "Region")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
