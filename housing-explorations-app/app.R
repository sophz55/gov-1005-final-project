library(shiny)
library(markdown)
library(tidyverse)

all_housing_data <- read_csv("all_housing_data.csv",
                             col_types = cols(.default = col_character(),
                                              date = col_date(format = "%Y-%m-%d"),
                                              mean_value = col_double()))

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Housing Explorations in US Areas of Interest",
  tabPanel("Explore",
           fluidPage(
             titlePanel("Housing Price Comparisons"),
             sidebarLayout(sidebarPanel(selectInput(
               "measure",
               "Measure",
               c("Zillow Home Value Index" = "mean_zhvi",
                 "Median Sales Price" = "mean_sale_price",
                 "Rental Values" = "mean_zri",
                 "Rent List Prices" = "mean_rental_price",
                 "Income" = "median_income"
               )
             )),
             mainPanel(plotOutput("line_plot")))
           )),
  tabPanel("Income and Housing Price Comparisons",
           titlePanel("Income"),
           mainPanel(plotOutput("animation"))),
  tabPanel("House Value Maps by Zip Code",
           mainPanel(htmlOutput("test"))),
  tabPanel("About", includeMarkdown("about.md"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$line_plot <- renderPlot({
    type <- case_when(
      input$measure == "mean_zhvi" ~ "Mean ZHVI",
      input$measure == "mean_sale_price" ~ "Mean Sale Price",
      input$measure == "mean_zri" ~ "Mean ZRI",
      input$measure == "mean_rental_price" ~ "Mean Rental Price",
      input$measure == "median_income" ~ "Median Income"
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
      labs(title = (paste(type, "By Month")),
           x = "Year",
           y = type,
           color = "Region")
  })
  
  output$animation <- renderImage({
    list(src = "income_graphic.gif",
         contentType = "image/gif",
         width = "80%")
  },
  deleteFile = FALSE)
  
  output$test <- renderUI({
    includeHTML("nyc_interactive.html")
    includeHTML("sf_interactive.html")
    includeHTML("sea_interactive.html")
  })
  output$tmap <- renderLeaflet({
    tm <- tm_shape(World) + tm_polygons("HPI", legend.title = "Happy Planet Index")
    tmap_leaflet(tm)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
