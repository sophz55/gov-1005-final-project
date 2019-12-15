library(shiny)
library(markdown)
library(tidyverse)

mean_housing_data <- read_csv("mean_housing_data.csv",
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
                 "Median Sale List Price/Sq Ft" = "mean_sale_list_price_sqft",
                 "Rental Values" = "mean_zri",
                 "Median Rent List Price/Sq Ft" = "mean_rent_list_price_sqft",
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
           titlePanel("Mean Zillow House Value Indices by Zipcode"),
           mainPanel(
             HTML(
               paste(
                 "I have put together a map of the three regions of interest over three decades, 
                 taking average ZHVI by zipcode from October of 1999, 2009, and 2019. 
                 Click on a zip code for details, including the ZHVI and sale list price/sq ft 
                 (the latter information was only available starting in 2019, 
                 but I wanted to include it for sake of including a more normalized variable.)"
               )
             ),
             htmlOutput("interactive_maps"))),
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
      input$measure == "mean_sale_list_price_sqft" ~ "Mean Sale List Price/Sq Ft",
      input$measure == "mean_rent_list_price_sqft" ~ "Mean Rent List Price/Sq Ft",
      input$measure == "median_income" ~ "Median Income"
    )
    
    mean_housing_data$metro <- factor(mean_housing_data$metro,
                                     levels = c("New York-Newark-Jersey City", 
                                                "San Francisco-Oakland-Hayward", 
                                                "Seattle-Tacoma-Bellevue"),
                                     labels = c("New York Area",
                                                "San Francisco Area",
                                                "Seattle Area"))
    
    mean_housing_data %>%
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
  
  output$interactive_maps <- renderUI({
    includeHTML("interactive_maps.html")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
