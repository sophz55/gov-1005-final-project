library(fs)
library(zoo)
library(janitor)
library(rmarkdown)
library(gganimate)
library(tidyverse)

# this R script prepares all of the data and graphics for the shiny app 
# by sourcing and rendering other files!

source("setup.R")

source("income.R")

render(input = "maps.Rmd", 
       output_format = "html_document", 
       output_dir = "housing-explorations-app",
       output_file = "interactive_maps.html")
