library(fs)
library(zoo)
library(janitor)
library(rmarkdown)
library(tidyverse)

source("setup.R")

source("income.R")

render(input = "maps.Rmd", output_format = "html_document", output_dir = "housing-explorations-app/interactive_maps.html")
