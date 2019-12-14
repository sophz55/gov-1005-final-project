dir_create("raw-data")
dir_create("clean-data")
types = list("zhvi", "sale_price", "zri", "rental_price", "median_income")
types %>% map(get_clean_data)
dir_delete("raw-data")

get_clean_data <- function(type) {
  url = case_when(type == "zhvi" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv",
                  type == "sale_prices" ~ "http://files.zillowstatic.com/research/public/Zip/Sale_Prices_Zip.csv",
                  type == "zri" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zri_AllHomesPlusMultifamily.csv",
                  type == "rental_prices" ~"http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPrice_AllHomes.csv",
                  type == "median_income" ~ "http://files.zillowstatic.com/research/public/Affordability_Income_2018Q4.csv")
  download.file(url = url, destfile = "raw-data/data.csv")
  col_types = case_when(
    type %in% c("zhvi", "zri", "rental_prices") ~ cols())
  df <- read_csv("raw-data/data.csv",
                 col_types = col_types)
  clean(df, type) %>% 
    write_csv(path = paste("clean-data/", type, ".csv"))
}

clean <- function(df, type) {
  x <- df %>%
    pivot_longer(cols = contains("-"),
                 names_to = "date",
                 values_to = type) %>%
    clean_names()
  
  if (type %in% c("zhvi", "zri", "rental_price")){
    x <- x %>% 
      filter(metro %in% c("New York-Newark-Jersey City", 
                          "San Francisco-Oakland-Hayward", 
                          "Seattle-Tacoma-Bellevue"),
             zip_code != 12853)
  }
  
  ifelse (
    type == "median_income",
    x <- x %>%
      filter(str_detect(region_name, "New York|San Francisco|Seattle")) %>%
      separate(region_name,
               into = c("city", "state"),
               sep = ", ") %>%
      mutate(metro = case_when(city == "New York" ~ "New York-Newark-Jersey City",
                               city == "San Francisco" ~ "San Francisco-Oakland-Hayward",
                               city == "Seattle" ~ "Seattle-Tacoma-Bellevue")),
    x <- x %>%
      rename(zip_code = region_name) %>%
      select(-size_rank)) 
  
  x$date <- as.Date(as.yearmon(x$date))
  
  return (x)
}