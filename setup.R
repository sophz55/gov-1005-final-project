get_clean_data <- function(type) {
  url = case_when(
    type == "zhvi" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv",
    type == "sale_prices" ~ "http://files.zillowstatic.com/research/public/Zip/Sale_Prices_Zip.csv",
    type == "zri" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zri_AllHomesPlusMultifamily.csv",
    type == "rental_prices" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPrice_AllHomes.csv",
    type == "median_income" ~ "http://files.zillowstatic.com/research/public/Affordability_Income_2018Q4.csv"
  )
  download.file(url = url, destfile = "raw-data/data.csv")
  
  ifelse(type %in% c("zhvi", "zri", "rental_prices"),
         df <- read_csv("raw-data/data.csv",
                        col_types = cols(
                          .default = col_double(),
                          RegionName = col_character(),
                          City = col_character(),
                          State = col_character(),
                          Metro = col_character(),
                          CountyName = col_character()
                        )),
         ifelse(type == "sale_prices",
                df <- read_csv("raw-data/data.csv",
                               col_types = cols(
                                 .default = col_double(),
                                 RegionName = col_character(),
                                 StateName = col_character()
                               )),
                df <- read_csv("raw-data/data.csv",
                               col_types = cols(.default = col_double(),
                                                RegionName = col_character()))))
  clean_df <- clean(df, type)
    
  clean_df %>% write_csv(path = paste0("clean_data/", type, ".csv"))
  
  return(clean_df)
}

clean <- function(df, type) {
  x <- df %>%
    pivot_longer(cols = contains("-"),
                 names_to = "date",
                 values_to = type) %>%
    clean_names()
  
  if (type %in% c("zhvi", "zri", "rental_prices")) {
    x <- x %>%
      filter(metro %in% c("New York-Newark-Jersey City",
                          "San Francisco-Oakland-Hayward",
                          "Seattle-Tacoma-Bellevue"))
  }
  
  ifelse (type == "median_income",
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
            select(zip_code, metro, city, state, date)
  )
  
  x$date <- as.Date(as.yearmon(x$date))
  
  return (x)
}


dir_create("raw-data")
dir_create("clean-data")

zhvi <- get_clean_data("zhvi")
sale_prices <- get_clean_data("sale_prices")
zri <- get_clean_data("zri")
rental_prices <- get_clean_data("rental_prices")
median_income <- get_clean_data("median_income")

dir_delete("raw-data")


all_housing_data <-
  left_join(zhvi, 
            sale_prices, 
            by = c("region_id", "zip_code", "date")) %>% 
  full_join(zri, 
            by = c("region_id", 
                   "zip_code", 
                   "city", 
                   "state", 
                   "metro", 
                   "county_name", 
                   "date")) %>%
  full_join(rental_prices, 
            by = c("zip_code", 
                   "city", 
                   "state", 
                   "metro", 
                   "county_name", 
                   "date"))

mean_housing_data <- all_housing_data %>%
  group_by(metro, date) %>%
  summarize(
    mean_zhvi = mean(zhvi, na.rm = TRUE),
    mean_sale_price = mean(sale_prices, na.rm = TRUE),
    mean_zri = mean(zri, na.rm = TRUE),
    mean_rental_price = mean(rental_prices, na.rm = TRUE)
  ) %>% 
  full_join(median_income, by = c("metro", "date")) %>% 
  pivot_longer(cols = c(mean_zhvi, 
                        mean_sale_price, 
                        mean_zri, 
                        mean_rental_price, 
                        median_income),
               names_to = "value_type",
               values_to = "mean_value")

mean_housing_data %>% 
  write_csv(path = "housing-explorations-app/all_housing_data.csv")