# This file downloads and cleans our housing data, and writes it into a csv for the shiny app.

# get_clean_data takes one parameter, a string type.
# This function downloads and cleans raw data, and returns a local version of the df.

get_clean_data <- function(type) {

  url = case_when(
    type == "zhvi" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv",
    type == "sale_prices" ~ "http://files.zillowstatic.com/research/public/Zip/Sale_Prices_Zip.csv",
    type == "zri" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_Zri_AllHomesPlusMultifamily.csv",
    type == "rental_prices" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPrice_AllHomes.csv",
    type == "sale_list_prices_sqft" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_MedianListingPricePerSqft_AllHomes.csv",
    type == "rent_list_prices_sqft" ~ "http://files.zillowstatic.com/research/public/Zip/Zip_MedianRentalPricePerSqft_AllHomes.csv",
    type == "median_income" ~ "http://files.zillowstatic.com/research/public/Affordability_Income_2018Q4.csv"
  )
  download.file(url = url, destfile = "raw-data/data.csv")

  ifelse(type %in% c("zhvi", "zri", "rental_prices", "sale_list_prices_sqft", "rent_list_prices_sqft"),
         df <- read_csv("raw-data/data.csv",
                        col_types = cols(
                          .default = col_double(),
                          City = col_character(),
                          State = col_character(),
                          Metro = col_character(),
                          CountyName = col_character()
                        )),
         ifelse(type == "sale_prices",
                df <- read_csv("raw-data/data.csv",
                               col_types = cols(
                                 .default = col_double(),
                                 StateName = col_character()
                               )),
                df <- read_csv("raw-data/data.csv",
                               col_types = cols(.default = col_double(),
                                                RegionName = col_character()))))
  
  clean_df <- clean(df, type)
  return(clean_df)
}

# this function is a helper to the get_clean_data function, and cleans the data

clean <- function(df, type) {

  x <- df %>%
    pivot_longer(cols = contains("-"),
                 names_to = "date",
                 values_to = type) %>%
    clean_names()
  
  if (type %in% c("zhvi", "zri", "rental_prices", "sale_list_prices_sqft", "rent_list_prices_sqft")) {
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
            select(-size_rank)
  )
  
  x$date <- as.Date(as.yearmon(x$date))
  
  return (x)
}


dir_create("raw-data")

zhvi <- get_clean_data("zhvi")
sale_prices <- get_clean_data("sale_prices")
zri <- get_clean_data("zri")
rental_prices <- get_clean_data("rental_prices")
sale_list_prices_sqft <- get_clean_data("sale_list_prices_sqft")
rent_list_prices_sqft <- get_clean_data("rent_list_prices_sqft")
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
                   "date")) %>% 
  full_join(sale_list_prices_sqft,
            by = c("zip_code", 
                   "city", 
                   "state", 
                   "metro", 
                   "county_name", 
                   "date")) %>%
  full_join(rent_list_prices_sqft,
            by = c("zip_code", 
                   "city", 
                   "state", 
                   "metro", 
                   "county_name", 
                   "date")) %>% 
  select(-region_id, -state_name)

dir_create("clean-data")
all_housing_data %>% 
  write_csv("clean-data/all_housing_data.csv")

mean_housing_data <- all_housing_data %>%
  group_by(metro, date) %>%
  summarize(
    mean_zhvi = mean(zhvi, na.rm = TRUE),
    mean_sale_price = mean(sale_prices, na.rm = TRUE),
    mean_zri = mean(zri, na.rm = TRUE),
    mean_rental_price = mean(rental_prices, na.rm = TRUE),
    mean_sale_list_price_sqft = mean(sale_list_prices_sqft, na.rm = TRUE),
    mean_rent_list_price_sqft = mean(rent_list_prices_sqft, na.rm = TRUE)
  ) %>% 
  full_join(median_income, by = c("metro", "date")) %>% 
  pivot_longer(cols = c(mean_zhvi, 
                        mean_sale_price, 
                        mean_zri, 
                        mean_rental_price, 
                        mean_sale_list_price_sqft,
                        mean_rent_list_price_sqft,
                        median_income),
               names_to = "value_type",
               values_to = "mean_value")

mean_housing_data %>% 
  write_csv(path = "housing-explorations-app/mean_housing_data.csv")