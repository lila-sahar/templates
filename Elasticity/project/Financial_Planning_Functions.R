#' ---
#' title: "Template: Elasticity Financial Functions"
#' author: "Lila Sahar and Juan Malaver"
#' output: github_document
#' ---
#' 

# 1.0 LIBRARIES & DATA ----

# core
library(tidyverse)
library(infer)
library(lubridate)
library(readxl)
library(haven)

# visuals
library(plotly)

# data paths
descriptions_path <- "raw_data_cereal_descriptions.csv"
prices_path <- "raw_data_cereal_prices.csv"
store_locations_path <- "demo.dta"


# 2.0 PREPROCESS DATA ----

# 2.1 Cleaning the Table ----

# selecting variables of choice, renaming the variables and standardizing label names
input_descriptions <- function(descriptions_path) {
  # importing file
  descriptions_tbl <- read_csv(descriptions_path) %>%
    select(UPC, DESCRIP) %>%
    rename(description = DESCRIP)
  
  saveRDS(object = descriptions_tbl, file = "../R/descriptions_tbl.rds")
  
  return(descriptions_tbl)
} 

# selecting the variables of choice, renaming the variable names
input_prices <- function(prices_path) {
  # importing file
  prices_tbl <- read_csv(prices_path) %>%
    select(STORE, UPC, WEEK, MOVE, QTY, PRICE, PROFIT, OK) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(
      store = STORE,
      week = WEEK,
      move = MOVE,
      qty = QTY,
      price = PRICE,
      profit = PROFIT
    )
  
  saveRDS(object = prices_tbl, file = "../R/prices_tbl.rds")
  
  return(prices_tbl)
}

# selecting the variables of choice, changing the format of the data
input_store_locations <- function(store_locations_path) {
  #importing file
  store_locations_tbl <- read_dta(store_locations_path) %>%
    rename(lon = long) %>%
    select(city, zip, lat, lon, store) %>%
    filter(city != "") %>%
    mutate(city = str_to_title(city)) %>%
    mutate(lat = format(lat / 10000, nsmall = 4)) %>%
    mutate(lon = format(lon / -10000, nsmall = 4))
  
  saveRDS(object = store_locations_tbl, file = "../R/store_locations_tbl.rds")
  
  return(store_locations_tbl)
}

# created a dates tibble to match up with the week numbers
input_dates <- function() {
  d <- as_date(7196)
  e <- as_date(7202)
  
  week <- seq(1, 400)
  
  start <- vector()
  start <- append(start, d)
  
  end <- vector()
  end <- append(end, e)
  
  for (i in 1:399) {
    start <- append(start, d + 7)
    end <- append(end, e + 7)
    d <- d + 7
    e <- e + 7
  }
  
  dates_tbl <- data.frame(week = week,
                          start = start,
                          end = end)
  
  saveRDS(object = dates_tbl, file = "../R/dates_tbl.rds")
  
  return(dates_tbl)
}

# 2.2 Joining the Tibbles ----

# joins prices, description, and dates table for financial stats
product_detail_tbl <- prices_tbl %>%
  inner_join(dates_tbl, by = c("week" = "week")) %>%
  inner_join(descriptions_tbl) %>%
  mutate(Year = year(end),
         Sales = price * move / qty) %>%
  filter(OK == 1,
         Sales > 0) %>%
  select(Year, description, move, price, qty, Sales, profit) %>%
  mutate(
    description = recode(
      description,
      `APPLE CINNAMON CHEER` = "Apple Cinnamon Cheerios",
      `APPLE CINNAMON CHERR` = "Apple Cinnamon Cheerios",
      CHEERIOS = "Cheerios",
      `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch",
      `G.M. FIBER ONE` = "Fiber One",
      `HONEY NUT CHEERIOS` = "Honey Nut Cheerios",
      KIX = "Kix",
      `NABISCO WHEAT N BRAN` = "Wheat 'N Bran",
      `SMORES CRUNCH CEREAL` = "S'mores Crunch",
      TOTAL = "Total",
      `TOTAL CORN FLAKES` = "Total Corn Flakes",
      `WHEATIES` = "Wheaties",
      `WHOLE GRAIN TOTAL` = "Whole Grain Total"
    ))

# 2.4 Modified Datasets ----

# this tibble looks for the cereal brands that were used for the nine years of the experiment
product_lookup_tbl <- product_detail_tbl %>%
  group_by(description) %>%
  summarize(Distinct_Year = n_distinct(Year),
            Sample_Size = n()) %>%
  filter(Distinct_Year == 9)

# this tibble joins the look-up table to the detail table to calculate sum of sales and profit
product_total_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl, by = "description") %>%
  group_by(Year) %>%
  summarize(Total_Revenue = sum(Sales),
            Total_GM = sum(profit))

# this tibble calculates our key statistics
product_summary_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl , by = "description") %>%
  group_by(Year, description) %>%
  summarize(Unit_Sales = sum(move), 
            Avg_Retail_Price = mean(price), 
            Bundle_Sales = sum(qty),
            Revenue = sum(Sales),
            Gross_Margin = sum(profit)) %>%
  arrange(description, Year) %>%
  left_join(product_total_tbl) %>%
  ungroup() %>%
  mutate(Revenue_Pct = Revenue / Total_Revenue,
         Gross_Margin_Pct = Gross_Margin / Total_GM,
         Unit_Sales_Growth = (Unit_Sales/dplyr::lag(Unit_Sales) - 1)) %>%
  select(Year, description, Unit_Sales, Unit_Sales_Growth, Avg_Retail_Price, Bundle_Sales, Revenue, Revenue_Pct, Gross_Margin, Gross_Margin_Pct)

# changes the format of the tibble
product_summary_wide_tbl <- product_summary_tbl %>%  
  pivot_longer(c(Unit_Sales:Gross_Margin_Pct), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Year, values_from = Value)


# 3.0 VISUALS: UNDERSTAND THE DATA ----

# stacked bar graph of Year on Revenue
product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, fill = description)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_discrete(name = "Cereal Brands") +
  labs(title = "Annual Revenue")

# line graph of Year on Revenue
product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, group = description, color = description)) +
  geom_line() +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_color_discrete("Cereal Brands") +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(title = "Annual Revenue")

