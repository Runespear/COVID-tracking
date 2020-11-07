
# Packages ----------------------------------------------------------------


library(tidyverse)
library(janitor)
library(tigris)
library(sf)
library(sparkline)
library(tidygeocoder)
library(sp)
library(maps)
library(maptools)

# Data -------------------------------------------------------------------------------------------------

us_counties <- st_read(
  "us_counties.shp")

# county populations (source: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html)

pop <- read_csv("pop.csv")

df <- read_csv("https://raw.githubusercontent.com/Runespear/COVID-tracking/master/data/output/file_to_plot/confusion_block_latest.csv?token=ANFOJAGRD2L25TZCL5B75AS7VK6YC") %>% 
  clean_names() %>% 
  mutate(fips = as.character(fips))

df_5 <- df %>%
  filter(nchar(fips) == 5)

df_4 <- df %>%
  filter(nchar(fips) == 4) %>%
  mutate(fips = str_c("0", fips))

all <- bind_rows(df_4, df_5)

# joining population
all <- all %>% left_join(pop, by = "fips")

options(scipen = 999) 

cty_sf_thre <- us_counties %>% 
  geo_join(all, by_sp = "fips", by_df = "fips") %>% 
  mutate(
    weekly_new_cases = round(weekly_new_cases),
    weekly_new_cases = replace(weekly_new_cases, which(weekly_new_cases < 0), 0),
    weekly_new_cases_avg = round(weekly_new_cases/7),
    weekly_new_cases_capita = round(weekly_new_cases/popestimate2019 * 100000),
    weekly_new_cases_capita = round(weekly_new_cases_capita/7),
    predicted_double_days_f = as.numeric(predicted_double_days/7),
    predicted_double_days_f = case_when(
      predicted_double_days_f >= 0 & predicted_double_days_f <= 0.999999 ~ "0 to 1 week",
      predicted_double_days_f >= 1 & predicted_double_days_f <= 1.999999 ~ "1 to 2 weeks",
      predicted_double_days_f >= 2 & predicted_double_days_f <= 2.999999 ~ "2 to 3 weeks",
      predicted_double_days_f >= 3 & predicted_double_days_f <= 3.999999 ~ "3 to 4 weeks",
      predicted_double_days_f >= 4 & predicted_double_days_f <= 4.999999 ~ "4 to 5 weeks", 
      predicted_double_days_f >= 5 & predicted_double_days_f <= 5.999999 ~ "5 to 6 weeks", 
      predicted_double_days_f >= 6 ~ "More than 6 weeks", 
      predicted_double_days_f < 0 ~ "More than 6 weeks",
      TRUE ~ NA_character_
      ))

# sparklines 

cty_spark <- cty_sf_thre %>%
  as.data.frame() %>%
  select(fips, starts_with("rolled_cases")) %>%
  clean_names() %>%
  pivot_longer(
    !fips,
    names_to = "date",
    values_to = "incident_cases") %>%
  mutate(date = str_remove(date, "rolled_cases."),
         date = lubridate::ymd(date),
         incident_cases = round(incident_cases))

cty_spark_join <- cty_spark %>%
  group_by(fips) %>%
  summarize(
    TrendSparkline = spk_chr(
      width = '200px',
      height = '50px',
      color = "#00FF00",
      incident_cases, type ="line",
      chartRangeMin = min(incident_cases), chartRangeMax = max(incident_cases)
    )
  ) %>% mutate(fips = as.character(fips))

cty_spark <- left_join(cty_spark, cty_spark_join)

cty_spark_final <- cty_spark %>% group_by(fips, TrendSparkline) %>% summarize(TrendSparkline = max(TrendSparkline))

cty_sf_thre <- cty_sf_thre %>% left_join(cty_spark_final, by = "fips")

saveRDS(cty_sf_thre, "cty_sf_thre.rds")

