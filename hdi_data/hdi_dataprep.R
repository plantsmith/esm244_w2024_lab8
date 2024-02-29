### Clean up HDI data for lab

library(tidyverse)
library(here)
library(CoordinateCleaner) ### country info as countryref dataframe

### load lat-long coordinates for countries, including iso3 code
latlong <- CoordinateCleaner::countryref %>%
  filter(type == 'country') %>%
  select(iso3, lon = centroid.lon, lat = centroid.lat) %>%
  group_by(iso3) %>%
  summarize(# range_lat = max(lat) - min(lat),
            lat = mean(lat, na.rm = TRUE))
    

hdi_raw <- read_csv(here('hdi_data', 'HDR21-22_Composite_indices_complete_time_series.csv'))

country_cols <- c('iso3', 'country', 'hdicode') ### region has lots of NAs
hdi_cols <- c('le', 'eys', 'mys', 'gnipc', 
              'ineq_le', 
              # 'ineq_edu', 'ineq_inc', ### lots of NAs
              'gii', 
              'mf',  ### lots of NAs
              'co2_prod')

hdi_2021_vars <- hdi_raw %>%
  select(all_of(country_cols), all_of(paste0(hdi_cols, '_2021'))) 

hdi_2021_clean <- hdi_2021_vars %>%
  drop_na() %>%
  left_join(latlong, by = 'iso3')


glimpse(hdi_2021_clean)

write_csv(hdi_2021_clean, here('data/hdi_clean.csv'))

