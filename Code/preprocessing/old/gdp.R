library(tidyverse)

# Read GDP data, creating a table of GDP by year by country
gdp_full <- read.csv("Data/original/gdp/gdp-per-capita-worldbank.csv",
                     header = T, stringsAsFactors = F)
gdp_full <- gdp_full %>% 
  filter(Year > 1999 & Year < 2022) %>% # We have common data for 2000 - 2021
  filter(nchar(Code) == 3) %>%  # Filter out rows for non-countries
  rename(GDPPerCapita = GDP.per.capita..PPP..constant.2017.international..., CountryCode = Code)

# Lookup table for country codes
countrynames_gdp <- gdp_full %>% 
  select(Entity, CountryCode) %>% 
  unique() %>%
  rename(Country = Entity)
rownames(countrynames_gdp) <- countrynames_gdp$CountryCode