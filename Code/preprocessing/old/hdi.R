library(dplyr)
library(tidyr)

# You'll need to set your own working directory
#setwd("C:/Users/willc/OneDrive/OMSA/02 FA23 MGT6203x/Group Project/git/Team-4")

# Read in HDI data and create a HDI classification lookup table

hdi_full <- read.csv("Data/original/hdi/HDR21-22_Composite_indices_complete_time_series.csv", 
                     header = T, stringsAsFactors = F)
hdi_full <- hdi_full %>% 
  filter(nchar(iso3) == 3) %>%
  rename(CountryCode = iso3, Country = country, HDICode = hdicode)

hdi_piv <- hdi_full[,-5] %>% 
  select(CountryCode, starts_with("hdi_")) %>% 
  pivot_longer(-CountryCode, names_prefix = "hdi_", names_to = "Year", values_to = "HDI")

# HDI Code and region could be useful for other joins.
# Original electricity dataset also has very detailed region/union membership info.
countrynames_hdi <- hdi_full %>%
  select(Country, CountryCode, HDICode, region) %>%
  unique() %>%
  rename(Region = region) %>%
  mutate(Region = ifelse(Region == "", NA, Region))
rownames(countrynames_hdi) <- countrynames_hdi$CountryCode