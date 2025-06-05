##load libraries
library(dplyr)
library(tidyr)
library(gdata)

##clear workspace and set working directory

rm(list = ls())

# You'll need to set your own working directory if not already
# wd_raw <- r"(/Users/liamodonnell/Documents/GitHub/Team-4)"
# setwd(wd_raw)


###Read in Electricity Data, creating a table of capacity, generation, demand, and import 
# quantities by country by year

electricity_full <- read.csv("Data/original/emberelectricity/yearly_Full_release_long_format.csv", 
                             header = T, stringsAsFactors = F)
electricity_full <- electricity_full %>% 
  filter(Area.type == "Country" & Year > 1999 & Year < 2022) %>% 
  filter(Country.code != "XKX" & nchar(Country.code) == 3) %>%
  rename(CountryCode = Country.code)

electricity_cap <- electricity_full %>% 
  filter(Category == "Capacity" & (Variable == "Clean" | Variable == "Fossil")) %>%
  pivot_wider(names_from = Variable, values_from = Value, names_prefix = "Cap",
              id_cols = c(CountryCode, Year)) %>%
  mutate(CapTotal = CapFossil + CapClean)

electricity_cap.wider <- electricity_full %>% 
  filter(Category == "Capacity" & (Variable == "Clean" | Variable == "Fossil")) %>%
  pivot_wider(names_from = Variable, values_from = Value, names_prefix = "Cap",
              id_cols = c(CountryCode, Year, Continent, Ember.region, EU, OECD, G20, G7, ASEAN)) %>%
  mutate(CapTotal = CapFossil + CapClean)

electricity_gen <- electricity_full %>% 
  filter(Category == "Electricity generation" & (Variable == "Clean" | Variable == "Fossil")) %>%
  filter(Unit == "TWh") %>%
  pivot_wider(names_from = Variable, values_from = Value, names_prefix = "Gen", 
              id_cols = c(CountryCode, Year))%>%
  mutate(GenTotal = GenFossil + GenClean)

electricity_demand <- electricity_full %>%
  filter(Category == "Electricity demand" & Subcategory == "Demand") %>%
  select(c(CountryCode, Year, Value)) %>%
  rename(Demand = Value)

electricity_import <- electricity_full %>% 
  filter(Category == "Electricity imports") %>%
  select(c(CountryCode, Year, Value)) %>%
  rename(Import = Value)

electricity_bycountryandyear <- electricity_cap %>%
  inner_join(electricity_gen, by = join_by(CountryCode, Year)) %>%
  inner_join(electricity_demand, by = join_by(CountryCode, Year)) %>%
  inner_join(electricity_import, by = join_by(CountryCode, Year))

# Lookup table for country codes
countrynames_electricity <- electricity_full %>% 
  select(Area, CountryCode) %>% 
  unique() %>%
  rename(Country = Area)
rownames(countrynames_electricity) <- countrynames_electricity$CountryCode

###Read GDP data, creating a table of GDP by year by country
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

gdp_full <- subset(gdp_full, select = -c(Entity))

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

###merge individual summary dataframes to one dataframe
#intent is to create a 'dictionary' for country code looking across all datasets
#need to clean the country column names up to compare across each source and if they are the same save 1 row
#if they are different potentially concatenate all non 'NA' columns that are not equal
#then only keep the newly created 'common country name' column and remove the others
#then add as a merge and clean up the unified data
#the unclean version has been added to the full dataset as a placeholder
countrynames_electricity <- countrynames_electricity %>% 
  rename(Country.electricity = Country)
countrynames_gdp <- countrynames_gdp %>% 
  rename(Country.gdp = Country)
countrynames_hdi <-countrynames_hdi %>% 
  rename(Country.hdi = Country)

country_names_all <- full_join(countrynames_electricity, countrynames_gdp, by = "CountryCode")
country_names_all <- full_join(country_names_all, countrynames_hdi, by = "CountryCode")

country_names_all <- country_names_all %>%
  tidyr::unite(col = "Country.all", c("Country.electricity", "Country.gdp", "Country.hdi"), sep = ";", na.rm = TRUE)

country_names_all$Country.all = sapply(strsplit(country_names_all$Country.all, ";"), 
                      function(x) Country.all = paste(unique(x), collapse = ";"))

data_unified_raw <- left_join(electricity_bycountryandyear, 
                                   gdp_full,
                                   by = c("CountryCode","Year"))

#this dataset seems to have some countries/rows where there is aggregate hdi, female hdi, and male hdi
#attempting to drop everything that is not a single aggregate hdi
hdi_piv <- hdi_piv[!is.na(as.numeric(as.character(hdi_piv$Year))),]
hdi_piv <- transform(hdi_piv, Year = as.numeric(Year))

data_unified_raw <- left_join(data_unified_raw, 
                              hdi_piv,
                              by = c("CountryCode","Year"))

data_unified_raw <- left_join(data_unified_raw, 
                              country_names_all,
                              by = c("CountryCode"))

data_unified_raw <- data_unified_raw %>%
  select(c("CountryCode","Year","Country.all", "Region"), everything())

data_unified_raw.wider <- data_unified_raw %>% 
  inner_join(
    electricity_cap.wider %>% 
      select(CountryCode, Year, Continent, Ember.region, EU, OECD, G20, G7, ASEAN), 
    by = join_by(CountryCode, Year)
  ) %>%
  rename(Region.Developing = Region, Region.Ember = Ember.region) %>%
  select(c(CountryCode,Year,Country.all,Continent,Region.Developing,Region.Ember,
           HDI,HDICode,EU,OECD,G20,G7,ASEAN), everything())

#uncomment the line below out if only final unified dataframe wants to be seen
keep(data_unified_raw, data_unified_raw.wider, sure = TRUE)

#write final dataframe to csv, uncomment below to write new datafile
unified_dir <- "Data/unified"
if (!dir.exists(unified_dir)) {
  dir.create(unified_dir)
}
write.csv(data_unified_raw, paste0(unified_dir, "/data_unified_raw.csv"), row.names=FALSE)
write.csv(data_unified_raw.wider, paste0(unified_dir, "/data_unified_raw_wide.csv"), row.names=FALSE)