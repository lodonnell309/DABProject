library(dplyr)
library(tidyr)

# Read in Electricity Data, creating a table of capacity, generation, demand, and import 
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
