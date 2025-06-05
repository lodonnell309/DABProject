elecdata <- read.csv("Data/unified/data_unified_raw_wide.csv", na.strings=c("", " ", "NA"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(astsa)

# Factor-ize HDICode column and remove NA
hdi_levels <- c("Low", "Medium", "High", "VeryHigh")
elecdata <- elecdata %>%
  mutate(HDICode = factor(gsub(" ", "", HDICode), levels = hdi_levels)) %>%
  filter(!is.na(HDICode))

unique(elecdata$Continent)

lagplotsByHdiGroup <- function(grouped_data) {
  lag2.plot(grouped_data$Medium, grouped_data$Low, max.lag=11)
  lag2.plot(grouped_data$High, grouped_data$Low, max.lag=11)
  lag2.plot(grouped_data$VeryHigh, grouped_data$Low, max.lag=11)
  lag2.plot(grouped_data$High, grouped_data$Medium, max.lag=11)
  lag2.plot(grouped_data$VeryHigh, grouped_data$Medium, max.lag=11)
  lag2.plot(grouped_data$VeryHigh, grouped_data$High, max.lag=11)
} 

visualizeHdiCor <- function(elecdata.cont) {
  cap.grouped <- elecdata.cont %>%
    group_by(Year, HDICode) %>%
    summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
    pivot_wider(names_from = HDICode, values_from = CapCleanProportion)
  
  if(is.null(cap.grouped$Low)) cap.grouped$Low <- 0
  if(is.null(cap.grouped$Medium)) cap.grouped$Medium <- 0
  if(is.null(cap.grouped$High)) cap.grouped$High <- 0
  if(is.null(cap.grouped$VeryHigh)) cap.grouped$VeryHigh <- 0

  lagplotsByHdiGroup(cap.grouped)
}

#
# Asia
#
elecdata.as <- elecdata %>%
  filter(Continent == "Asia") 
elecdata.as %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.as %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
    geom_line() + theme_minimal()

# Cross correlations
visualizeHdiCor(elecdata.as)
# low and med highly correlated
# low and high weakly correlated with no lag
# low and very high weakly correlate with some lag
# med and high most correlated with no lag
# very high lags high
# could try modeling med and low, or asian sub regions

#
# Europe
#
elecdata.eu <- elecdata %>%
  filter(Continent == "Europe") 
elecdata.eu %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.eu %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal()
# visualizeHdiCor("Europe")
# No low dev

# 
# North America
#
elecdata.na <- elecdata %>%
  filter(Continent == "North America") 
elecdata.na %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.na %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal()
visualizeHdiCor(elecdata.na)
# Low med moderately neg correlated
# Low high weakly correlated with lag
# Low very high weakly correlated with lag
# Med high strongly correlated with no lag
# Med very high strongly correlated with no lag
# Could try modeling negative relationship bt low and med, modeling med with high and v high

#
# South America
#
elecdata.sa <- elecdata %>%
  filter(Continent == "South America") 
elecdata.sa %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.sa %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal()
visualizeHdiCor(elecdata.sa)
# No low dev
# Medium strongly correlated high with no and some lag
# Medium moderately correalted very high with no lag, strongly with some lag
# Try modeling with two exogenous series?

#
# Oceania
#
elecdata.oc <- elecdata %>%
  filter(Continent == "Oceania") 
elecdata.oc %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.oc %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal()
visualizeHdiCor(elecdata.oc)
# No low
# Med and high moderately negatively correlated with no to some lag
# Med and very high strongly negatively correlated with no lag, moderately with some

#
# Africa
#
elecdata.af <- elecdata %>%
  filter(Continent == "Africa") 
elecdata.af %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))
# Trends by HDI group
elecdata.af %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal()
visualizeHdiCor(elecdata.af)
# Low medium high corr with no-some lag
# Low high weakly corr with some lag
# Low very high weakly corr with some lag
# med high moderately corr with lag
# med very high moderately corr with lag
# high very high strong-mod corr with no lag
