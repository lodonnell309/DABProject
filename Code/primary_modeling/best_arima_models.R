#
# This file includes the minimum code needed to produce each continent's model
# (plus some visuals), now trained on the full dataset.
# 
# Of all Continents under analysis, only Africa and North America's best models
# utilized High-development country data.

elecdata <- read.csv("Data/unified/data_unified_raw_wide.csv", na.strings=c("", " ", "NA"))

library(dplyr)
library(tidyr)
library(forecast)

# To enable extra graphics, uncomment below
# library(ggplot2)

hdi_levels <- c("Low", "Medium", "High", "VeryHigh")
elecdata <- elecdata %>%
  mutate(HDICode = factor(gsub(" ", "", HDICode), levels = hdi_levels)) %>%
  filter(!is.na(HDICode))

################################################################################
# Africa
################################################################################
elecdata.af <- elecdata %>%
  filter(Continent == "Africa")

capdata.piv.af <- elecdata.af %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)

capdata.low.ts.af <- ts(capdata.piv.af$Low, start=2000, end=2021, frequency=1)
capdata.high.ts.af <- ts(capdata.piv.af$High, start=2000, end=2021, frequency=1)

model.af <- Arima(capdata.low.ts.af, 
                  xreg = capdata.high.ts.af, 
                  order = c(0,1,0), include.drift = T)
summary(model.af)
checkresiduals(model.af)
# Observations: low RMSE, good residual plot and Ljung-Box test result

# Uncomment for plot of actual series and fitted values
# autoplot(capdata.low.ts.af, series = "Actual") +
#   autolayer(model.af$fitted, series = "Fitted") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   ggtitle("Clean Capacity Proportion in Low-Development African Countries",
#           subtitle = "Modeled by Regression With ARIMA(0,1,0) Errors") +
#   ylab("CapCleanProportion") +
#   scale_color_manual(values=c("black", "red"))

# To forecast ahead, will need to build a model for capdata.high.ts.af and 
# simulate or forecast from it. Then those values would be passed to the 
# xreg parameter of the forecast() function

################################################################################
# Asia
################################################################################
elecdata.as <- elecdata %>%
  filter(Continent == "Asia")

capdata.piv.as <- elecdata.as %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)

capdata.low.ts.as <- ts(capdata.piv.as$Low, start=2000, end=2021, frequency=1)

model.as <- Arima(capdata.low.ts.as, order=c(0,2,0))
summary(model.as)
checkresiduals(model.as)
# Observations: low RMSE, good residual plot and Ljung-Box test result

# Uncomment for plot of actual series and fitted values
# autoplot(capdata.low.ts.as, series = "Actual") +
#   autolayer(model.as$fitted, series = "Fitted") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   ggtitle("Clean Capacity Proportion in Low-Development Asian Countries",
#           subtitle = "Modeled by ARIMA(0,2,0)") +
#   ylab("CapCleanProportion") +
#   scale_color_manual(values=c("black", "red"))

# To forecast ahead, simply use the forecast() function without xreg, as the best
# model does not use high HDI values to predict

################################################################################
# North America
################################################################################
elecdata.na <- elecdata %>%
  filter(Continent == "North America")

capdata.piv.na <- elecdata.na %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)

capdata.low.ts.na <- ts(capdata.piv.na$Low, start=2000, end=2021, frequency=1)
capdata.high.ts.na <- ts(capdata.piv.na$High, start=2000, end=2021, frequency=1)

model.na <- Arima(capdata.low.ts.na, 
                  xreg = capdata.high.ts.na, 
                  order = c(1,0,0), include.constant = F)

summary(model.na)
checkresiduals(model.na)
# Observations: higher RMSE than other models, residual plot shows some 
# autocorrelation, but good Ljung-Box test result

# Uncomment for plot of actual series and fitted values
# autoplot(capdata.low.ts.na, series = "Actual") +
#   autolayer(model.na$fitted, series = "Fitted") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   ggtitle("Clean Capacity Proportion in Low-Development North American Countries",
#           subtitle = "Modeled by Regression with ARIMA(1,0,0) Errors") +
#   ylab("CapCleanProportion") +
#   scale_color_manual(values=c("black", "red"))

# To forecast ahead, will need to build a model for capdata.high.ts.na and 
# simulate or forecast from it. Then those values would be passed to the 
# xreg parameter of the forecast() function

################################################################################
# Oceania
################################################################################
elecdata.oc <- elecdata %>%
  filter(Continent == "Oceania")

capdata.piv.oc <- elecdata.oc %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)

capdata.low.ts.oc <- ts(capdata.piv.oc$Low, start=2000, end=2021, frequency=1)

model.oc <- Arima(capdata.low.ts.oc, order=c(0,1,0))
summary(model.oc)
checkresiduals(model.oc)
# Observations: Comparable RMSE to North America model, residual plot shows some
# notable autocorrelation, Ljung-Box test results would force us to reject hypothesis
# of IID residuals with alpha = .05

# Uncomment for plot of actual series and fitted values
# autoplot(capdata.low.ts.oc, series = "Actual") +
#   autolayer(model.oc$fitted, series = "Fitted") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   ggtitle("Clean Capacity Proportion in Low-Development Oceanian Countries",
#           subtitle = "Modeled by ARIMA(0,1,0)") +
#   ylab("CapCleanProportion") +
#   scale_color_manual(values=c("black", "red"))

# To forecast ahead, simply use the forecast() function without xreg, as the best
# model does not use high HDI values to predict

################################################################################
# South America
################################################################################
elecdata.sa <- elecdata %>%
  filter(Continent == "South America")

capdata.piv.sa <- elecdata.sa %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)

capdata.low.ts.sa <- ts(capdata.piv.sa$Low, start=2000, end=2021, frequency=1)

model.sa <- Arima(capdata.low.ts.sa, order = c(0,1,0), include.drift = T)
summary(model.sa)
checkresiduals(model.sa)
# Observations: ok RMSE, residual plot looks good and good Ljung-Box test result

#Uncomment for plot of actual series and fitted values
# autoplot(capdata.low.ts.sa, series = "Actual") +
#   autolayer(model.sa$fitted, series = "Fitted") +
#   theme_minimal() +
#   theme(legend.position = "top", legend.title = element_blank()) +
#   ggtitle("Clean Capacity Proportion in Low-Development South American Countries",
#           subtitle = "Modeled by ARIMA(0,1,0) With Drift") +
#   ylab("CapCleanProportion") +
#   scale_color_manual(values=c("black", "red"))

# To forecast ahead, simply use the forecast() function without xreg, as the best
# model does not use high HDI values to predict


################################################################################

# All models output
# summary(model.af)
# summary(model.as)
# summary(model.na)
# summary(model.oc)
# summary(model.sa)