elecdata <- read.csv("Data/unified/data_unified_raw_wide.csv", na.strings=c("", " ", "NA"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(astsa)

################################################################################
# Prep and exploration
################################################################################

# Factor-ize HDICode column and remove NA
hdi_levels <- c("Low", "Medium", "High", "VeryHigh")
elecdata <- elecdata %>%
  mutate(HDICode = factor(gsub(" ", "", HDICode), levels = hdi_levels)) %>%
  filter(!is.na(HDICode))
elecdata <- elecdata %>%
  filter(Continent == "South America") 

elecdata %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))

# Trends by HDI group
elecdata %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + theme_minimal() + theme(legend.position = "top") +
  ggtitle("Clean Capacity Proportion in South America From 2000 to 2021")

capdata.piv <- elecdata %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)
capdata.piv %>%
  ggplot(aes(Year)) + 
  geom_line(aes(y = Low, color="Low")) +
  geom_line(aes(y = High, color="High")) +
  theme_minimal() + theme(legend.position = "top") +
  labs(color = "Development Rank") +
  ylab("CapCleanProportion") +
  ggtitle("Clean Capacity Proportion in South America From 2000 to 2021")


################################################################################
# Modeling
################################################################################

# Split data
N <- nrow(capdata.piv)
capdata.train <- capdata.piv[1:17,]
capdata.test <- capdata.piv[18:N,]

capdata.low.train.ts <- ts(capdata.train$Low, start=2000, end=2016, frequency=1)
capdata.high.train.ts <- ts(capdata.train$High, start=2000, end=2016, frequency=1)
capdata.low.test.ts <- ts(capdata.test$Low, start=2017, end=2021, frequency=1)
capdata.high.test.ts <- ts(capdata.test$High, start=2017, end=2021, frequency=1)

# Select lags for exogenous model
lag2.plot(capdata.train$High, capdata.train$Low,11)
# Strongest correlations from 0 to 5 (> .3)

X <- cbind(
  HighLag0 = capdata.train$High,
  HighLag1 = dplyr::lag(capdata.train$High, n=1),
  HighLag2 = dplyr::lag(capdata.train$High, n=2),
  HighLag3 = dplyr::lag(capdata.train$High, n=3),
  HighLag4 = dplyr::lag(capdata.train$High, n=4),
  HighLag5 = dplyr::lag(capdata.train$High, n=5)
)
head(X)

# First, train baseline model
# Start train window in 2005 to account for NAs in lag variables
model.baseline <- auto.arima(window(capdata.low.train.ts, start=2005), stepwise = F)
summary(model.baseline)
checkresiduals(model.baseline)
baseline.pred <- forecast(model.baseline, h=5)
autoplot(baseline.pred) +
  autolayer(baseline.pred$mean, series="Forecast") +
  autolayer(capdata.low.test.ts, series = 'Actual') +
  theme_minimal() +
  scale_color_manual(values=c("black", "red")) + 
  scale_x_continuous(breaks = pretty(capdata.piv$Year, n = 11)) + 
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0.3, 0.85)) +
  ylab("CapCleanProportion") +
  ggtitle("Forecasts From Baseline South America Model",
          subtitle = "ARIMA(0,1,0) With Drift") +
  theme(legend.position = "top", legend.title = element_blank())
# not bad, but not capturing potential change in trend

# Train lagged exogenous models
trainLagModels <- function (xreg, y) {
  offset <- ncol(xreg) - 1
  lagmodels <- lapply(1:ncol(xreg), 
                      \(i) auto.arima(window(y, start=2000+offset), 
                                      xreg = xreg[-(1:offset),1:i], 
                                      stepwise = F))
  return(lagmodels)
}

lagModels <- trainLagModels(X, capdata.low.train.ts)
ics <- sapply(lagModels, \(mod) as.list(mod))[c("aic", "aicc", "bic"),]
colnames(ics) <- colnames(X)
ics
# Lag order 5 has a stand out lowest AICc

model.exogenous <- lagModels[[6]]

X.test <- cbind(
  HighLag0 = capdata.piv$High,
  HighLag1 = dplyr::lag(capdata.piv$High, n=1),
  HighLag2 = dplyr::lag(capdata.piv$High, n=2),
  HighLag3 = dplyr::lag(capdata.piv$High, n=3),
  HighLag4 = dplyr::lag(capdata.piv$High, n=4),
  HighLag5 = dplyr::lag(capdata.piv$High, n=5)
)
head(X.test)
X.test <- X.test[-(1:17),]

summary(model.exogenous)
checkresiduals(model.exogenous)
exogen.pred <- forecast(model.exogenous, xreg = X.test, h=5)
autoplot(exogen.pred) +
  autolayer(exogen.pred$mean, series="Forecast") +
  autolayer(capdata.low.test.ts, series = "Actual") +
  theme_minimal() +
  scale_color_manual(values=c("black", "red")) +
  scale_x_continuous(breaks = pretty(capdata.piv$Year, n = 11)) + 
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0.3, 0.85)) +
  ylab("CapCleanProportion") + 
  # ggtitle("Forecasts From South America Model With Exogenous Predictors",
          # subtitle = "Regression With ARIMA(1,0,4) Errors") +
  theme(legend.position = "top", legend.title = element_blank())
# Wide prediction intervals, and forecast begins to veer away from true values

accuracy(baseline.pred, capdata.low.test.ts)
accuracy(exogen.pred, capdata.low.test.ts)
# Exogenous model very likely overfit, testing shows that simple model performs better