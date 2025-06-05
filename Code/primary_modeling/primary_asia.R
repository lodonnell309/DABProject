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
  filter(Continent == "Asia") 

elecdata %>%
  group_by(HDICode) %>%
  summarise(n_distinct(CountryCode))

# View trends by HDI group
elecdata %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%  
  ggplot(aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + 
  theme_minimal() + 
  theme(legend.position = "top") +
  ggtitle("Clean Capacity Proportion in Asia From 2000 to 2021")

# Only 3 low hdi so lets look at individuals
capdata.low <- elecdata %>%
  filter(HDICode == "Low") %>%
  group_by(Year, CountryCode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal)) %>%
  pivot_wider(names_from = CountryCode, values_from = CapCleanProportion)

capdata.piv <- elecdata %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = HDICode, values_from = CapCleanProportion) %>%
  inner_join(capdata.low, by=join_by(Year))

# Baseline arimas to explore
auto.arima(capdata.piv$Low, stepwise = F, seasonal = F)
auto.arima(capdata.piv$PAK, stepwise = F, seasonal = F)
auto.arima(capdata.piv$AFG, stepwise = F, seasonal = F)
# Low & Pakistan = AR(1)
# AFG = random walk

capdata.piv2 <- elecdata %>%
  mutate(Group = ifelse(HDICode %in% c("Low", "Medium"), "Low", "High")) %>%
  group_by(Year, Group) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last") %>%
  pivot_wider(names_from = Group, values_from = CapCleanProportion)
# Trends with low/high groups aggregated
capdata.piv2 %>%
  ggplot(aes(Year)) + 
  geom_line(aes(y = Low, color="Low")) +
  geom_line(aes(y = High, color="High")) +
  theme_minimal() + theme(legend.position = "top") +
  labs(color = "Development Rank") +
  ylab("CapCleanProportion") +
  ggtitle("Clean Capacity Proportion in Asia From 2000 to 2021")

# Lag plot for whole dataset
lag2.plot(capdata.piv2$High, capdata.piv2$Low,11)


################################################################################
# Modeling
################################################################################

# Test train split
N <- nrow(capdata.piv2)
capdata.train <- capdata.piv2[1:17,]
capdata.test <- capdata.piv2[18:N,]

capdata.low.train.ts <- ts(capdata.train$Low, start=2000, end=2016, frequency=1)
capdata.high.train.ts <- ts(capdata.train$High, start=2000, end=2016, frequency=1)
capdata.low.test.ts <- ts(capdata.test$Low, start=2017, end=2021, frequency=1)
capdata.high.test.ts <- ts(capdata.test$High, start=2017, end=2021, frequency=1)

# Select lags for exogenous model
lag2.plot(capdata.train$High, capdata.train$Low,11)
# Strongest correlations between 0 and 2, so lag up to 2 (abs(cor) > 0.3)

X <- cbind(
  HighLag0 = capdata.train$High,
  HighLag1 = dplyr::lag(capdata.train$High, n=1),
  HighLag2 = dplyr::lag(capdata.train$High, n=2)
)
head(X)
# We'll have to trim off 1st 2

# But first... Baseline model
model.baseline <- auto.arima(window(capdata.low.train.ts, start=2002), stepwise = F)
summary(model.baseline)
checkresiduals(model.baseline)

baseline.pred <- forecast(model.baseline, h=5)
autoplot(baseline.pred) +
  autolayer(baseline.pred$mean, series="Forecast") +
  autolayer(capdata.low.test.ts, series = "Actual") +
  theme_minimal() +
  scale_color_manual(values=c("black", "red")) +
  scale_x_continuous(breaks = pretty(capdata.piv2$Year, n = 11))+
  ylab("CapCleanProportion") +
  ggtitle("Forecasts From Baseline Asia Model", 
          subtitle = "ARIMA(0,2,0)") +
  theme(legend.position = "top", legend.title = element_blank())

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
# Lag order 0 has lowest of all IC

model.exogenous <- lagModels[[1]]
X.test <- capdata.high.test.ts

summary(model.exogenous)
checkresiduals(model.exogenous)
# Residual test - must reject IID assumtion
exogen.pred <- forecast(model.exogenous, xreg = X.test, h=5)
autoplot(exogen.pred) +
  autolayer(exogen.pred$mean, series="Forecast") +
  autolayer(capdata.low.test.ts, series = "Actual") +
  theme_minimal() +
  scale_color_manual(values=c("black", "red")) +
  scale_x_continuous(breaks = pretty(capdata.piv2$Year, n = 11)) +
  ylab("CapCleanProportion") +
  ggtitle("Forecasts From Asia Model With Exogenous Predictor",
          subtitle = "Regression With ARIMA(2,0,0) Errors")+
  theme(legend.position = "top", legend.title = element_blank())
# Extreme deviation from true values!

accuracy(baseline.pred, capdata.low.test.ts)
accuracy(exogen.pred, capdata.low.test.ts)
# Very poor performance