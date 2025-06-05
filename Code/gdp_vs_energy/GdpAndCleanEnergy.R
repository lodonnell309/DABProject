# Uncomment and edit to manually set working directory. Opening with Rproj file
# should set this automatically.
# setwd('/Users/liamodonnell/Documents/GitHub/Team-4')
data <- read.csv('Data/unified/data_unified_raw.csv')

data$GenClean_prop <- data$GenClean/data$GenTotal

# Based on analysis done on basic linear.R, it looks like there are a significant amount 
## of countries with no clean energy consumption. To narrow down the search,
### we'll reference the plot EnergyByGenType.png. This plot shows clean energy
#### production begin to pick up in the year 2012. Using this graph, I will
##### first sort by the year > 2012 and then look at country's whose clean
###### energy consumption > bottom 15th percentile. This will be stored in current_df

cutoff_year = 2012
future <- data[data$Year >= cutoff_year, ]

# Looking at countries in the top 50th percentile of ratios of GenClean/GenTotal
cutoff_cleancap <- quantile(future$GenClean_prop, 0.50,na.rm=T)

# Filter rows over the threshold
future1 <- future[future$GenClean_prop >= cutoff_cleancap, ]
current_df <- na.omit(future1)

hist(current_df$GenClean)

# we only want to keep the countries that appear 10 times (each year between 2012-2021)

current_df[current_df$CountryCode %in% names(table(current_df$CountryCode[current_df$Year >= 2012 & current_df$Year <= 2021])) >= 10, ]

country_counts <- table(current_df$CountryCode)

# Filter countries that appear at least 10 times
filtered_countries <- names(country_counts[country_counts >= 10])

# Filter current_df based on the list of country codes
current_df <- current_df[current_df$CountryCode %in% filtered_countries, ]

# Create GenFossil_prop
current_df$GenFossil_prop <- current_df$GenFossil/current_df$GenTotal

# Create CapClean_prop
current_df$CapClean_prop <- current_df$CapClean/current_df$CapTotal

# Create CapFossil_prop
current_df$CapFossil_prop <- current_df$CapFossil/current_df$CapFossil

# now that we have removed countries with zeros, let's create a model for fossil generation and clean energy generation
model3 = lm(current_df$GDPPerCapita~current_df$GenFossil_prop+current_df$GenClean_prop)
summary(model3)

# r^2 value is low for model. Try log-log model and remove GenClean as it is redundant data
model4 = lm(log(current_df$GDPPerCapita)~log(current_df$GenFossil_prop+1)+
              log(current_df$GenClean_prop+1))
summary(model4)

# One item that is being neglected in this case is comparing counties with small GDP per capita
## against counties with disproportionately larger GDP/capita

hist(current_df$GDPPerCapita)

# Let's subset based on GDP 4 different ways 0-25,25-50,50-75,75-100th percentiles and see
## if we get more favorable results
percentiles <- quantile(current_df$GDPPerCapita, c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)

# Create subsets based on GDP percentiles
subset_0_25 <- current_df[current_df$GDPPerCapita >= percentiles[1] & current_df$GDPPerCapita < percentiles[2], ]
subset_25_50 <- current_df[current_df$GDPPerCapita >= percentiles[2] & current_df$GDPPerCapita < percentiles[3], ]
subset_50_75 <- current_df[current_df$GDPPerCapita >= percentiles[3] & current_df$GDPPerCapita < percentiles[4], ]
subset_75_100 <- current_df[current_df$GDPPerCapita >= percentiles[4], ]

model_75_100 <- lm(GDPPerCapita~CapClean_prop+CapFossil_prop,data = subset_75_100)
summary(model_75_100)

plot(x=subset_75_100$CapClean,y=subset_75_100$GDPPerCapita)

model_0_25 <- lm(GDPPerCapita~scale(CapClean)+scale(CapFossil),data = subset_0_25)
summary(model_0_25)

plot(x=subset_0_25$CapClean,y=subset_0_25$GDPPerCapita)

# Both models showing no statistical significance when observing 
# Let's try looking at the change from the previous year for each subset when predicting

current_df$GDPPerCapita_change <- c(NA, diff(current_df$GDPPerCapita))

current_df$GenClean_change <- c(NA, diff(current_df$GenClean))

# Created this copy to be used later -Will
current_df.copy <- data.frame(current_df)
current_df <- na.omit(current_df)

subset_0_25 <- current_df[current_df$GDPPerCapita >= percentiles[1] & current_df$GDPPerCapita < percentiles[2], ]
subset_25_50 <- current_df[current_df$GDPPerCapita >= percentiles[2] & current_df$GDPPerCapita < percentiles[3], ]
subset_50_75 <- current_df[current_df$GDPPerCapita >= percentiles[3] & current_df$GDPPerCapita < percentiles[4], ]
subset_75_100 <- current_df[current_df$GDPPerCapita >= percentiles[4], ]

model_75_100 <- lm(GDPPerCapita_change~GenClean_change,data = subset_75_100)
summary(model_75_100)

plot(x=subset_75_100$GDPPerCapita_change,y=subset_75_100$GenClean_change)

model_0_25 <- lm(GDPPerCapita_change~GenClean_change,data = subset_0_25)
summary(model_0_25)

plot(x=subset_0_25$GenClean_change,y=subset_0_25$GDPPerCapita_change)

# To break the problem down, look only at 2012 (arbitrary)
# Observations year over year are not independent, breaking an assumption of 
# linear regression.
library(dplyr)
library(car)
library(caret)
library(randomForest)

subset_12 <- current_df.copy %>% filter(Year == 2012 & !is.na(GDPPerCapita))
head(subset_12)
subset_12 <- subset_12 %>% dplyr::select(GDPPerCapita, CapClean_prop, Region, Demand, Import) %>%
  mutate(Region = as.factor(Region))
summary(subset_12)
scatterplotMatrix(subset_12[,-4])
#really wacky shapes there

# Build scaled dataset
subset_12_scl <- subset_12 %>%
  mutate(across(where(is.numeric), ~ scale(.)))

mod12.full <- lm(GDPPerCapita ~ ., data=subset_12_scl)
summary(mod12.full)
# plot(mod12.full)
# some unhealthy residual plots

# Calculate RMSE
predictions.full <- predict(mod12.full, newdata = subset_12_scl)
actualgdp <- subset_12_scl$GDPPerCapita
RMSE(predictions.full, actualgdp) # 0.734

# Box cox transformation
bc <- powerTransform(mod12.full, family="bcnPower")
summary(bc)
# Rounded power = 0 (log)
# Add constant to log transform val = 1 - min(Y)
logTransConst = 1 - min(subset_12_scl$GDPPerCapita)
mod12.bc <- lm(log(GDPPerCapita + logTransConst) ~ .,data=subset_12_scl)
summary(mod12.bc) # Improved R2, no significance
# plot(mod12.bc)

# Calculate RMSE
predictions.bc <- predict(mod12.bc, subset_12_scl)
# Have to reverse the log transform
RMSE(exp(predictions.bc)-logTransConst, actualgdp) #0.745

# Fit random forest model
set.seed(123)
mod12.rf<- train(
  GDPPerCapita~., data = subset_12_scl, method = "rf",
  trControl = trainControl("cv", number = 10)
)

print(mod12.rf$finalModel)
varImpPlot(mod12.rf$finalModel)

# Make predictions on the train data
predictions.rf <- mod12.rf %>% predict(subset_12_scl)
RMSE(predictions.rf, actualgdp) # 0.50221025

# Compare all on training
rmse.full <- RMSE(predictions.full, actualgdp)
rmse.bc <- RMSE(exp(predictions.bc)-logTransConst, actualgdp) 
rmse.rf <- RMSE(predictions.rf, actualgdp)

scaleFactor <- attr(subset_12_scl$GDPPerCapita, "scaled:scale")
scaleCenter <- attr(subset_12_scl$GDPPerCapita, "scaled:center")

unscale <- function(v) {
  v * scaleFactor + scaleCenter
}
actualgdp.unscaled <- unscale(actualgdp)
rmse.full.unscaled <- RMSE(unscale(predictions.full), actualgdp.unscaled)
rmse.bc.unscaled <- RMSE(unscale(exp(predictions.bc)-logTransConst), actualgdp.unscaled) 
rmse.rf.unscaled <- RMSE(unscale(predictions.rf), actualgdp.unscaled)

results <- rbind(c(rmse.full, rmse.bc, rmse.rf), 
                 c(rmse.full.unscaled, rmse.bc.unscaled, rmse.rf.unscaled))
colnames(results) <- c("Basic", "Log-Linear", "Random Forest")
rownames(results) <- c("Scaled", "Unscaled")
results

# Test on 2013
subset_13 <- current_df.copy %>% 
  filter(Year == 2013 & !is.na(GDPPerCapita)) %>%
  dplyr::select(GDPPerCapita, CapClean_prop, Region, Demand, Import) %>%
  mutate(Region = as.factor(Region))
# Scale with respect to training set
subset_13_scl <- subset_12 %>%
  mutate(across(where(is.numeric), ~ scale(., center=scaleCenter, scale=scaleFactor)))


# Compare all on testing
predictions.full.test <- predict(mod12.full, newdata = subset_13_scl)
predictions.bc.test <- predict(mod12.bc, subset_13_scl)
predictions.rf.test <- predict(mod12.rf, subset_13_scl)

rmse.full.test <- RMSE(predictions.full.test, actualgdp)
rmse.bc.test <- RMSE(exp(predictions.bc.test)-logTransConst, actualgdp) 
rmse.rf.test <- RMSE(predictions.rf.test, actualgdp)

rmse.full.test.unscaled <- RMSE(unscale(predictions.full.test), actualgdp.unscaled)
rmse.bc.test.unscaled <- RMSE(unscale(exp(predictions.bc.test)-logTransConst), actualgdp.unscaled) 
rmse.rf.test.unscaled <- RMSE(unscale(predictions.rf.test), actualgdp.unscaled)

results.test <- rbind(c(rmse.full.test, rmse.bc.test, rmse.rf.test),
                      c(rmse.full.test.unscaled, rmse.bc.test.unscaled, rmse.rf.test.unscaled))
colnames(results.test) <- c("Basic", "Log-Linear", "Random Forest")
rownames(results.test) <- c("Scaled", "Unscaled")
results.test

# Compare results train/test
results
results.test
c(sd=sd(subset_13$GDPPerCapita), sd2=2*sd(subset_13$GDPPerCapita))
# Avg prediction error within 1 sd. Random Forest RMSE increases significantly
# on 2013 test set while other models remain about the same. This would indicate
# that the random forest is very overfit. 

# Realistically, would want to train on 
# a few years of data. E.g., train set of 2012-2015, test set of 2016. If that doesn't
# work, you Might have to average each column to flatten the dataset into 1 row 
# per country and remove the year aspect.

# Could also play with genclean_prop instead of capclean_prop. We talked mostly 
# about capacity in the proposal and progress report, but if we find that genclean
# works instead, then that's something (especially if we also include gentotal).


# Train pred vs actual
plot(predictions.full, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
plot(predictions.bc, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
plot(predictions.rf, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
# This one looks great but it was too good to be true

# Test pred vs actual
plot(predictions.full.test, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
plot(predictions.bc.test, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
plot(predictions.rf.test, actualgdp, xlab="predicted",ylab="actual")
abline(a=0,b=1)
# Yeah, yikes

#_______________________________________________________________

# Subset data for 2012-2017. Switching from CapClean to GenClean

subset_12_17 <- current_df %>%
  filter(Year >= 2012 & Year <= 2017 & !is.na(GDPPerCapita))

subset_12_17 <- subset_12_17 %>% dplyr::select(GDPPerCapita, GenClean_prop, Region, Demand, Import) %>%
  mutate(Region = as.factor(Region))
summary(subset_12_17)
scatterplotMatrix(subset_12_17[,-4])

# Similar shapes to previous scatterplots for 2012 only

subset_12_17_scl <- subset_12_17 %>%
  mutate(across(where(is.numeric), ~ scale(.)))

mod12_17.full <- lm(GDPPerCapita ~ ., data=subset_12_17_scl)
summary(mod12_17.full)
plot(mod12_17.full)

predictions.full_1217 <- predict(mod12_17.full, newdata = subset_12_17_scl)
actualgdp_1217 <- subset_12_17_scl$GDPPerCapita
RMSE(predictions.full_1217, actualgdp_1217) # 0.75 - small increase when compared to 2012 only model

# RegionLAC is only statistically significant variable in above lm

# Repeating Will's process for selected years to see if we get any improvement

# Box cox transformation
bc1217 <- powerTransform(mod12_17.full, family="bcnPower")
summary(bc1217)
# Rounded power = 0 (log)
# Add constant to log transform val = 1 - min(Y)
logTransConst = 1 - min(subset_12_17_scl$GDPPerCapita)
mod12_17.bc <- lm(log(GDPPerCapita + logTransConst) ~ .,data=subset_12_17_scl)
summary(mod12_17.bc) # Bingo ! R^2 is 0.48 and GenClean_prop is statistically significant

# Let's look at the residuals plot
plot(mod12_17.bc)

# Residual plot isn't great, but is overall better than before, let's take a look at the RMSE
predictions.bc_12_17 <- predict(mod12_17.bc, subset_12_17_scl)
# Have to reverse the log transform
RMSE(exp(predictions.bc)-logTransConst, actualgdp) # 0.75 again

## predict the year 2018
subset_18 <- current_df %>%
  filter(Year == 2018 & !is.na(GDPPerCapita))

subset_18 <- subset_18 %>% dplyr::select(GDPPerCapita, GenClean_prop, Region, Demand, Import) %>%
  mutate(Region = as.factor(Region))

subset_18_scl <- subset_18 %>%
  mutate(across(where(is.numeric), ~ scale(.)))

predictions.bc_18 <- predict(mod12_17.bc,subset_18_scl)

plot(x= predictions.bc_18,y=subset_18_scl$GDPPerCapita)
# Based on all the above analysis. It seems reasonable to say that GenClean_prop
# and CapClean_Prop can not be used along with a linear model

## Flattening the dataset. Using the average of GenClean_prop, GDPPerCapita, and GenClean: group by CountryCode

group_df <- data %>% group_by(CountryCode) %>%
  summarise(AvgCleanProp=mean(GenClean_prop),AvgGDP=mean(GDPPerCapita),AvgGenClean=mean(GenClean))

# Drop na from group_df
group_df <- na.omit(group_df)

# plot AvgGenClean vs. AvgGDP
plot(x=log(group_df$AvgGenClean),y=group_df$AvgGDP)

# Fitting random forest on group_df
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(group_df), replace=TRUE, prob=c(0.7,0.3))
train  <- group_df[sample, ]
test   <- group_df[!sample, ]
testx <- subset(test, select = c(AvgCleanProp,AvgGenClean))
testy <- test[,3]


modelrf<- train(
  AvgGDP~AvgCleanProp+AvgGenClean, data = train, method = "rf",
  trControl = trainControl("cv", number = 2)
)

print(modelrf$finalModel)
varImpPlot(modelrf$finalModel)

predictionsrf <- predict(modelrf,testx)
RMSE(predictionsrf, test$AvgGDP)

# Overall plot of GDP//capita, GenClean and GenFossil per year in data set. 

library(scales)
# Aggregate the data
agg_data <- aggregate(cbind(GenClean, GenFossil, GDPPerCapita) ~ Year, data = data, sum)

# Calculate the rescaling factor for GenClean and GenFossil
rescale_factor <- max(agg_data$GDPPerCapita) / max(c(max(agg_data$GenClean), max(agg_data$GenFossil)))

# Apply the rescaling factor to GenClean and GenFossil
agg_data$GenClean_scaled <- agg_data$GenClean * rescale_factor
agg_data$GenFossil_scaled <- agg_data$GenFossil * rescale_factor

# Plot the data
ggplot(agg_data, aes(x = Year)) +
  geom_line(aes(y = GenClean_scaled, color = "GenClean"), size = 1.5) +
  geom_line(aes(y = GenFossil_scaled, color = "GenFossil"), size = 1.5) +
  geom_line(aes(y = GDPPerCapita, color = "GDPPerCapita"), size = 1.5) +
  
  labs(title = "Aggregate Data by Year",
       x = "Year",
       y = "Generation (GenClean and GenFossil)",
       color = "Variable") +
  scale_color_manual(values = c("blue", "green", "red"), 
                     breaks = c("GenClean", "GenFossil", "GDPPerCapita"),
                     labels = c("GenClean", "GenFossil", "GDPPerCapita")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size = 15), # Adjust the legend font size as needed
        axis.text = element_text(size = 15),   # Adjust the axis labels font size as needed
        axis.title = element_text(size = 15)) + # Adjust the axis titles font size as needed
  scale_y_continuous(
    name = "GDPPerCapita",
    sec.axis = sec_axis(~./rescale_factor, name = "Generation (GenClean and GenFossil)", labels = scales::comma),
    breaks = pretty_breaks()
  )




# ggsave("EnergyGenByYear.png")
