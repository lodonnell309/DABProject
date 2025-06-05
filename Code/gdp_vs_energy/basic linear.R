setwd('/Users/liamodonnell/Documents/GitHub/Team-4')
data <- read.csv('Data/unified/data_unified_raw.csv')

head(data)

# Building simple regression models for some general understanding of relationships

# simple regression on GenTotal and GDPPerCapita

GenTotal_GDP <- lm(GDPPerCapita ~ GenTotal,data=data)
summary(GenTotal_GDP)

# Generation total is statistically significant Pr(>|t|) = 1.6e-13

# Regressing Clean Generation on Fossil Generation

Clean_Fossil <- lm(GenFossil ~ GenClean,data=data)
summary(Clean_Fossil)

# Reg. above suggests increase by 1.97 units in fossil fuels when clean increases by 1 unit 

## NEW: Looking at the proportion of GenTotal that is GenClean and regressing against GenClean
data$GenClean_prop <- data$GenClean/data$GenTotal
prop_model <- lm(GDPPerCapita~GenClean_prop,data=data)
summary(prop_model)

par(mfrow = c(2, 2))
plot(prop_model)

# Q-Q plot shows irregular shape

# Repeating the above model for countries with GDP per capita in the 80th + percentile

# Find the threshold value for the 85th percentile
threshold <- quantile(data$GDPPerCapita, 0.85,na.rm=T)

# Filter rows based on the threshold
filtered_data <- data[data$GDPPerCapita >= threshold, ]
filtered_data <- na.omit(filtered_data)

high_GDP_model <- lm(GDPPerCapita~GenClean_prop,data=filtered_data)
summary(high_GDP_model)

# For countries in the 85th percentile, genclean is not statistically relevant

par(mfrow = c(2, 2))
plot(high_GDP_model)

# Based on results of regression and corresponding plots. GenClen/GenTotal is not a good predictor for a county's GDP

# Repeating the process for countries in the bottom 5th percentile. 

# Find the threshold value for the 5th percentile
bottom_threshold <- quantile(data$GDPPerCapita, 0.05,na.rm=T)

# Filter rows based on the threshold
filtered_data1 <- data[data$GDPPerCapita <= bottom_threshold, ]
filtered_data1 <- na.omit(filtered_data1)

low_GDP_model <- lm(GDPPerCapita~GenClean_prop,data=filtered_data1)
summary(low_GDP_model)

par(mfrow = c(2, 2))
plot(low_GDP_model)

# GenClean_prop is statistically relevant and suggests a negative increase in GDP
## Q-Q plot shows heavy tails


## Trying a different approach as suggested by the team. Instead of looking
### at the ratio between capclean and captotal, I will scale capclean and compare that against total GDP

# Overall before individual HDI groups

model <- lm(GDPPerCapita~scale(GenClean),data=data)
summary(model)

# model shows statistical relevance for scaled genClean
## plotting the two attributes

clean_countries <- data

plot(x=scale(clean_countries$GenClean),y=clean_countries$GDPPerCapita)

library(ggplot2)
ggsave("GenCleanVsGDP.png")

hist(data$GenClean,freq=F)

ggsave("GenCleanHist.png")

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

# Overall before individual HDI groups

model <- lm(GDPPerCapita~scale(GenClean),data=data)
summary(model)

# model shows statistical relevance for scaled genClean
## plotting the two attributes

clean_countries <- data

plot(x=scale(clean_countries$GenClean),y=clean_countries$GDPPerCapita)

library(ggplot2)
ggsave("GenCleanVsGDP.png")

hist(data$GenClean,freq=F)

ggsave("GenCleanHist.png")
