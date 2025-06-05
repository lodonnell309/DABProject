library(ggplot2)
library(dplyr)
library(tidyr)

getwd()
# Set your WD to the git project root if necessary
# setwd('/Users/liamodonnell/Documents/GitHub/Team-4')
data <- read.csv('Data/unified/data_unified_raw.csv')
head(data)

# Visualizing total energy generation by year and region 
agg_data <- data %>%
  group_by(Year, Region) %>%
  summarise(total_gen = sum(GenTotal))

ggplot(agg_data, aes(x = Year, y = total_gen, color = Region)) +
  geom_line() +
  labs(title = "Energy Demand by Year and Region", x = "Year", y = "Total Energy Demand") +
  #scale_color_brewer(palette = "Set1") +  # You can choose a different color palette if you prefer
  theme_minimal()

# uncomment if you want to save png to base 
# ggsave("EnergyByYearAndRegion.png")

# Creating aggregated_data for total energy generation by type per year
aggregated_data <- data %>%
  group_by(Year) %>%
  summarise(Total_GenClean = sum(GenClean),Total_GenFossil=sum(GenFossil))

ggplot(aggregated_data, aes(x = Year)) +
  geom_line(aes(y = Total_GenClean, color = "GenClean"), size = 1) +
  geom_line(aes(y = Total_GenFossil, color = "GenFossil"), size = 1) +
  labs(x = "Year", y = "Total Generation") +
  # Uncomment to include plot title
  # ggtitle("GenClean vs GenFossil by Year")
  # scale_color_manual(values = c("GenClean" = "blue", "GenFossil" = "red")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = "Generation Type"))

# uncomment if you want to save png
# ggsave("EnergyGenByType.png")

# Now looking at percent change by type by year

pct_data <- aggregated_data %>%
  arrange(Year) %>%
  mutate(PercentChange_GenClean = (Total_GenClean - lag(Total_GenClean)) / lag(Total_GenClean) * 100,
         PercentChange_GenFossil = (Total_GenFossil - lag(Total_GenFossil)) / lag(Total_GenFossil) * 100)

# Create a new dataframe for plotting
plot_data <- pct_data %>%
  select(Year, PercentChange_GenClean, PercentChange_GenFossil)

# Plot the percentage change
ggplot(plot_data, aes(x = Year)) +
  geom_line(aes(y = PercentChange_GenClean, color = "GenClean"), size = 1) +
  geom_line(aes(y = PercentChange_GenFossil, color = "GenFossil"), size = 1) +
  geom_abline(slope = 0, intercept = 0, linetype="dashed") +
  labs(x = "Year", y = "% Change") +
  # ggtitle("% Change in GenClean and GenFossil by Year") +
  # scale_color_manual(values = c("GenClean" = "blue", "GenFossil" = "red")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = "Generation Type"))

# uncomment if you want to save png to base 
# ggsave("PercentChangeFossilClean.png")

