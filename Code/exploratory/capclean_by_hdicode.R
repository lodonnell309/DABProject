elecdata <- read.csv("Data/unified/data_unified_raw.csv", na.strings=c("","NA"))

library(tidyr)
library(dplyr)
library(ggpubr)

hdi_levels <- c("Low", "Medium", "High", "Very High")
elecdata <- elecdata %>% 
  mutate(HDICode = factor(HDICode, levels = hdi_levels, ordered = T),
         CapCleanProportion = CapClean / CapTotal,
         CapFossilProportion = CapFossil / CapTotal)


summary(elecdata %>% select(HDICode, CapCleanProportion, CapFossilProportion))

elecdata %>% 
  filter(!is.na(HDICode)) %>%
  group_by(HDICode) %>%
  summarise(CapCleanProportion = mean(CapCleanProportion, na.rm = T),
            CapFossilProportion = mean(CapFossilProportion, na.rm = T),
            n = n())
# Interestingly, Low HDI nations have higher clean production than high HDI ones overall

# A view at 2021
elecdata %>% 
  filter(!is.na(HDICode) & Year == 2021) %>%
  group_by(HDICode) %>%
  summarise(CapCleanProportion = mean(CapCleanProportion, na.rm = T),
            CapFossilProportion = mean(CapFossilProportion, na.rm = T),
            n = n())

# Get cleaned set with whitespace removed from factor and NAs removed
hdi_levels <- gsub(" ", "", hdi_levels)
elecdata.cleaned <- elecdata %>%
  mutate(HDICode = factor(gsub(" ", "", HDICode), levels = hdi_levels, ordered = T),) %>%
  select(Year, CountryCode, HDICode, CapClean, CapTotal) %>%
  na.omit()

# Revisit 2021
elecdata.cleaned21 <- elecdata.cleaned[elecdata.cleaned$Year == 2021,]
elecdata.cleaned21$CapCleanProportion = elecdata.cleaned21$CapClean / elecdata.cleaned21$CapTotal
options(jupyter.plot.width=8)
ggplot(elecdata.cleaned21, aes(x = HDICode, y = CapCleanProportion, color = HDICode)) + 
  geom_boxplot() + 
  xlab("HDI Code") + ylab("Proportion") + 
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(size = 11))

aov.21 <- aov(CapCleanProportion ~ HDICode, data = elecdata.cleaned21)
summary(aov.21)
# Significant differences in groups

TukeyHSD(aov.21)
# Only High-VeryHigh group means significantly different

par(mfrow = c(1, 2))
plot(aov.21, c(1, 2))
# uncomment for plot title
# mtext("ANOVA Residuals: CapCleanProportion ~ HDICode", side = 3, line = -2, outer = TRUE)

# Shapiro test for normality
print("Shapiro-Wilk Normality Test: P-value by Group")
sapply(hdi_levels, function (hdi_code) {
  shap <- elecdata.cleaned21 %>% filter(HDICode == hdi_code) %>% pull(CapCleanProportion) %>% shapiro.test()
  shap$p.value
})
# options(jupyter.plot.width=8)
ggdensity(elecdata.cleaned21, x = "CapCleanProportion", color = "HDICode", 
          ggtheme = theme_minimal(), fill = NA) + 
  # uncomment for plot title
  # ggtitle("Distribution of Clean Capacity Proportion by HDI Code") +
  xlab("Proportion") + theme(legend.position = "top")
# Non normality demonstrated

kruskal.21 <- kruskal.test(CapCleanProportion ~ HDICode, data = elecdata.cleaned21)
kruskal.21

pairwise.wilcox.test(elecdata.cleaned21$CapCleanProportion, elecdata.cleaned21$HDICode,
                     p.adjust.method = "BH")
# Nonparametric test shows same results

capclean.grouped <- elecdata.cleaned %>%
  group_by(Year, HDICode) %>%
  summarise(CapCleanProportion = sum(CapClean) / sum(CapTotal), .groups = "drop_last")

ggplot(data = capclean.grouped, aes(x = Year, y = CapCleanProportion, color = HDICode)) +
  geom_line() + ylab("Proportion") +
  # ggtitle("Clean Capacity Proportion from 2000-2021") + 
  theme_minimal() + 
  theme(legend.position = "top")
# Line shows mostly flat trend for low and medium countries

# A view since 2010
ggplot(data = capclean.grouped[capclean.grouped$Year > 2010,], 
       aes(x = Year, y = CapCleanProportion, color = HDICode)) + 
  geom_line() + 
  # ggtitle("Clean Capacity Proportion from 2011-2021") + 
  ylab("Proportion")
# Since 2010, things look more upward for everything except low hdi