# This script looks at fire severity of all fires that burned after 1984 to see if there were any differences/trends among plots by fire. This script also performs a regression on RWI by VPD and fire severity to explain why pre-fire growth rates were significantly lower in the year before subsequent fire and after fires

library(readxl)
library(dplyr)
library(tidyverse)
library(car)
library(MASS)
library(dplR)
library(data.table)
library(reshape2)
library(xlsx)
library(ggplot2)
library(ggpubr)
library(agricolae)
library(rstatix)
library(treeclim)
library(matrixStats) #rowSds
library(dplR)
library(nlme)

## First, lets get the fire severity data in here and checked out
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

### Fire severity was quantified with relativized burn ratio (RBR) calculated by Parks et al. 2015
#### https://esajournals.onlinelibrary.wiley.com/doi/10.1890/14-1430.1
### RBR is calculated using methods from Parks et al. 2014
#### https://www.mdpi.com/2072-4292/6/3/1827 
fire_severity <- read_excel("Fire_Severity_7-4-24.xlsx", sheet = 1)

#### Values below 100 indicate unchanged by fire, 100-~140 represents low severity, 140-268 is moderate severity for the only fire captured in the analysis in Parks et al. 2014 (Miller fire 2002)
hist(fire_severity$Severity, breaks = c(seq(-200,200,25)))

tally(fire_severity[fire_severity$Severity < 100,]) # n = 65 for unchanged
tally(fire_severity[fire_severity$Severity >= 100 & fire_severity$Severity <= 140,]) # n = 9 for low severity
tally(fire_severity[fire_severity$Severity > 140,]) # n = 2 for moderate severity

range(fire_severity$Severity)

mean(fire_severity$Severity)
sd(fire_severity$Severity)

### See if there is a trend in subsequent fires for increasing severity over time. Spoiler: No
summary(lm(Severity ~ Year, data = fire_severity[fire_severity$First_Sub == "Subsequent",]))

## Need VPD 
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/SPEI_Climate_Files/")

## Followed the following youtube video:
### https://www.youtube.com/watch?v=dLpmUVDRFbM

library(SPEI)

## Prep climate data
Climate_1900_1915_csv <- "PRISM_Monthly_Climate_1900-1915_SPEI.csv"

headers = read.csv(Climate_1900_1915_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1900_1915 <- read.csv(Climate_1900_1915_csv, skip = 11, header = F)
colnames(Climate_1900_1915) = headers

Climate_1915_1930_csv <- "PRISM_Monthly_Climate_1915-1930_SPEI.csv"

headers = read.csv(Climate_1915_1930_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1915_1930 <- read.csv(Climate_1915_1930_csv, skip = 11, header = F)
colnames(Climate_1915_1930) = headers

Climate_1930_1945_csv <- "PRISM_Monthly_Climate_1930-1945_SPEI.csv"

headers = read.csv(Climate_1930_1945_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1930_1945 <- read.csv(Climate_1930_1945_csv, skip = 11, header = F)
colnames(Climate_1930_1945) = headers

Climate_1945_1960_csv <- "PRISM_Monthly_Climate_1945-1960_SPEI.csv"

headers = read.csv(Climate_1945_1960_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1945_1960 <- read.csv(Climate_1945_1960_csv, skip = 11, header = F)
colnames(Climate_1945_1960) = headers

Climate_1960_1975_csv <- "PRISM_Monthly_Climate_1960-1975_SPEI.csv"

headers = read.csv(Climate_1960_1975_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1960_1975 <- read.csv(Climate_1960_1975_csv, skip = 11, header = F)
colnames(Climate_1960_1975) = headers

Climate_1975_1990_csv <- "PRISM_Monthly_Climate_1975-1990_SPEI.csv"

headers = read.csv(Climate_1975_1990_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1975_1990 <- read.csv(Climate_1975_1990_csv, skip = 11, header = F)
colnames(Climate_1975_1990) = headers

Climate_1990_2005_csv <- "PRISM_Monthly_Climate_1990-2005_SPEI.csv"

headers = read.csv(Climate_1990_2005_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_1990_2005 <- read.csv(Climate_1990_2005_csv, skip = 11, header = F)
colnames(Climate_1990_2005) = headers

Climate_2005_2020_csv <- "PRISM_Monthly_Climate_2005-2020_SPEI.csv"

headers = read.csv(Climate_2005_2020_csv, skip = 10, header = F, nrows = 1, as.is = T)
Climate_2005_2020 <- read.csv(Climate_2005_2020_csv, skip = 11, header = F)
colnames(Climate_2005_2020) = headers

Climate_1901_2020 <- rbind(Climate_1900_1915, Climate_1915_1930, Climate_1930_1945, Climate_1945_1960, Climate_1960_1975, Climate_1975_1990,Climate_1990_2005,Climate_2005_2020)

Climate_1901_2020 <- arrange(Climate_1901_2020, Name)

#Climate_1901_2020 <- Climate_1901_2020[, (-c(2:4))]

Climate_1901_2020 <- Climate_1901_2020 %>% 
  separate(Name, into = c("Fire", "Patch", "Transect"), sep = "-")

Climate_1901_2020$Fire[Climate_1901_2020$Fire == 'G'] <- "Glass"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == 'T'] <- "Torres"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == 'S'] <- "Shelly"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == 'D'] <- "Divide"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == '5'] <- "Five"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == '4'] <- "Four"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == '3'] <- "Three"
Climate_1901_2020$Fire[Climate_1901_2020$Fire == '0'] <- "Zero"
Climate_1901_2020$Patch[Climate_1901_2020$Patch== 'Sha'] <- "Shallow"
Climate_1901_2020$Patch[Climate_1901_2020$Patch== 'Mod'] <- "Moderate"
Climate_1901_2020$Transect[Climate_1901_2020$Fire == 'Zero' & Climate_1901_2020$Patch== 'Shallow' & Climate_1901_2020$Transect == 'North1'] <- "North1S"
Climate_1901_2020$Transect[Climate_1901_2020$Fire == 'Zero' & Climate_1901_2020$Patch== 'Shallow' & Climate_1901_2020$Transect == 'South1'] <- "South1S"

Climate_1901_2020 <- Climate_1901_2020 %>% 
  separate(Date, into = c("Year", "Month"), sep = "-")

### Convert this into water years for a more accurate representation of the trends
Climate_1901_2020$Month <- as.numeric(Climate_1901_2020$Month) + 3

Climate_1901_2020$Year <- ifelse(Climate_1901_2020$Month > 12, as.numeric(Climate_1901_2020$Year) +1, Climate_1901_2020$Year)

Climate_1901_2020$Month <- ifelse(Climate_1901_2020$Month > 12, as.numeric(Climate_1901_2020$Month) -12, Climate_1901_2020$Month)

annual_climate_variables <- Climate_1901_2020 %>% 
  filter(Fire %in% c("Five", "Four", "Three")) %>% 
  group_by(Fire, Patch, Transect, Year) %>% 
  summarise(annual_mean_max_vpd = mean(`vpdmax (hPa)`)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(annual_mean_max_vpd = mean(annual_mean_max_vpd))

#### Check for trends between severity and vpd
annual_climate_variables_plot <- Climate_1901_2020 %>% 
  filter(Fire %in% c("Five", "Four", "Three")) %>% 
  group_by(Fire, Patch, Transect, Year) %>% 
  summarise(annual_mean_max_vpd = mean(`vpdmax (hPa)`)) %>% 
  ungroup()

fire_severity_climate <- merge(fire_severity, annual_climate_variables_plot)

# ### See if there is a trend for increasing severity over with aridity. Spoiler: significant exponential relationship
# check <- lm(Severity ~ poly(annual_mean_max_vpd,3), data = fire_severity_climate)
# 
# shapiro.test(check$residuals)
# 
# summary(check)

### Significant difference in severity between first and sub fires? NO
t.test(fire_severity_climate$Severity ~ fire_severity_climate$First_Sub, var.equal = TRUE)

### First-entry fire mean and SE
mean(fire_severity_climate$Severity[fire_severity_climate$First_Sub == "First"])
sd(fire_severity_climate$Severity[fire_severity_climate$First_Sub == "First"])/
  sqrt(nrow(fire_severity_climate[fire_severity_climate$First_Sub == "First",]))

### Reburn mean and SE
mean(fire_severity_climate$Severity[fire_severity_climate$First_Sub == "Subsequent"])
sd(fire_severity_climate$Severity[fire_severity_climate$First_Sub == "Subsequent"])/
  sqrt(nrow(fire_severity_climate[fire_severity_climate$First_Sub == "Subsequent",]))

### Significant difference in severity before and after fire? NO
fire_severity_climate$Pre_post_2000 <- ifelse(fire_severity_climate$Year < 2000, "Pre", "Post")

t.test(fire_severity_climate$Severity ~ fire_severity_climate$Pre_post_2000, var.equal = TRUE)

### Before 2000 mean and SE
mean(fire_severity_climate$Severity[fire_severity_climate$Pre_post_2000 == "Pre"])
sd(fire_severity_climate$Severity[fire_severity_climate$Pre_post_2000 == "Pre"])/
  sqrt(nrow(fire_severity_climate[fire_severity_climate$Pre_post_2000 == "Pre",]))

### After 2000 mean and SE
mean(fire_severity_climate$Severity[fire_severity_climate$Pre_post_2000 == "Post"])
sd(fire_severity_climate$Severity[fire_severity_climate$Pre_post_2000 == "Post"])/
  sqrt(nrow(fire_severity_climate[fire_severity_climate$Pre_post_2000 == "Post",]))

## Get growth data
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

Fire_trees <- read.csv("recurrent_fire_tree_growth_std_mne_6-6-24.csv")

Fire_trees <- Fire_trees %>%
  mutate(FPTT = paste0(Fire, Patch, Transect, Tree)) 

length(unique(Fire_trees$FPTT))

Control_trees <- read_excel("Control_trees_std_mne_growth_6-6-24.xlsx") 
Control_trees <- Control_trees[,-c(12,13)]

Control_trees$Year <- as.numeric(Control_trees$Year)

all_trees <- rbind(Fire_trees[,c(1:6)]) #, Control_trees[,c(1:6)]

### Add dbh for trees to see if it is important in the model or not
tree_size <- read.csv("Annual_tree_size.csv")

all_trees <- merge(all_trees, tree_size, all.x = T, by = c("Fire", "Patch", "Transect", "Tree", "Year"))

### Add fire severity to the year of and two years after fire to see if that has an effect on growth patterns
#### First, need to add rows so that I can have the two years after be added as well
fire_severity_1 <- fire_severity
fire_severity_1$Year <- fire_severity_1$Year + 1

fire_severity_2 <- fire_severity
fire_severity_2$Year <- fire_severity_2$Year + 2

fire_severity <- rbind(fire_severity,fire_severity_1,fire_severity_2)

all_trees <- merge(all_trees, fire_severity[,c(1:5)], by = c("Fire", "Patch", "Transect", "Year"), all.x = T)

all_trees$fire_w_severity <- ifelse(is.na(all_trees$Severity), "No", "Yes")

all_trees$Severity <- ifelse(is.na(all_trees$Severity), 0, all_trees$Severity)

### Add climate variables
all_trees <- merge(all_trees, annual_climate_variables, all.x = T, by = "Year")

### Get previous year growth to account for autocorrelation
all_trees <- all_trees %>%
  group_by(Fire, Patch, Transect, Tree) %>%
  mutate(prev_growth_1 = lag(value), FPT = paste0(Fire, Patch, Transect), FPTT = paste0(Fire, Patch, Transect,Tree)) %>%
  drop_na()


all_trees <- all_trees[all_trees$Year >= 1984 & all_trees$Year <= 2018,]

### lets try modeling this
regression_check <- lme(sqrt(value) ~ annual_mean_max_vpd + Severity + annual_diameter, random = ~1|FPT/(Year), #
                  correlation = corAR1(form = ~ prev_growth_1|FPT/(Year)),  
                  data = all_trees, na.action = "na.fail") #, [all_trees$Year >= 2007 & all_trees$Year <= 2018,]

#### Check autocorrelation
plot(ACF(regression_check, resType = "normalized"), alpha = 0.05)

#### Check normality 
shapiro.test(resid(regression_check))
qqnorm(resid(regression_check)); qqline(resid(regression_check))

all_trees$outliers <- resid(regression_check)

### The majority of issues with normality came among points with residuals less than -0.5
all_trees_outliers <- all_trees[all_trees$outliers < -0.5,]

### It turned out that a number of these residuals came from the same trees, so we identified which trees had multiple residual values <0.5 and removed them from the dataset to achieve normality. 
duplicate_outlier_trees <- all_trees_outliers[duplicated(all_trees_outliers$FPTT),]

duplicate_outlier_trees_unique <- unique(duplicate_outlier_trees$FPTT)

all_trees <- all_trees[!(all_trees$FPTT %in% duplicate_outlier_trees_unique),]
 
regression_check <- lme(sqrt(value) ~ annual_mean_max_vpd + Severity + annual_diameter, random = ~1|FPT/(Year), #+ Severity
                        correlation = corAR1(form = ~ prev_growth_1|FPT/(Year)),
                        data = all_trees, na.action = "na.fail")

#### Check autocorrelation
plot(ACF(regression_check, resType = "normalized"), alpha = 0.05)

#### Check normality
shapiro.test(resid(regression_check))
qqnorm(resid(regression_check)); qqline(resid(regression_check))

### Check if all variables are important to keep
#### drop annual diameter
dredge_output <- MuMIn::dredge(regression_check)

dredge_output[is.na(dredge_output)] <- 0

htmlTable::htmlTable(dredge_output)

regression_check <- lme(sqrt(value) ~ annual_mean_max_vpd + Severity, random = ~1|FPT/(Year), #+ Severity
                        correlation = corAR1(form = ~ prev_growth_1|FPT/(Year)),
                        data = all_trees, na.action = "na.fail")

#### Check autocorrelation
plot(ACF(regression_check, resType = "normalized"), alpha = 0.05)

#### Check normality
shapiro.test(resid(regression_check))
qqnorm(resid(regression_check)); qqline(resid(regression_check))

#### Check multicollinearity, none
car::vif(regression_check)


summary(regression_check)
MuMIn::r.squaredGLMM(regression_check)

