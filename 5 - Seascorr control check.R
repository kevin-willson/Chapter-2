# This script checks the climate relationship between the control and fire trees to see how similar they are

## TL;DR - these trees are responding similarly to the climate around them. By month, season, half year and full year
## correlations

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

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/SPEI_Climate_Files/")


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

Climate_1901_2020 <- rbind(Climate_1900_1915, Climate_1915_1930, Climate_1930_1945, Climate_1945_1960,
                           Climate_1960_1975,Climate_1975_1990,Climate_1990_2005,Climate_2005_2020)

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
Climate_1901_2020$Transect[Climate_1901_2020$Fire == 'Zero'&Climate_1901_2020$Patch== 'Shallow'&Climate_1901_2020$Transect == 'North1'] <- "North1S"
Climate_1901_2020$Transect[Climate_1901_2020$Fire == 'Zero'&Climate_1901_2020$Patch== 'Shallow'&Climate_1901_2020$Transect == 'South1'] <- "South1S"

Climate_1901_2020 <- Climate_1901_2020 %>%
  separate(Date, into = c("Year", "Month"), sep = "-")

### HAD TO DELETE THREE MONTHS AT THE END OF 1975 BECAUSE THE hargreaves() FUNCTION ASSUMES THAT EVERYTHING STARTS IN JANUARY
Climate_1901_2020 <- Climate_1901_2020[!Climate_1901_2020$Year == "1900" & !Climate_1901_2020$Year == "2020",]

rownames(Climate_1901_2020) <- NULL

## Check correlations between groups with climate using seascorr()
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

fire_trees_rwl <- read.rwl("Fire_trees_Tucson_6-6-24.txt")
fire_trees_rwl$year <- row.names(fire_trees_rwl)
# fire_trees_rwl <- fire_trees_rwl[fire_trees_rwl$year>=1850 & fire_trees_rwl$year<=2019,]
fire_trees_rwl$year <- NULL

### I kept on switching between all fire excluded trees as control and just a subset of control trees, both give practically the same responses, so I included the 87 trees for more statistical power
control_trees_rwl <- read.rwl("Control_trees_Tucson_6-6-24.txt")
control_trees_rwl$year <- row.names(control_trees_rwl)
# control_trees_rwl <- control_trees_rwl[control_trees_rwl$year>=1850 & control_trees_rwl$year<=2019,]
control_trees_rwl$year <- NULL

all_trees <- cbind(fire_trees_rwl, control_trees_rwl)
all_trees_detrend <- detrend(all_trees, method = "ModNegExp")

fire_trees_rwl_detrended <- all_trees_detrend[,c(1:87)] ## 90
control_trees_rwl_detrended <- all_trees_detrend[,c(88:length(all_trees_detrend))] ## 91


### For trees that experienced fire 
grouped_climate_fire_1900 <- Climate_1901_2020 %>%
  filter(Fire != "Zero", Year >= 1948, Year <= 2018) %>% 
  group_by(Year, Month) %>%
  summarise(mean_ppt = mean(`ppt (mm)`),
            mean_temp = mean(`tmax (degrees C)`))

win <- 1900:2019
cores.yrs <- time(fire_trees_rwl)
fire_trees_rwl_detrended <- fire_trees_rwl_detrended[cores.yrs %in% win, ]

grouped_climate_fire_1900 <- as.data.frame(grouped_climate_fire_1900)

grouped_climate_fire_1900$Year <- as.numeric(as.character(grouped_climate_fire_1900$Year))
grouped_climate_fire_1900$Month <- as.numeric(as.character(grouped_climate_fire_1900$Month))

prefire_fire_chron  <- chron(fire_trees_rwl_detrended, na.rm = T, prefix = "CAM")

### Now for the control trees
grouped_climate_control_1900 <-  Climate_1901_2020 %>%
  filter(Fire == "Zero", Year >= 1948, Year <= 2018) %>% 
  group_by(Year, Month) %>%
  summarise(mean_ppt = mean(`ppt (mm)`),
            mean_temp = mean(`tmax (degrees C)`))

win <- 1900:2019
cores.yrs <- time(control_trees_rwl)
control_trees_rwl_detrended <- control_trees_rwl_detrended[cores.yrs %in% win, ]

grouped_climate_control_1900 <- as.data.frame(grouped_climate_control_1900)

grouped_climate_control_1900$Year <- as.numeric(as.character(grouped_climate_control_1900$Year))
grouped_climate_control_1900$Month <- as.numeric(as.character(grouped_climate_control_1900$Month))

prefire_control_chron  <- chron(control_trees_rwl_detrended, prefix = "CAM")


### Making plots supplemental
no_fire_plot <- plot(seascorr(prefire_control_chron, grouped_climate_control_1900, complete = 9, season_lengths = 6, var_names = c("temperature", "precipitation")))
fire_plot <- plot(seascorr(prefire_fire_chron, grouped_climate_fire_1900, complete = 9, season_lengths = 6, var_names = c("temperature", "precipitation")))

ggarrange(no_fire_plot, fire_plot, common.legend = T)



