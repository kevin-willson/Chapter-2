# This script makes the VPD figure that I plan on including in the presentation 
## It also includes some of the stats for the results section related to VPD

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
library(grid)

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

### get growth data that is organized differently for later
Fire_trees <- read.csv("recurrent_fire_tree_growth_std_mne_6-6-24.csv")

## Get the chronologies and plot the pre/post fire trends 
### All trees together
prefire_fire_trees_rwl <- read.rwl("Fire_trees_Tucson_6-6-24.txt")
prefire_fire_trees_rwl <- prefire_fire_trees_rwl[rowSums(!is.na(prefire_fire_trees_rwl))>4,]
prefire_fire_trees_rwl_detrended <- detrend(prefire_fire_trees_rwl, method = "ModNegExp")

### Separate all the sets of trees with different fire histories
trees_1 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('5112'), starts_with('5132'), starts_with('5142'))
trees_2 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('5122'))
trees_3 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('4211'))
trees_4 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('5121'), starts_with('4121'), starts_with('4131'),
                                                              starts_with('5111'), starts_with('5141'), starts_with('5131'))
trees_6 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('4141'))
trees_7 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('3'))
trees_8 <- prefire_fire_trees_rwl_detrended %>% dplyr::select(starts_with('4221'), starts_with('4241'))

### Get the specific fire years for each set of trees we are interested in
#### 1950, 1993, 2002, 2007, and 2012
trees_1 <- trees_1[c((1948-1899):(1952-1899), (1991-1899):(1995-1899),
                     (2000-1899):(2004-1899), (2005-1899):(2009-1899),
                     (2010-1899):(2014-1899)),]

trees_1$year <- row.names(trees_1)

trees_1$lag <- c(rep(-2:2, 5))

trees_1 <- trees_1 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_1 <- trees_1[!is.na(trees_1$growth),]

trees_1$first_sub <- ifelse(trees_1$year > 1952, "Subsequent", "First")

#### Fires in 1950, 1993, 2002, and 2012
trees_2 <- trees_2[c((1948-1899):(1952-1899), (1991-1899):(1995-1899),
                     (2000-1899):(2004-1899),
                     (2010-1899):(2014-1899)),]

trees_2$year <- row.names(trees_2)

trees_2$lag <- c(rep(-2:2, 4))

trees_2 <- trees_2 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_2 <- trees_2[!is.na(trees_2$growth),]

trees_2$first_sub <- ifelse(trees_2$year > 1952, "Subsequent", "First")

#### Fires in 1950, 1997, 2002, and 2012
trees_3 <- trees_3[c((1948-1899):(1952-1899), (1995-1899):(1999-1899),
                     (2000-1899):(2004-1899),
                     (2010-1899):(2014-1899)),]

trees_3$year <- row.names(trees_3)

trees_3$lag <- c(rep(-2:2, 4))

trees_3 <- trees_3 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_3 <- trees_3[!is.na(trees_3$growth),]

trees_3$first_sub <- ifelse(trees_3$year > 1952, "Subsequent", "First")

#### Fires in 1979, 1985, 1993, 2006, and 2012
trees_4 <- trees_4[c((1977-1899):(1981-1899), (1983-1899):(1987-1899),
                     (1991-1899):(1995-1899), (2004-1899):(2008-1899),
                     (2010-1899):(2014-1899)),]

trees_4$year <- row.names(trees_4)

trees_4$lag <- c(rep(-2:2, 5))

trees_4 <- trees_4 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_4 <- trees_4[!is.na(trees_4$growth),]

trees_4$first_sub <- ifelse(trees_4$year > 1981, "Subsequent", "First")

#### Fires in 1985, 1993, 2006, and 2012
trees_6 <- trees_6[c((1983-1899):(1987-1899),
                     (1991-1899):(1995-1899), (2004-1899):(2008-1899),
                     (2010-1899):(2014-1899)),]

trees_6$year <- row.names(trees_6)

trees_6$lag <- c(rep(-2:2, 4))

trees_6 <- trees_6 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_6 <- trees_6[!is.na(trees_6$growth),]

trees_6$first_sub <- ifelse(trees_6$year > 1987, "Subsequent", "First")

#### Fires in 1993, 2006, and 2012
trees_7 <- trees_7[c((1991-1899):(1995-1899), (2004-1899):(2008-1899),
                     (2010-1899):(2014-1899)),]

trees_7$year <- row.names(trees_7)

trees_7$lag <- c(rep(-2:2, 3))

trees_7 <- trees_7 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_7 <- trees_7[!is.na(trees_7$growth),]

trees_7$first_sub <- ifelse(trees_7$year > 1995, "Subsequent", "First")

#### Fires in 1997, 2002, and 2012
trees_8 <- trees_8[c((1995-1899):(1999-1899),
                     (2000-1899):(2004-1899),
                     (2010-1899):(2014-1899)),]

trees_8$year <- row.names(trees_8)

trees_8$lag <- c(rep(-2:2, 3))

trees_8 <- trees_8 %>% 
  pivot_longer(cols = !c(year,lag), names_to = "Tree", values_to = "growth")

trees_8 <- trees_8[!is.na(trees_8$growth),]

trees_8$first_sub <- ifelse(trees_8$year > 1999, "Subsequent", "First")

### Bring em all together 
all_trees_fire <- rbind(trees_1, trees_2, trees_3, trees_4, trees_6, trees_7, trees_8)

### Need climate 
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

rownames(Climate_1901_2020) <- NULL

counter <- 1
cutoff <- 1440
PET_list <- c()
CWB_list <- c()
SPEI_1_list <- c()
SPEI_3_list <- c()
SPEI_6_list <- c()
SPEI_12_list <- c()
SPEI_48_list <- c()

while(counter <= length(Climate_1901_2020$Fire)){
  location <- Climate_1901_2020[c(counter:cutoff),]
  PET <- hargreaves(Tmin = location$`tmin (degrees C)`, Tmax = location$`tmax (degrees C)`, 
                    lat = location$Latitude[1], Pre = location$`ppt (mm)`, na.rm = TRUE)
  PET_list <- append(PET_list, PET)
  CWB <- location$`ppt (mm)` - PET
  CWB_list <- append (CWB_list, CWB)
  SPEI_1 <- spei(CWB, scale = 1)
  SPEI_1_list <- append(SPEI_1_list, SPEI_1$fitted)
  SPEI_3 <- spei(CWB, scale = 3)
  SPEI_3_list <- append(SPEI_3_list, SPEI_3$fitted)
  SPEI_6 <- spei(CWB, scale = 6)
  SPEI_6_list <- append(SPEI_6_list, SPEI_6$fitted)
  SPEI_12 <- spei(CWB, scale = 12)
  SPEI_12_list <- append(SPEI_12_list, SPEI_12$fitted)
  SPEI_48 <- spei(CWB, scale = 48)
  SPEI_48_list <- append(SPEI_48_list, SPEI_48$fitted)
  counter <- counter +1440
  cutoff <- cutoff +1440
}

Climate_1901_2020$PET <- PET_list

Climate_1901_2020$CWB <- CWB_list

Climate_1901_2020$SPEI_1 <- SPEI_1_list

Climate_1901_2020$SPEI_3 <- SPEI_3_list

Climate_1901_2020$SPEI_6 <- SPEI_6_list

Climate_1901_2020$SPEI_12 <- SPEI_12_list

Climate_1901_2020$SPEI_48 <- SPEI_48_list

Climate_1901_2020$Year <- as.numeric(Climate_1901_2020$Year)

### Get it to annual values to get summed ppt and spei
#### Lets group normal by 25 years from 1945-2019 (1945-1969, 1970-1994, 1995-2019)
#### Grouping by 20 year normals worked a bit better from 1940-2019 (1940-1959, 1960-1979, 1980-1999, 2000-2019)
annual_climate_variables <- Climate_1901_2020 %>% 
  filter(Fire %in% c("Five", "Four", "Three")) %>% 
  group_by(Fire, Patch, Transect, Year) %>% 
  summarise(year_SPEI = SPEI_12[12], 
            annual_mean_max_vpd = mean(`vpdmax (hPa)`),
            annual_ppt = sum(`ppt (mm)`)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(year_SPEI = mean(year_SPEI),
            annual_mean_max_vpd = mean(annual_mean_max_vpd),
            annual_ppt = mean(annual_ppt)) %>% 
  filter(Year >= 1940 & Year <2020) %>% 
  mutate(normal_groups_25 = case_when(Year < 1970 ~ 1,
                                      Year >= 1970 & Year < 1995 ~ 2,
                                      Year >= 1995 ~3),
         normal_groups_20 = case_when(Year < 1960 ~ 1,
                                      Year >= 1960 & Year < 1980 ~ 2,
                                      Year >= 1980 & Year < 2000 ~ 3,
                                      Year >= 2000 ~4)) %>% 
  group_by(normal_groups_25) %>% 
  mutate(normal_ppt_25 = mean(annual_ppt),
         normal_vpd_25 = mean(annual_mean_max_vpd),
         normal_spei_25 = mean(year_SPEI)) %>% 
  ungroup() %>% 
  group_by(normal_groups_20) %>% 
  mutate(normal_ppt_20 = mean(annual_ppt),
         normal_vpd_20 = mean(annual_mean_max_vpd),
         normal_spei_20 = mean(year_SPEI))

#### Get the count of first vs subsequent fires for each
fire_first_sub_tally <- all_trees_fire %>% 
  mutate(Year = case_when(year <= 1952 ~ 1950, # 
                          year > 1952 & year <= 1981 ~ 1979, # 
                          year > 1981 & year <= 1987 ~ 1985, # 1985
                          year > 1987 & year <= 1994 ~ 1993, # 1993
                          year == 1995 & lag == 2 ~ 1993,
                          year == 1995 & lag == -2 ~ 1997, # 1997
                          year >= 1996 & year <= 1999 ~ 1997,
                          year > 1999 & year <= 2003 ~ 2002, # 2002
                          year == 2004 & lag == 2 ~ 2002,
                          year == 2004 & lag == -2 ~ 2006, # 2006,
                          year == 2005 & lag == -1 ~ 2006, 
                          year == 2006 & lag == 0 ~ 2006,
                          year == 2007 & lag == 1 ~ 2006,
                          year == 2008 & lag == 2 ~ 2006,
                          year == 2005 & lag == -2 ~ 2007, # 2007
                          year == 2006 & lag == -1 ~ 2007,
                          year == 2007 & lag == 0 ~ 2007, 
                          year == 2008 & lag == 1 ~ 2007, 
                          year == 2009 & lag == 2 ~ 2007, 
                          year > 2009 ~ 2012)) %>%
  group_by(Year, first_sub) %>% 
  summarise(for_count = n()/5)

#### Calculate 95% CIs for the 20 year CI values
normal_95_cis <- annual_climate_variables %>% 
  ungroup() %>% 
  group_by(normal_groups_20) %>% 
  summarise(upper_ci = mean(annual_mean_max_vpd) + 2*sd(annual_mean_max_vpd),
            lower_ci = mean(annual_mean_max_vpd) - 2*sd(annual_mean_max_vpd))

## Calculated bootstrap by group following this webpage:
# https://stackoverflow.com/questions/66253597/calculate-mean-and-bootstrap-confidence-intervals-by-group
library(Hmisc)

### Use this to do 20 year normals
normal_95_cis_boot <- annual_climate_variables %>% 
  ungroup() %>% 
  group_by(normal_groups_20) %>% 
  select(annual_mean_max_vpd) %>% 
  summarise(data = list(smean.cl.boot(cur_data(), conf.int = .95, B = 1000, na.rm = TRUE))) %>%
  tidyr::unnest_wider(data)




### VPD figure
Fig_A_normal <- ggplot() + # 
  geom_hline(yintercept = 47, size = 1, linetype = "dashed") +
  geom_bar(data = fire_first_sub_tally, aes(x = Year, y = for_count, fill = first_sub), stat = "identity", width = 1) +
  # geom_line(data = annual_climate_variables, aes(x = Year, y = (annual_mean_max_vpd-14)*12.5, color = "VPD"), size = 1) +
  ggpattern::geom_col_pattern(data = fire_first_sub_tally, aes(x = Year, y = for_count, fill = first_sub),
                              colour= c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                        "black","black","black","black"), 
                              pattern_density = 0.35, 
                              pattern_fill = c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                               "black","black","black","black"),
                              pattern_colour = c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                                 "black","black","black","black")) +
  annotate("rect", xmin = c(1940, 1960, 1980, 2000), xmax = c( 1959, 1979, 1999,2019), 
           ymin = (-c(normal_95_cis_boot$Lower-14)*12.5)+84.3, ymax = (-c(normal_95_cis_boot$Upper-14)*12.5)+84.3, fill = "blue", alpha = 0.3) +
  geom_line(data = annual_climate_variables[annual_climate_variables$normal_groups_20 == 1,], 
            aes(x = Year, y = (-(normal_vpd_20-14)*12.5)+84.3, color = "Normal"), size = 1.25) +
  geom_line(data = annual_climate_variables[annual_climate_variables$normal_groups_20 == 2,], 
            aes(x = Year, y = (-(normal_vpd_20-14)*12.5)+84.3, color = "Normal"), size = 1.25) +
  geom_line(data = annual_climate_variables[annual_climate_variables$normal_groups_20 == 3,], 
            aes(x = Year, y = (-(normal_vpd_20-14)*12.5)+84.3, color = "Normal"), size = 1.25) +
  geom_line(data = annual_climate_variables[annual_climate_variables$normal_groups_20 == 4,], 
            aes(x = Year, y = (-(normal_vpd_20-14)*12.5)+84.3, color = "Normal"), size = 1.25) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(breaks = c(seq(0,90, by = 30)), position = "left", sec.axis = sec_axis(~ -((./12.5)+14)+34.75, name = NULL, breaks = c(seq(14,21, by = 1)))) + #"Vapor pressure deficit (hPa)"
  coord_cartesian(ylim = c(4.25,90)) +
  scale_color_manual(values = c(VPD = "lightblue",
                                Normal = "blue"),
                     labels = c("20-year \nVPD normal", "VPD")) +
  scale_fill_manual(values = c("firebrick1", "brown4"),
                    labels = c("First-entry\nfire", "Reburns")) +
  labs(x = NULL, y = "Number of trees burned", fill = NULL, color = NULL) + #"Fire recurrence"
  # ggtitle(expression( "Annual mean maximum VPD with 20 year normals" )) + 
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=28),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.text.x.bottom = element_text(angle = 0),
        axis.title.y.right = element_text(vjust = 1.5))


library(zoo)

#### The reviewer suggested trying to do a running average, so lets do that with bootstrapping
running_mean_sd <- Climate_1901_2020 %>% 
  filter(Fire %in% c("Five", "Four", "Three")) %>% 
  group_by(Fire, Patch, Transect, Year) %>% 
  summarise(annual_mean_max_vpd = mean(`vpdmax (hPa)`)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(annual_mean_max_vpd = mean(annual_mean_max_vpd)) %>%
  mutate(running_vpd_avg = rollmean(annual_mean_max_vpd, k = 3, align = "right", fill = NA),
         running_vpd_sd = rollapply(annual_mean_max_vpd, width = 3, by = 1, sd, align ="right", fill = NA)) %>% 
  filter(Year >= 1940 & Year <= 2019) %>% 
  mutate(min_sd = running_vpd_avg - running_vpd_sd,
         max_sd = running_vpd_avg + running_vpd_sd)

running_mean_boot <- Climate_1901_2020 %>% 
  filter(Fire %in% c("Five", "Four", "Three")) %>% 
  group_by(Fire, Patch, Transect, Year) %>% 
  summarise(annual_mean_max_vpd = mean(`vpdmax (hPa)`)) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  summarise(annual_mean_max_vpd = mean(annual_mean_max_vpd))

running_mean_boot_df <- as.data.frame(replicate(1000, rollapply(running_mean_boot$annual_mean_max_vpd, width = 3, by = 1, 
                                                                function(x) mean( x[sample(length(x), repl = T)]), 
                                                                align ="right", fill = NA)))

colnames(running_mean_boot_df) <- c(paste0("replicate_",1:1000))

running_mean_boot_df$Year <- seq(1901,2020,1)

running_mean_boot_df_test <- running_mean_boot_df %>% 
  na.omit() %>% 
  pivot_longer(!Year, names_to = "variable", values_to = "value") %>% 
  group_by(Year)%>%
  summarise(bs_025 = quantile(value, 0.025),
            bs_975 = quantile(value, 0.975)) %>% 
  filter(Year >= 1940 & Year <= 2019) %>% 
  select(-Year)

running_mean_sd <- cbind(running_mean_sd, running_mean_boot_df_test)

Fig_A_running <- ggplot() + # 
  geom_hline(yintercept = 47, size = 1, linetype = "dashed") +
  geom_bar(data = fire_first_sub_tally, aes(x = Year, y = for_count, fill = first_sub), stat = "identity", width = 1) +
  # geom_line(data = annual_climate_variables, aes(x = Year, y = (annual_mean_max_vpd-14)*12.5, color = "VPD"), size = 1) +
  ggpattern::geom_col_pattern(data = fire_first_sub_tally, aes(x = Year, y = for_count, fill = first_sub),
                              colour= c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                        "black","black","black","black"), 
                              pattern_density = 0.35, 
                              pattern_fill = c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                               "black","black","black","black"),
                              pattern_colour = c("firebrick1","firebrick1","firebrick1",'black',"firebrick1","black","firebrick1","black",
                                                 "black","black","black","black")) +
  geom_smooth(data = running_mean_sd, 
            aes(x = Year, y = (-(running_vpd_avg-14)*12.5)+84.3, color = "Normal"), size = 1.25, se = F, span = 0.25) +
  stat_smooth(data = running_mean_sd, geom = "ribbon", method = "loess", span = 0.25, 
              aes(x = Year, y = (-(running_vpd_avg-14)*12.5)+84.3, ymin = (-(min_sd-14)*12.5)+84.3, ymax =(-(max_sd-14)*12.5)+84.3, 
                  color = "Normal"), fill = "blue",alpha = 0.3) +
  # geom_ribbon(data = running_mean_sd, 
  #             aes(x = Year, ymin = (-(bs_025-14)*12.5)+84.3, ymax =(-(bs_975-14)*12.5)+84.3, color = "Normal"), alpha = 0.3) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(breaks = c(seq(0,90, by = 30)), position = "left", sec.axis = sec_axis(~ -((./12.5)+14)+34.75, name = NULL, breaks = c(seq(14,21, by = 1)))) + #"Vapor pressure deficit (hPa)"
  coord_cartesian(ylim = c(4.25,90)) +
  scale_color_manual(values = c(VPD = "lightblue",
                                Normal = "blue"),
                     labels = c("3-year VPD\nrolling mean", "VPD")) +
  scale_fill_manual(values = c("firebrick1", "brown4"),
                    labels = c("First-entry\nfire", "Reburns")) +
  labs(x = NULL, y = "Number of trees burned", fill = NULL, color = NULL) + #"Fire recurrence"
  # ggtitle(expression( "Annual mean maximum VPD with 20 year normals" )) + 
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=28),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.text.x.bottom = element_text(angle = 0),
        axis.title.y.right = element_text(vjust = 1.5))

## Now I need to make the second half of the figure, which will show growth of fire-maintained trees and VPD together
### Get burned tree growth chronology
burned_trees_1940 <- prefire_fire_trees_rwl_detrended[c(41:(length(prefire_fire_trees_rwl_detrended$`31111`)-2)),]
burned_trees_1940_chron <- chron(burned_trees_1940)
burned_trees_1940_chron$Year <- as.numeric(row.names(burned_trees_1940_chron))

### Get values to normalize RWI distribution to VPD properly
mean(burned_trees_1940_chron$std)
sd(burned_trees_1940_chron$std)

mean(annual_climate_variables$annual_mean_max_vpd)
sd(annual_climate_variables$annual_mean_max_vpd)

Fig_B <- ggplot() + #Fig_B <- 
  geom_hline(yintercept = 1, size = 1, linetype = "dashed") +
  geom_line(data = annual_climate_variables, aes(x = Year, y = -((((annual_mean_max_vpd-17.08)/1.271377)*0.3958383)+0.9846346)+2, color = "VPD"), size = 1.25) +
  geom_line(data = burned_trees_1940_chron, aes(x = Year, y = std, color = "treegrowth"), size = 1.25) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_y_continuous(breaks = c(seq(0,2, by = 0.5)), sec.axis = sec_axis(~ -(((.-0.9846346)/0.3958383)*1.271377+17.08)+34.136,
                                                                         breaks = c(seq(14,21, by = 1)))) +
  scale_color_manual(values = c(VPD = "lightblue",
                                treegrowth = "brown4"),
                     labels = c("RWI", "VPD")) +
  labs(x = NULL, y = "Ring Width Index", fill = NULL, color = NULL) + #"Vapor pressure deficit (hPa)"
  # annotate("text", x = 1945, y = 20.5, label = "sqrt(RWI) = 3.03 - 0.12*VPD + 0.47*Release event") +
  theme(plot.title = element_text(hjust = 0),
        panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        text = element_text(size=28),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        axis.text.x.bottom = element_text(angle = 0),
        axis.title.y.right = element_text(vjust = 1.5))

test_normal <- ggarrange(Fig_B, Fig_A_normal, ncol = 1, align = "hv")
annotate_figure(test_normal, right = textGrob("Vapor pressure deficit (hPa)", rot = -90, vjust = 0.5, gp = gpar(cex = 2.5)))

test_running <- ggarrange(Fig_B, Fig_A_running, ncol = 1, align = "hv")
annotate_figure(test_running, right = textGrob("Vapor pressure deficit (hPa)", rot = -90, vjust = 0.5, gp = gpar(cex = 2.5)))

### Results section stats (not including calculations that used bootstrapping)
#### percent of subsequent fires after 2000
##### 253 tree-events occurred in subsequent fires
##### 196 of those tree-events were after 2000
196/235*100

#### average and SE in the year before first fires  
mean(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1949,1978,1984,1992,1996)])
sd(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1949,1978,1984,1992,1996)])/
  sqrt(nrow(annual_climate_variables[annual_climate_variables$Year%in%c(1949,1978,1984,1992,1996),]))

#### average and SE in the year before subsequent fires 
mean(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1984,1992,1996,2001,2005,2006,2011)])
sd(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1984,1992,1996,2001,2005,2006,2011)])/
  sqrt(nrow(annual_climate_variables[annual_climate_variables$Year%in%c(1984,1992,1996,2001,2005,2006,2011),]))

#### Two years before fire growth t-test
##### If I want to use burned trees:
pre_fire_growth_2 <- Fire_trees %>% 
  filter((Year == Fire_year_1 - 2)|
           (Year == Fire_year_2 - 2)|
           (Year == Fire_year_3 - 2)|
           (Year == Fire_year_4 - 2)|
           (Year == Fire_year_5 - 2)) %>% 
  mutate(Fire_number = case_when(Year <= Fire_year_1 +2 & Year >= Fire_year_1 - 2 ~ "First",
                                 Year <= Fire_year_2 +2 & Year >= Fire_year_2 - 2 ~ "Second",
                                 Year <= Fire_year_3 +2 & Year >= Fire_year_3 - 2 ~ "Third",
                                 Year <= Fire_year_4 +2 & Year >= Fire_year_4 - 2 ~ "Fourth",
                                 Year <= Fire_year_5 +2 & Year >= Fire_year_5 - 2 ~ "Fifth")) %>% 
  mutate(first_sub = ifelse(Fire_number == "First", "First", "Subsequent"))

t.test(pre_fire_growth_2$value ~ pre_fire_growth_2$first_sub, var.equal = TRUE)

##### If I want to use control trees instead
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

Control_trees <- read_excel("Control_trees_std_mne_growth_6-6-24.xlsx") 
Control_trees <- Control_trees[,-c(12,13)]

Control_trees$Year <- as.numeric(Control_trees$Year)

pre_fire_growth_2_first <- Control_trees %>% 
  filter(Year %in% c(1948, 1977, 1983, 1991, 1995)) %>%  
  mutate(first_sub = "First")

pre_fire_growth_2_sub <- Control_trees %>% 
  filter(Year %in% c(1983, 1991, 1995, 2000, 2004, 2005, 2010)) %>%  
  mutate(first_sub = "sub")

pre_fire_growth_2 <- rbind(pre_fire_growth_2_first, pre_fire_growth_2_sub)

t.test(pre_fire_growth_2$value ~ pre_fire_growth_2$first_sub, var.equal = TRUE)

#### Pre-fire growth t-test
pre_fire_growth <- Fire_trees %>% 
  filter((Year == Fire_year_1 - 1)|
           (Year == Fire_year_2 - 1)|
           (Year == Fire_year_3 - 1)|
           (Year == Fire_year_4 - 1)|
           (Year == Fire_year_5 - 1)) %>% 
  mutate(Fire_number = case_when(Year <= Fire_year_1 +2 & Year >= Fire_year_1 - 2 ~ "First",
                                 Year <= Fire_year_2 +2 & Year >= Fire_year_2 - 2 ~ "Second",
                                 Year <= Fire_year_3 +2 & Year >= Fire_year_3 - 2 ~ "Third",
                                 Year <= Fire_year_4 +2 & Year >= Fire_year_4 - 2 ~ "Fourth",
                                 Year <= Fire_year_5 +2 & Year >= Fire_year_5 - 2 ~ "Fifth")) %>% 
  mutate(first_sub = ifelse(Fire_number == "First", "First", "Subsequent"))

t.test(pre_fire_growth$value ~ pre_fire_growth$first_sub, var.equal = TRUE)

##### If I want to use control trees instead
pre_fire_growth_1_first <- Control_trees %>% 
  filter(Year %in% c(1949, 1978, 1984, 1992, 1996)) %>%  
  mutate(first_sub = "First")

pre_fire_growth_1_sub <- Control_trees %>% 
  filter(Year %in% c(1984, 1992, 1996, 2001, 2005, 2006, 2011)) %>%  
  mutate(first_sub = "sub")

pre_fire_growth_1 <- rbind(pre_fire_growth_1_first, pre_fire_growth_1_sub)

t.test(pre_fire_growth_1$value ~ pre_fire_growth_1$first_sub, var.equal = TRUE)

#### Year of fire t-test
##### Exclusively use control trees to eliminate any potential fire effect
pre_fire_growth_0_first <- Control_trees %>% 
  filter(Year %in% c(1950, 1979, 1985, 1993, 1997)) %>%  
  mutate(first_sub = "First")

pre_fire_growth_0_sub <- Control_trees %>% 
  filter(Year %in% c(1985, 1993, 1997, 2002, 2006, 2007, 2012)) %>%  
  mutate(first_sub = "sub")

pre_fire_growth_0 <- rbind(pre_fire_growth_0_first, pre_fire_growth_0_sub)

t.test(pre_fire_growth_0$value ~ pre_fire_growth_0$first_sub, var.equal = TRUE)

#### The year after fire
##### Exclusively use control trees to eliminate any potential fire effect
pre_fire_growth_post_first <- Control_trees %>% 
  filter(Year %in% c(1951, 1980, 1986, 1994, 1998)) %>%  
  mutate(first_sub = "First")

pre_fire_growth_post_sub <- Control_trees %>% 
  filter(Year %in% c(1986, 1994, 1998, 2003, 2007, 2008, 2013)) %>%  
  mutate(first_sub = "sub")

pre_fire_growth_post <- rbind(pre_fire_growth_post_first, pre_fire_growth_post_sub)

t.test(pre_fire_growth_post$value ~ pre_fire_growth_post$first_sub, var.equal = TRUE)

#### average and SE during first fire years 
mean(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1950,1979,1985,1993,1997)])
sd(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1950,1979,1985,1993,1997)])/
  sqrt(nrow(annual_climate_variables[annual_climate_variables$Year%in%c(1950,1979,1985,1993,1997),]))

#### average and SE during subsequent fire years 
mean(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year
                                                                     %in%c(1985,1993,1997,2002,2006,2007,2012)])
sd(annual_climate_variables$annual_mean_max_vpd[annual_climate_variables$Year%in%c(1985,1993,1997,2002,2006,2007,2012)])/
  sqrt(nrow(annual_climate_variables[annual_climate_variables$Year%in%c(1985,1993,1997,2002,2006,2007,2012),]))



