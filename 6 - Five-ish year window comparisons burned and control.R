# This script looks at tree growth during the years surrounding fire for burned and control trees
## The normal window is two years before and after, but if the window could be expanded, I did that

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

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

## Get the chronologies
fire_trees_rwl <- read.rwl("Fire_trees_Tucson_6-6-24.txt")

### 63 fire-excluded trees
control_trees_rwl <- read.rwl("Control_trees_Tucson_6-6-24.txt")

##### First got to detrend all FM and FE trees together so they are relativized the same way
all_trees <- cbind(fire_trees_rwl, control_trees_rwl)

all_trees_detrend <- detrend(all_trees, method = "ModNegExp")

fire_trees_rwl_detrended <- all_trees_detrend[,c(1:87)]

control_trees_rwl_detrended <- all_trees_detrend[,c(88:length(all_trees_detrend))]

### And finally, back to idea 1: Plot 5 year chronologies surrounding each fire with trees that didn't experience fire 
#### Get the 5 year periods of burned trees, unburned trees, and FE trees
#### burned trees
##### Separate all the sets of trees with different fire histories
trees_1_keep <- all_trees_detrend %>% dplyr::select(starts_with('5112'), starts_with('5132'), starts_with('5142'))
trees_2_keep <- all_trees_detrend %>% dplyr::select(starts_with('5122'))
trees_3_keep <- all_trees_detrend %>% dplyr::select(starts_with('4211'))
trees_4_keep <- all_trees_detrend %>% dplyr::select(starts_with('5121'), starts_with('4121'), starts_with('4131'),
                                                    starts_with('5111'), starts_with('5141'), starts_with('5131'))
trees_6_keep <- all_trees_detrend %>% dplyr::select(starts_with('4141'))
trees_7_keep <- all_trees_detrend %>% dplyr::select(starts_with('3'))
trees_8_keep <- all_trees_detrend %>% dplyr::select(starts_with('4221'), starts_with('4241'))


##### Get the specific fire years for each set of trees we are interested in
#### 1950, 1993, 2002, 2007, and 2012
trees_1_keep[-c((1945-1899):(1952-1899), (1988-1899):(1995-1899),
                (2000-1899):(2004-1899), (2005-1899):(2009-1899),
                (2010-1899):(2014-1899)),] <- NA


#### Fires in 1950, 1993, 2002, and 2012
trees_2_keep[-c((1945-1899):(1952-1899), (1988-1899):(1995-1899),
                (2000-1899):(2004-1899),
                (2010-1899):(2014-1899)),] <- NA

#### Fires in 1950, 1997, 2002, and 2012
trees_3_keep[-c((1945-1899):(1952-1899), (1995-1899):(1999-1899),
                (2000-1899):(2004-1899),
                (2010-1899):(2014-1899)),] <- NA

#### Fires in 1979, 1985, 1993, 2006, and 2012
trees_4_keep[-c((1974-1899):(1981-1899), (1983-1899):(1987-1899),
                (1988-1899):(1995-1899), (2001-1899):(2008-1899),
                (2010-1899):(2014-1899)),] <- NA

#### Fires in 1985, 1993, 2006, and 2012
trees_6_keep[-c((1983-1899):(1987-1899),
                (1988-1899):(1995-1899), (2001-1899):(2008-1899),
                (2010-1899):(2014-1899)),] <- NA

#### Fires in 1993, 2006, and 2012
trees_7_keep[-c((1988-1899):(1995-1899), (2001-1899):(2008-1899),
                (2010-1899):(2014-1899)),] <- NA


#### Fires in 1997, 2002, and 2012
trees_8_keep[-c((1995-1899):(1999-1899),
                (2000-1899):(2004-1899),
                (2010-1899):(2014-1899)),] <- NA

### Bring em all together 
all_burned_trees <- cbind(trees_1_keep, trees_2_keep, trees_3_keep, trees_4_keep, trees_6_keep, trees_7_keep, trees_8_keep)
all_burned_trees <- all_burned_trees[rowSums(!is.na(all_burned_trees))>2,]
all_burned_trees_chron <- chron.ci(all_burned_trees, R = 1000)
all_burned_trees_chron$year <- as.numeric(row.names(all_burned_trees_chron))

###### Unburned FM trees
trees_1_rm <- all_trees_detrend %>% dplyr::select(starts_with('5112'), starts_with('5132'), starts_with('5142'))
trees_2_rm <- all_trees_detrend %>% dplyr::select(starts_with('5122'))
trees_3_rm <- all_trees_detrend %>% dplyr::select(starts_with('4211'))
trees_4_rm <- all_trees_detrend %>% dplyr::select(starts_with('5121'), starts_with('4121'), starts_with('4131'))
trees_6_rm <- all_trees_detrend %>% dplyr::select(starts_with('4141'))
trees_7_rm <- all_trees_detrend %>% dplyr::select(starts_with('3'))
trees_8_rm <- all_trees_detrend %>% dplyr::select(starts_with('4221'), starts_with('4241'))

##### Get the specific fire years for each set of trees we are interested in
#### 1950, 1993, 2002, 2007, and 2012
trees_1_rm[c((1945-1899):(1952-1899), (1988-1899):(1995-1899),
             (2000-1899):(2004-1899), (2005-1899):(2009-1899),
             (2010-1899):(2014-1899)),] <- NA


#### Fires in 1950, 1993, 2002, and 2012
trees_2_rm[c((1945-1899):(1952-1899), (1988-1899):(1995-1899),
             (2000-1899):(2004-1899),
             (2010-1899):(2014-1899)),] <- NA

#### Fires in 1950, 1997, 2002, and 2012
trees_3_rm[c((1945-1899):(1952-1899), (1995-1899):(1999-1899),
             (2000-1899):(2004-1899),
             (2010-1899):(2014-1899)),] <- NA

#### Fires in 1979, 1985, 1993, 2006, and 2012
trees_4_rm[c((1974-1899):(1981-1899), (1983-1899):(1987-1899),
             (1988-1899):(1995-1899), (2001-1899):(2008-1899),
             (2010-1899):(2014-1899)),] <- NA

#### Fires in 1985, 1993, 2006, and 2012
trees_6_rm[c((1983-1899):(1987-1899),
             (1988-1899):(1995-1899), (2001-1899):(2008-1899),
             (2010-1899):(2014-1899)),] <- NA

#### Fires in 1993, 2006, and 2012
trees_7_rm[c((1988-1899):(1995-1899), (2001-1899):(2008-1899),
             (2010-1899):(2014-1899)),] <- NA


#### Fires in 1997, 2002, and 2012
trees_8_rm[c((1995-1899):(1999-1899),
             (2000-1899):(2004-1899),
             (2010-1899):(2014-1899)),] <- NA

### Bring em all together 
all_unburned_FM_trees <- cbind(trees_1_rm, trees_2_rm, trees_3_rm, trees_4_rm, trees_6_rm, trees_7_rm, trees_8_rm)
all_unburned_FM_trees <- all_unburned_FM_trees[rowSums(!is.na(all_unburned_FM_trees))>2,]
all_unburned_FM_trees_chron <- chron.ci(all_unburned_FM_trees, R = 1000)
all_unburned_FM_trees_chron$year <- as.numeric(row.names(all_unburned_FM_trees_chron))

### FE trees 
FE_trees <- all_trees_detrend[,88:length(all_trees_detrend)]
FE_trees <- FE_trees[rowSums(!is.na(FE_trees))>2,]
FE_trees_chron <- chron.ci(FE_trees, R = 1000)
FE_trees_chron$year <- as.numeric(row.names(FE_trees_chron))

#### Alright, lets look at the different fires
##### 1950 
all_burned_trees_chron_1950 <- all_burned_trees_chron[all_burned_trees_chron$year >= 1945 
                                                      & all_burned_trees_chron$year <= 1952,]

FE_trees_chron_1950 <- FE_trees_chron[FE_trees_chron$year >= 1945 
                                      & FE_trees_chron$year <= 1952,]


ggplot() +
  geom_vline(xintercept = 1950, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_1950, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_1950, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_1950, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_1950, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "1950 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1), limits = c(1945, 1952)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #, "Unburned FM trees"
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 1979 
all_burned_trees_chron_1979 <- all_burned_trees_chron[all_burned_trees_chron$year >= 1974 
                                                      & all_burned_trees_chron$year <= 1981,]

FE_trees_chron_1979 <- FE_trees_chron[FE_trees_chron$year >= 1974 
                                      & FE_trees_chron$year <= 1981,]


ggplot() +
  geom_vline(xintercept = 1979, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_1979, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_1979, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_1979, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_1979, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "1979 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees",
  theme(plot.title = element_text(hjust = 0, vjust = 0.5),
        axis.text.x=element_text(angle=90),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 1985 
all_burned_trees_chron_1985 <- all_burned_trees_chron[all_burned_trees_chron$year >= 1983 
                                                      & all_burned_trees_chron$year <= 1987,]

FE_trees_chron_1985 <- FE_trees_chron[FE_trees_chron$year >= 1983 
                                      & FE_trees_chron$year <= 1987,]


ggplot() +
  geom_vline(xintercept = 1985, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_1985, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_1985, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_1985, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_1985, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "1985 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 1993 
all_burned_trees_chron_1993 <- all_burned_trees_chron[all_burned_trees_chron$year >= 1988 
                                                      & all_burned_trees_chron$year <= 1995,]

FE_trees_chron_1993 <- FE_trees_chron[FE_trees_chron$year >= 1988 
                                      & FE_trees_chron$year <= 1995,]


gg_1993 <- ggplot() +
  geom_vline(xintercept = 1993, color = "firebrick1", linetype = "dashed", size = 1.25) +
  geom_hline(yintercept = 1, size = 1.25) +
  geom_line(data = all_burned_trees_chron_1993, aes(x = year, y = std, group = 1), size = 1.5, color = "firebrick1") +
  geom_line(data = FE_trees_chron_1993, aes(x = year, y = std, group = 3), size = 1.5, color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_1993, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_1993, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "1993 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90, vjust = 0.5),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1.25, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=48),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 1997 
all_burned_trees_chron_1997 <- all_burned_trees_chron[all_burned_trees_chron$year >= 1995
                                                      & all_burned_trees_chron$year <= 1999,]

FE_trees_chron_1997 <- FE_trees_chron[FE_trees_chron$year >= 1995
                                      & FE_trees_chron$year <= 1999,]

### Chang pointed out that all the theme elements (and pretty much anything you want to include) can go into a list to shorten the size of the code for a figure
list_test <- list(theme(plot.title = element_text(hjust = 0),
                        axis.text.x=element_text(angle=90, vjust = 0.5), #, vjust = .4
                        panel.background = element_rect(fill = 'white'), 
                        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
                        panel.border = element_rect(colour = "black", fill=NA, size=1),
                        text = element_text(size=40),
                        axis.ticks.length=unit(.25, "cm"),
                        axis.ticks = element_line(colour = "black", size = 1),
                        legend.position = "top"))

ggplot() +
  geom_vline(xintercept = 1997, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_1997, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_1997, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_1997, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_1997, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "1997 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  list_test

##### 2002 
all_burned_trees_chron_2002 <- all_burned_trees_chron[all_burned_trees_chron$year >= 2000 
                                                      & all_burned_trees_chron$year <= 2004,]

all_unburned_FM_trees_chron_2002 <- all_unburned_FM_trees_chron[all_unburned_FM_trees_chron$year >= 2000 
                                                                & all_unburned_FM_trees_chron$year <= 2004,]

FE_trees_chron_2002 <- FE_trees_chron[FE_trees_chron$year >= 2000 
                                      & FE_trees_chron$year <= 2004,]


ggplot() +
  geom_vline(xintercept = 2002, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_2002, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_2002, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_2002, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_2002, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "2002 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90, vjust = 0.5),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 2006 
all_burned_trees_chron_2006 <- all_burned_trees_chron[all_burned_trees_chron$year >= 2001 
                                                      & all_burned_trees_chron$year <= 2008,]

FE_trees_chron_2006 <- FE_trees_chron[FE_trees_chron$year >= 2001 
                                      & FE_trees_chron$year <= 2008,]


gg_2006 <- ggplot() +
  geom_vline(xintercept = 2006, color = "firebrick1", linetype = "dashed", size = 1.25) +
  geom_hline(yintercept = 1, size = 1.25) +
  geom_line(data = all_burned_trees_chron_2006, aes(x = year, y = std, group = 1), size = 1.5, color = "firebrick1") +
  geom_line(data = FE_trees_chron_2006, aes(x = year, y = std, group = 3), size = 1.5, color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_2006, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_2006, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "2006 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90, vjust = 0.5),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1.25, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=48),
        axis.text = element_text(color = "black"),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")


ggarrange(gg_1993, gg_2006, align = "hv", labels = "auto", ncol = 1, common.legend = T, legend = "top", 
          font.label = list(size = 28))


##### 2007 
all_burned_trees_chron_2007 <- all_burned_trees_chron[all_burned_trees_chron$year >= 2005 
                                                      & all_burned_trees_chron$year <= 2009,]

FE_trees_chron_2007 <- FE_trees_chron[FE_trees_chron$year >= 2005 
                                      & FE_trees_chron$year <= 2009,]


ggplot() +
  geom_vline(xintercept = 2007, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_2007, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_2007, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_2007, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_2007, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  # ggtitle(expression( "2007 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #"Unburned FM trees", 
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90, vjust = 0.5),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")

##### 2012 
all_burned_trees_chron_2012 <- all_burned_trees_chron[all_burned_trees_chron$year >= 2010 
                                                      & all_burned_trees_chron$year <= 2014,]

FE_trees_chron_2012 <- FE_trees_chron[FE_trees_chron$year >= 2010 
                                      & FE_trees_chron$year <= 2014,]


ggplot() +
  geom_vline(xintercept = 2012, color = "firebrick1", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 1, size = 1) +
  geom_line(data = all_burned_trees_chron_2012, aes(x = year, y = std, group = 1), color = "firebrick1") +
  geom_line(data = FE_trees_chron_2012, aes(x = year, y = std, group = 3), color = "skyblue") +
  geom_ribbon(data = all_burned_trees_chron_2012, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = year,
                                                      fill = "Burned_trees"), alpha = 0.35, color = NA) +
  geom_ribbon(data = FE_trees_chron_2012, aes(group = 3, ymin = lowerCI, ymax = upperCI, x = year,
                                              fill = "Fire_excluded_trees"), alpha = 0.35, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  ggtitle(expression( "2012 fire" )) + 
  scale_x_continuous(breaks = seq(1904, 2019, by = 1)) + #, limits = c(1929, 1978)
  scale_y_continuous(breaks = seq(0, 2.5, by = .5)) + 
  scale_fill_manual(name = "Chronologies", values = c(Burned_trees = "firebrick1",
                                                      Fire_excluded_trees = "skyblue"),
                    labels = c("Burned trees", "Control trees")) + #, "Unburned FM trees"
  theme(plot.title = element_text(hjust = 0),
        axis.text.x=element_text(angle=90, vjust = 0.5),
        panel.background = element_rect(fill = 'white'), 
        panel.grid.major.y = element_line(color = "gray75", size = 1, linetype = "dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text = element_text(size=40),
        axis.ticks.length=unit(.25, "cm"),
        axis.ticks = element_line(colour = "black", size = 1),
        legend.position = "top")
