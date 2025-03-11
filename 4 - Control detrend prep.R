# This script incorporates growth from a subset of 60 highly correlated trees that did not experience fires to compare growth response between trees that experienced fire in a given year against trees that did not experience fire

## Import libraries and datasets
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

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")

fires <- read_excel("Fire_Occurences_Corrected_1-15-23.xlsx", sheet = 1)

fires$Num_fires <- NULL

# trees <- read.rwl("Control_trees_Tucson_9-18-23.txt")

fire_trees_rwl <- read.rwl("Fire_trees_Tucson_6-6-24.txt")
fire_trees_rwl$year <- row.names(fire_trees_rwl)
# fire_trees_rwl <- fire_trees_rwl[fire_trees_rwl$year>=1850 & fire_trees_rwl$year<=2019,]
fire_trees_rwl$year <- NULL

control_trees_rwl <- read.rwl("Control_trees_Tucson_6-6-24.txt")
control_trees_rwl$year <- row.names(control_trees_rwl)
# control_trees_rwl <- control_trees_rwl[control_trees_rwl$year>=1850 & control_trees_rwl$year<=2019,]
control_trees_rwl$year <- NULL

all_trees <- cbind(fire_trees_rwl, control_trees_rwl)
all_trees_detrend <- detrend(all_trees, method = "ModNegExp")

control_trees_rwl_detrended <- all_trees_detrend[,c(88:length(all_trees_detrend))] #91 with old set of data

### Only needed if including single fire trees
# colnames(trees) <- controlled_tree_colnames

trees <- control_trees_rwl_detrended[,order(colnames(control_trees_rwl_detrended))]

basal <- detrend(rwl =  trees, method = "ModNegExp")

## Get the year from the row name to a column
basal <- setDT(basal, keep.rownames = T)[]
colnames(basal)[1] <- "Year"

## Transform the file from wide to long
basal.long <- melt(basal, id.vars = "Year", variable.names = "Tree")
colnames(basal.long)[2] <- "Tree"

basal.long$Tree <- gsub("-","", basal.long$Tree)

## Separate the tree ID into the individual digits
basal.long <- basal.long %>%
  separate(Tree, into = c("Fire", "Patch", "Transect", "Plot_no", "Tree", "Extra"), sep = c(1,2,3,4,5,6))

basal.long_recurrent <- basal.long[basal.long$Fire == "5" | basal.long$Fire == "4" | basal.long$Fire == "3" | basal.long$Fire == "0",]
basal.long_recurrent$Extra <- NULL

basal.long_recurrent$Patch <- ifelse(basal.long_recurrent$Patch == "1", "Shallow", basal.long_recurrent$Patch)
basal.long_recurrent$Patch <- ifelse(basal.long_recurrent$Patch == "2", "Moderate", basal.long_recurrent$Patch)

basal.long_recurrent$Transect <- ifelse(basal.long_recurrent$Transect == "1", "North", basal.long_recurrent$Transect)
basal.long_recurrent$Transect <- ifelse(basal.long_recurrent$Transect == "2", "East", basal.long_recurrent$Transect)
basal.long_recurrent$Transect <- ifelse(basal.long_recurrent$Transect == "3", "South", basal.long_recurrent$Transect)
basal.long_recurrent$Transect <- ifelse(basal.long_recurrent$Transect == "4", "West", basal.long_recurrent$Transect)

basal.long_one <- basal.long[basal.long$Fire != "5" & basal.long$Fire != "4" & basal.long$Fire != "3" & basal.long$Fire != "0",]

basal.long_one$Patch <- ifelse((basal.long_one$Fire != "D" & basal.long_one$Fire != "G") & (basal.long_one$Extra == "1" | basal.long_one$Extra == "2" |
                                                                                              basal.long_one$Extra == "3" | basal.long_one$Extra == "4"), 
                               paste0(basal.long_one$Patch,basal.long_one$Transect), basal.long_one$Patch)

basal.long_one$Transect <- ifelse((basal.long_one$Fire != "D" & basal.long_one$Fire != "G") & 
                                    (basal.long_one$Extra == "1" | basal.long_one$Extra == "2" | basal.long_one$Extra == "3" |
                                       basal.long_one$Extra == "4"), basal.long_one$Plot_no, basal.long_one$Transect)

basal.long_one$Transect <- ifelse((basal.long_one$Fire == "D" | basal.long_one$Fire == "G") & 
                                    (basal.long_one$Extra == "1" | basal.long_one$Extra == "2"),
                                  paste0(basal.long_one$Transect,basal.long_one$Plot_no), basal.long_one$Transect)

basal.long_one$Plot_no <- ifelse(basal.long_one$Extra == "1" | basal.long_one$Extra == "2" |basal.long_one$Extra == "3" |basal.long_one$Extra == "4",
                                 basal.long_one$Tree, basal.long_one$Plot_no)

basal.long_one$Tree <- ifelse(basal.long_one$Extra == "1" | basal.long_one$Extra == "2" |basal.long_one$Extra == "3" |basal.long_one$Extra == "4",
                              basal.long_one$Extra, basal.long_one$Tree)

basal.long_one$Extra <- NULL

basal.long <- rbind(basal.long_recurrent, basal.long_one)

## Convert columns to match fire columns to prepare for merging
basal.long$Fire[basal.long$Fire == "5"] <- "Five"
basal.long$Fire[basal.long$Fire == "4"] <- "Four"
basal.long$Fire[basal.long$Fire == "3"] <- "Three"
basal.long$Fire[basal.long$Fire == "0"] <- "Zero"
basal.long$Fire[basal.long$Fire == "D"] <- "Divide"
basal.long$Fire[basal.long$Fire == "G"] <- "Glass"
basal.long$Fire[basal.long$Fire == "T"] <- "Torres"
basal.long$Fire[basal.long$Fire == "S"] <- "Shelly"

basal.long <- na.omit(basal.long)

basal.long$Transect <- ifelse(basal.long$Plot_no == "2", paste0(basal.long$Transect, basal.long$Plot_no), basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Plot_no == "3", paste0(basal.long$Transect, basal.long$Plot_no), basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Plot_no == "4", paste0(basal.long$Transect, basal.long$Plot_no), basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Plot_no == "5", paste0(basal.long$Transect, basal.long$Plot_no), basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Plot_no == "6", paste0(basal.long$Transect, basal.long$Plot_no), basal.long$Transect)

basal.long$Plot_no <- NULL

### Have to quickly correct transect names for zero
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Moderate" & basal.long$Transect == "North", "North1", basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Moderate" & basal.long$Transect == "East", "East1", basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Shallow" & basal.long$Transect == "East", "East1S", basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Shallow" & basal.long$Transect == "North2", "North1S", basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Shallow" & basal.long$Transect == "South2", "South1S", basal.long$Transect)
basal.long$Transect <- ifelse(basal.long$Fire == "Zero" & basal.long$Patch == "Shallow" & basal.long$Transect == "West", "West1S", basal.long$Transect)

basal.long <- merge(basal.long, fires, all.x = TRUE)

basal.long$Group <- NULL

write.xlsx(basal.long, "Control_trees_std_mne_growth_6-6-24.xlsx") 
