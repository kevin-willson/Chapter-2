library(dplR)
library(dplyr)
library(tidyr)
library(data.table)

## This code is used to average tree growth when two cores were collected from a tree. The idea is to bring in the raw data, average the data,
## convert it back to a rwl format and save it as a new rwl that dplR can read for analyses

setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/Non-aggregated tree core data")
Q2_3_Cores.rwl <- read.tucson(fname = "All_2022_Cores_Q2_6-6-24_Tucson.txt")

### To filter out cores with poor cross correlation after 1900
#### Burned trees: Three trees completely removed, one tree removed after 1992: 86 trees really in analysis
#### Unburned trees: three tree completely removed, one tree removed after 2001: 85 trees really in analysis 
Q2_3_Cores.rwl <- Q2_3_Cores.rwl[,!(colnames(Q2_3_Cores.rwl) %in% c("51123A","51313A","42211A","42211B","42213A","42213B","42113B","41212A",
                                                                    "41311A","41311B","32112A","32112B","32113A","32113B","31211B","31214A",
                                                                    "01414B","02353B","02361B","02362A","02363B","02441A","02442A","02442B",
                                                                    "01243B","01114A","01121B","01131A","01141B","01351A","01351B","01353A",
                                                                    "01415B","02444A","02444B","01122A","02445A","02143A","32312A","02443A"))]

### Need to remove some measurements 
Q2_3_Cores.rwl$Year <- as.numeric(row.names(Q2_3_Cores.rwl))

#### 32313B needs dates removed after 2001
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year > 2001, colnames(Q2_3_Cores.rwl) %in% "32313B"] <- NA

#### 51112A and B need dates removed after 1992
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year > 1992, colnames(Q2_3_Cores.rwl) %in% c("51112A","51112B")] <- NA

#### 42117B needs dates removed before 1970
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1970, colnames(Q2_3_Cores.rwl) %in% "42117B"] <- NA

#### 42417A and B need dates removed before 1948
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1948, colnames(Q2_3_Cores.rwl) %in% c("42417A","42417B")] <- NA

#### 42111A needs dates removed before 1955
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1955, colnames(Q2_3_Cores.rwl) %in% "42111A"] <- NA

#### 42211i needs dates removed before 1925
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1925, colnames(Q2_3_Cores.rwl) %in% "42211i"] <- NA

#### 31314A and B need dates removed before 1970
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1970, colnames(Q2_3_Cores.rwl) %in% c("31314A","31314B")] <- NA

#### 01122B needs dates removed after 2001
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year > 2001, colnames(Q2_3_Cores.rwl) %in% "01122B"] <- NA

#### 01123B needs dates removed before 1940
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1940, colnames(Q2_3_Cores.rwl) %in% "01123B"] <- NA

#### 01114B needs dates removed before 1980 
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1980, colnames(Q2_3_Cores.rwl) %in% "01114B"] <- NA

#### 01115A and B need dates removed before 1970
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1970, colnames(Q2_3_Cores.rwl) %in% c("01115A","01115B")] <- NA

#### 02114A needs dates removed before 1920
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1920, colnames(Q2_3_Cores.rwl) %in% "02114A"] <- NA

#### 02445B needs dates removed before 1960
Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year < 1960, colnames(Q2_3_Cores.rwl) %in% "02445B"] <- NA


### Then remove all dates before 1900
Q2_3_Cores.rwl_1900 <- Q2_3_Cores.rwl[Q2_3_Cores.rwl$Year >= 1900,]

Q2_3_Cores.rwl_1900$Year <- NULL

#### Double check these to make sure everything else looks good in Cofecha
write.rwl(Q2_3_Cores.rwl_1900, fname = "Q2_3_check_6-6-24.rwl", prec = 0.01)


## Get the year from the row name to a column
Q2_3_Cores.rwl_1900 <- setDT(Q2_3_Cores.rwl_1900, keep.rownames = T)[]
colnames(Q2_3_Cores.rwl_1900)[1] <- "Year"

## Transformt the file from wide to long
Q2_3_Cores.long <- melt(Q2_3_Cores.rwl_1900, id.vars = "Year", variable.names = "Tree")
colnames(Q2_3_Cores.long)[2] <- "Tree"

## Separate the tree ID into the individual digits
Q2_3_Cores.long <- Q2_3_Cores.long %>%
   separate(Tree, into = c("Fire", "Slope_Sev", "Aspect", "Plot_no", "Tree", "A/B"), sep = c(1,2,3,4,5,6))

## remove NAs for the calculations
Q2_3_Cores.long.omit <- na.omit(Q2_3_Cores.long)

## Get the sum of the ring widths
Q2_3_Cores_sum_growth <- Q2_3_Cores.long.omit %>% 
  group_by(Fire, Slope_Sev, Aspect, Plot_no, Tree, Year) %>% 
  summarize(Sum_growth = sum(value))

## Get the count of the number of ring widths (some only have one core, some cores date back further)
Q2_3_Cores_tally_growth <- Q2_3_Cores.long.omit %>% 
  group_by(Fire, Slope_Sev, Aspect, Plot_no, Tree, Year) %>% 
  tally()

## Prepare to average
Q2_3_Cores_avg_growth <- Q2_3_Cores_sum_growth

Q2_3_Cores_avg_growth <- merge(Q2_3_Cores_avg_growth, Q2_3_Cores_tally_growth, by = c("Fire", "Slope_Sev", "Aspect", "Plot_no", "Tree", "Year"))

## Calculate the average 
Q2_3_Cores_avg_growth$Avg_growth <- Q2_3_Cores_avg_growth$Sum_growth/Q2_3_Cores_avg_growth$n

## Remake the tucson file format
Q2_3_Cores_avg_growth$Tree_name <- paste(Q2_3_Cores_avg_growth$Fire, Q2_3_Cores_avg_growth$Slope_Sev, Q2_3_Cores_avg_growth$Aspect, 
                                         Q2_3_Cores_avg_growth$Plot_no, Q2_3_Cores_avg_growth$Tree, sep = "")

Q2_3_Cores_avg_growth <- Q2_3_Cores_avg_growth[,c(6,10,9)]

Q2_3_cores_avgd_back <- spread(Q2_3_Cores_avg_growth, key = Tree_name, value = Avg_growth)

rownames(Q2_3_cores_avgd_back) <- Q2_3_cores_avgd_back$Year

Q2_3_cores_avgd_back$Year <- NULL

## Save file
write.tucson(Q2_3_cores_avgd_back, fname = "Q2_3_Avgd_Cores_final_6-6-24_Tucson.txt", prec = 0.001)

## Make sure it works properly after saving 
test <- read.tucson(fname = "Q2_3_Avgd_Cores_final_6-6-24_Tucson.txt")
