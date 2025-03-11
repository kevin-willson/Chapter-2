library(dplR)
library(detrendeR)
setwd("D:/OneDrive - University of New Mexico/Kevin's_Grad_School_projects/ESEL/Dissertation/Dissertation_Analysis/Question_2_and_3/")
trees <- "Q2_3_Avgd_Cores_final_6-6-24_Tucson.txt"

#Read in tree core data. Make sure to have a three line header at the top of the txt file that is 12 columns to fit the .rwl file format
cores_start <- read.rwl(trees)
cores.yrs <- time(cores_start)

cores <- detrend(cores_start, method = "ModNegExp")
## This was for textfiles with Q1, 2, and 3 cores in them, but I did not include Q1 cores for reasons mentioned below
# non_single_cores <- cores[,c(1:126)]

# Controls  
## Lets see how they cross correlate with one another
win <- 1948:2018
cores.trunc <- cores[cores.yrs %in% win, ]

zeros_group <- cores.trunc

correlation_tbl <-data.frame(Cores=numeric(),
                             pvalue=numeric(),
                             stringsAsFactors=FALSE)

zero_core_value <- 1

while(zero_core_value <= 85){ #87 if using all cores
  temp.cores.trunc <- zeros_group[,c(zero_core_value,86:length(zeros_group))] #88 if using all cores
  temp.corr <- interseries.cor(temp.cores.trunc)[1,]
  correlation_tbl <- rbind(correlation_tbl, temp.corr)
  zero_core_value <- zero_core_value + 1
}

correlation_tbl <- correlation_tbl[order(-correlation_tbl$res.cor),]

plot(correlation_tbl$res.cor)

### take the top cores that have a correlation of at least 0.65.
correlation_tbl_cont <- correlation_tbl[correlation_tbl$res.cor > 0.65,]

## Create a list of the row names to select the proper columns for future analysis using these cores for the first fire of all trees
trees_to_use <- row.names(correlation_tbl_cont)

control_trees_prefire <- cores_start[, c(trees_to_use)]

### Curious to compare growth trends among those retained and removed
removed_trees_prefire <- cores[, !colnames(cores) %in% trees_to_use]
removed_trees_prefire <- removed_trees_prefire[,c(1:27)]
removed_trees_prefire <- removed_trees_prefire[rowSums(!is.na(removed_trees_prefire))>2,]
removed_trees_prefire_chron <- chron.ci(removed_trees_prefire, R = 1000)
removed_trees_prefire_chron$yrs <- as.numeric(row.names(removed_trees_prefire_chron))

retained_trees <- cores[, colnames(cores) %in% trees_to_use]
retained_trees <- retained_trees[rowSums(!is.na(retained_trees))>2,]
retained_trees_chron <- chron.ci(retained_trees, R = 1000)
retained_trees_chron$yrs <- as.numeric(row.names(retained_trees_chron))

ggplot() +
  # geom_vline(xintercept = c(1950, 1979,1985,1993,1997,2002,2006,2007,2012), linetype= "dashed", color = "red")+
  geom_line(data = removed_trees_prefire_chron, aes(x = yrs, y = std, group = 1), color = "brown") +
  geom_line(data = retained_trees_chron, aes(x = yrs, y = std, group = 2), color = "blue") +
  geom_ribbon(data = removed_trees_prefire_chron, aes(group = 1, ymin = lowerCI, ymax = upperCI, x = yrs, fill = "Removed"),
              alpha = 0.2, color = NA) +
  geom_ribbon(data = retained_trees_chron, aes(group = 2, ymin = lowerCI, ymax = upperCI, x = yrs, fill = "Kept"),
              alpha = 0.2, color = NA) +
  labs(y="RWI (95% CI)", x = "Year") +
  ggtitle(expression( "Post-fire control comparison (1950-2018)" )) + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=15)) +
  scale_x_continuous(breaks = seq(1900, 2019, by = 5), limits = c(1950, 2018)) + 
  scale_fill_manual(name = "Chronologies", values = c(Removed = "brown",
                                                      Kept = "blue"),
                    labels = c("r > 0.65", "r < 0.65")) + 
  theme_pubclean(base_size = 24)



control_plus_fire <- cbind(control_trees_prefire, cores_start[,c(86:172)]) #88:177 if using all the cores

### Check the Expressed population signal. The standard is that it is above 0.85, which it is (0.XXX without fire trees, 0.993 with fire trees)
cont.cores.yrs <- rownames(control_plus_fire)

cont.cores.trunc <- control_plus_fire[cont.cores.yrs %in% win, ]

EPS.value(cont.cores.trunc)

### Check the Subsample signal strength as well, seems... Fine?
control_plus_fire_detrend <- detrend(control_plus_fire, method = "ModNegExp")

sss(control_plus_fire_detrend)

#### NEED THIS FOR THE NEXT SPREADSHEET (IF USING SINGLE FIRE TREES)
# controlled_tree_colnames <- colnames(control_trees_prefire)
fire_trees_rwl <- cores_start[,c(86:172)]
write.tucson(fire_trees_rwl, fname = "Fire_trees_Tucson_6-6-24.txt", prec = 0.001)

write.tucson(control_trees_prefire, fname = "Control_trees_Tucson_6-6-24.txt", prec = 0.001)
