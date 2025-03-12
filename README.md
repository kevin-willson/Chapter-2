Code for "Increasing aridity may contribute to similar tree growth responses following first-entry fires and reburns after long-term fire-exclusion"

Publisher link: https://www.sciencedirect.com/science/article/pii/S0378112724005383

Data associated with the project is described below and can be downloaded from Dryad: Willson, Kevin; Hurteau, Matthew (2024). Trees have similar growth responses to first-entry fires and reburns following long-term fire exclusion [Dataset]. Dryad. https://doi.org/10.5061/dryad.6djh9w18z

Created by Kevin G. Willson

Code:

1 - Question2-3 averaging cores.R = Code to remove undateable cores and average growth between series for each tree

2 - Question 2 analysis prep std-growth.R = Code to detrend growth data among trees from fire-maintained forests and create a long dataframe to use in future analyses

3 - Control tree selection.R = Code for selecting trees from fire-excluded forests to use as controls for analyses

4 - Control detrend prep.R = Code to detrend growth data among the 67 control trees from fire-excluded forests and create a long dataframe to use in future analyses

5 - Seascorr control check.R = Code to assess climate-growth correlation patterns among burned and control trees

6 - Five year window comparisons burned and control.R = Code to assess pre-fire growth rates among burned and control trees

7 - Control vs fire cluster bootstrap comparison.R = Code to make Figure 4, which assesses growth patterns before and after first entry fires and reburns among burned and control trees

8 - VPD figure and stats.R = Code to make Figure 2, which compares a growth chronology against annual VPD and plots the number of trees burned by year against 20 year normals for VPD. This script also include some stats used in the results section.

9 - Fire severity and RWI~VPD regression code.R = Code to quantify the relationship between tree growth (RWI), aridity (VPD), and fire severity.


Data:

Annual_tree_size.xlsx - This dataframe contains estimated annual tree sizes for all tree in the analysis. Estimates were calculated using measured tree DBH and annual growth increments from tree core data. Columns are as follows: "Fire", "Patch", "Transect", "Tree" = identification for each tree, "Year" = the year of the estimated size of the tree, "annual_diameter" = estimated dbh of tree for that year in cm. 

All_2022_Cores_Q2_Tucson.txt - This textfile contains growth data for all tree cores collected for analysis. Columns are as follows: First column = tree core series identification, Second column = decade affiliated with growth measurements, Third-Twelth columns = tree-ring width measurements (in thousands of mm). 

Fire_Occurences.xlsx - This dataframe contains number of fires and years of fires that burned through each plot, which was aggregated from monitoring trends in fire severity data, corrected fire boundaries by Parks et al. 2015, and historical fire boundaries from Rollins et al. 2002. Columns are as follows: "Fire", "Patch", and "Transect" = identification for each plot, "Num_fires" = the number of fires each plot experienced, "Fire_year_1" = the year a plot experienced its first fire, "Fire_year_2" = the year a plot experienced its second fire, "Fire_year_3" = the year a plot experienced its third fire, "Fire_year_4" = the year a plot experienced its fourth fire, "Fire_year_5" = the year a plot experienced its fifth fire.

Fire_Severity_7-4-24.xlsx - This dataframe contains the fire severity values (in relativized burn ratio values, RBR) for every fire that burned plots from 1950-2012. Data was gathered from RBR raster data calculated by Parks et al. 2015. Columns are as follows: "Fire", "Patch", and "Transect" = identification for each plot, "Year" = the year of a fire, "Severity" = RBR value for that plot of that fire, "First_Sub" = delineation for whether the first was the first-entry fire ("First") that burned the plot after fire-exclusion or a reburn ("Subsequent").

PRISM_Monthly_Climate_1900-1915_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1900-1915, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals. 

PRISM_Monthly_Climate_1915-1930_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1915-1930, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_1930-1945_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1930-1945, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_1945-1960_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1945-1960, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_1960-1975_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1960-1975, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_1975-1990_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1975-1990, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_1990-2005_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 1990-2005, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

PRISM_Monthly_Climate_2005-2020_SPEI.csv - This dataframe contains monthly climate data for all plots in the study from 2005-2020, which was used to quantify annual VPD values for that period and to assess seasonal climate-growth correlation patterns with temperature and precipitation. Columns are as follows: "Name" = identification for each plot, "Longitude" = longitude for each plot location, "Latitude" = latitude for each plot location, "Elevation" = elevation of the centroid for the 4-km grid cell the plot occurred in, "Date" = Year and month of the climate data point, "ppt (mm)" = total precipitation for that year/month in millimeters, "tmin (degrees C)" = minimum temperature recorded during that year/month in degrees Celsius, "tmax (degree C)" = maximum temperature recorded during that year/month in degrees Celsius, "vpdmax (hPa)" = maximum vapor pressure deficit recorded during that year/month in Hectopascals.

Spatial Data:

Fire_recurrence_shapefile.shp - Shapefile of fire history in the Gila from 1909-2018 used to make Figure 1. Locations did not experience any fire until 1950 at the earliest. 

Gila_DEM.tif - 10 meter elevation raster across the Gila national forest used to calculate hillshade in the Gila wilderness. 

Gila_hillshade.tif - 10 meter hillshade of the Gila wilderness used to make Figure 1. 
