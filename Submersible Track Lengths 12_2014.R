####################################################################################################################
# Submersible Track Lengths for Cobb Cruise Tech Report
# 
# Objective:  Determine track lengths for submersible dives             
#
# Background: All tracks points plotted in ArcMap using navigation points,
#             points converted to polylines
#
# Summary:    This script opens tracklines shp file, plots tracks, extracts attribute data, 
#             determines track length, and outputs summary table and an ugly plot 
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       December, 2014
###################################################################################################################

setwd("~/Cobb/Spatial/ROV_Dives/Post Cruise")

# start fresh
rm(list=ls())

library(plyr)
library(maptools)

options(useFancyQuotes=FALSE)

# 1. Read in shapefile and attribute table 
# 2. Plot dives
# 3. Create table of tracklengths by dive name

# Read shp, plot ugly map
tracks.shp<-readShapeLines("All_vehicles") 
class(tracks.shp)
plot(tracks.shp) 
# Add code here to make this map prettier...

# Extract attribute table from shapefile 
tracks<-data.frame(tracks.shp@data) 
head(tracks)
str(tracks)

# # Be tidy
rm(tracks.shp)
setwd("~/R/Cobb") 

lengths <- subset(tracks, select=c(DiveNumber, SiteName, Length_m))
arrange(lengths, DiveNumber)
write.csv(lengths, file="Track Length Summaries.csv")

positions <- subset(tracks, select=c(DiveNumber, SiteName, start_lat, start_long, end_lat, end_long))
write.csv(positions, file="Track Position Summaries.csv")
