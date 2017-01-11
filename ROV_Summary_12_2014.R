####################################################################################################################
# Submersible summaries for Cobb Cruise Tech Report
# 
# Objective:  For each dive determine:
#                 beginning and ending timestamp
#                 beginning and ending depth
#                 average field of view
#                 dominant substrate codes
#                 relief ID codes
#
# Background: Data compiled from dive log, navigation, video miner in MS Access database 
#
# Summary:    Connect to database, run queries from R, read table, summarize data with xtabs() & ddply()
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       December, 2014
###################################################################################################################

# start fresh
rm(list=ls())

setwd("H:/Pac2012-043 Tully Cobb Seamount/Sarah and Janelle analysis")

library(RODBC)
library(plyr)

options(useFancyQuotes=FALSE)

## 1. Access Cobb Analysis db
## 2. Read video miner data table
## 3. For each dive, determine the beginning and ending timestamp and depth
## 4. For each dive, determine average field of view, summarize dominant substrate, and relief codes

### Functions ###
ReadAccessDB <- function(filename.mdb) {
  require(RODBC) 
  connection.access <<- odbcConnectAccess(filename.mdb)  
  print(connection.access) 
  cat("------------------ connected to database --------------------------","\n")    
#  odbcClose(connection.access) 
}

### Data ###
ReadAccessDB("Cobb Analysis.mdb")
obs <- sqlFetch(connection.access, "data_ROV_video")

### Queries & Summaries ###

# Determine depth range for each dive
query.depths.start <- "SELECT [ROV dive sites].SiteName, [Transect Times].TransectName,
      [Transect Times].MinOfTimeCode, B_TRACKING.Depth
      FROM B_TRACKING INNER JOIN ((B_DIVES INNER JOIN ([ROV dive sites]
      INNER JOIN [Transect Times] ON [ROV dive sites].Dive = [Transect Times].TransectName)
      ON B_DIVES.DiveNumber = [ROV dive sites].DiveNumber) INNER JOIN data_ROV_video ON
      [Transect Times].MinOfTimeCode = data_ROV_video.TimeCode) ON (B_DIVES.DiveID = B_TRACKING.DiveID)
      AND (B_TRACKING.charTime = data_ROV_video.TimeText)
      GROUP BY [ROV dive sites].SiteName, [Transect Times].TransectName,
      [Transect Times].MinOfTimeCode, B_TRACKING.Depth;"
query.result <- sqlQuery(connection.access, query.depths.start)
depths.start <- query.result
depths.start$MinOfTimeCode <- format(depths.start$MinOfTimeCode, "%H:%M:%S")
depths.start <- depths.start[order(depths.start$TransectName),]
depths.start

query.depths.end <- "SELECT [ROV dive sites].SiteName, [Transect Times].TransectName,
    [Transect Times].MaxOfTimeCode, B_TRACKING.Depth
    FROM (B_DIVES INNER JOIN ([ROV dive sites] INNER JOIN [Transect Times] ON
    [ROV dive sites].Dive = [Transect Times].TransectName) ON
    B_DIVES.DiveNumber = [ROV dive sites].DiveNumber) INNER JOIN
    (B_TRACKING INNER JOIN data_ROV_video ON B_TRACKING.charTime = data_ROV_video.TimeText)
    ON (B_DIVES.DiveID = B_TRACKING.DiveID) AND ([Transect Times].MaxOfTimeCode = data_ROV_video.TimeCode)
    GROUP BY [ROV dive sites].SiteName, [Transect Times].TransectName,
    [Transect Times].MaxOfTimeCode, B_TRACKING.Depth;"
query.result <- sqlQuery(connection.access, query.depths.end)
depths.end <- query.result
depths.end$MaxOfTimeCode <- format(depths.end$MaxOfTimeCode, "%H:%M:%S")
depths.end

# Be tidy
odbcClose(connection.access)
cat("------------------ connection closed ------------------------------","\n")    
setwd("~/R/Cobb")

# Make transect name a factor
obs$TransectName <- as.factor(obs$TransectName)

# Calculate field of view, remove NULL Field of View values 
obs.fov <- subset(obs, DataCode == 15)
foView <- ddply(obs.fov, .(TransectName, TransectNum), summarize, meanFoV = mean(FieldOfView),
              sd = sd(FieldOfView), count = length(FieldOfView))
foView
colnames(foView) <- c("SiteName", "TransectName", "meanFoV","sd", "count")
foView <- foView[order(foView$TransectName),]

# Hard code in end of dive 18, site DFO_8 
dfo_8 <- data.frame(SiteName="DFO_8",TransectName="18",MaxOfTimeCode="02:23:11",Depth="202")
depths.end <- rbind(depths.end, dfo_8)
depths.end$TransectName <- as.numeric(depths.end$TransectName)
depths.end <- depths.end[order(depths.end$TransectName),]
depths.end

# Write out results to .csv
summary <- depths.start
summary <- cbind(summary, depths.end$MaxOfTimeCode, depths.end$Depth, foView$meanFoV, foView$sd, foView$count)
colnames(summary) <- c("Site Name", "Transect Number", "Start Time", "Start Depth (m)", "End Time", "End Depth (m)", "Field of View (cm)","sd FoV", "count FoV")
summary[,c(2,1,3,5,4,6,7,8,9)]
write.csv(summary, "ROV Summary.csv", row.names=TRUE)


# Habitat summary ....
obs$DominantSubstrate <- as.factor(obs$DominantSubstrate)
obs$DominantSubstrate <- revalue(obs$DominantSubstrate,
                               c("1"="Bedrock, smooth", "2"="Bedrock with crevices","3"="Boulders",
                                 "4"="Cobble", "5"="Gravel","7"="Sand","10"="Crushed Shell",
                                 "11"="Whole Shell","14"="Coral Rubble"))
hab<-xtabs(~TransectName+DominantSubstrate, data=obs, na.action=na.exclude)
write.csv(hab, "ROV Dominant Substrate.csv", row.names=TRUE) 
hab

# Relief summary ....
obs$ReliefID <- as.factor(obs$ReliefID)
obs$ReliefID <- revalue(obs$ReliefID, c("1"="Flat or rolling","2"="Vertical relief 0.5 - 2m",
                                      "3"="Vertical relief > 2m","4"="Slope or wall"))
relief <- (xtabs(~TransectName+ReliefID, data=obs, na.action=na.exclude))
write.csv(relief, "ROV Relief.csv", row.names=TRUE) 
relief



