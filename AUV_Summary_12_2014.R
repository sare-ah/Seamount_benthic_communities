####################################################################################################################
# Submersible summaries for Cobb Cruise Tech Report
# 
# Objective:  Determine dominant substrate, relief ID, and transect start/end positions and
#             depths for each dive              
#
# Background: AUV tracking data compiled by Curt Whitmore and stored in personal geodatabase 
#             Habitat and relief data located in video miner database
#
# Summary:    The GrabsobsDat() function runs on 32 bit R only
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       December, 2014
###################################################################################################################

setwd("H:/Pac2012-043 Tully Cobb Seamount/Sarah and Janelle analysis")

# start fresh
rm(list=ls())

library(RODBC)
library(plyr)

options(useFancyQuotes=FALSE)
options(expressions=10000)


### Controls###
###############
# Location and name of the database
locDB <- "Cobb Analysis.mdb" 
# Tables with ROV video data, tracking, and dive site
obsTable <- "data_AUV"


### Functions ###
#################
ReadAccessDB <- function(filename.mdb) {
  require(RODBC) 
  connection.access <<- odbcConnectAccess(filename.mdb)  
  print(connection.access) 
  cat("------------------ Reading Tables --------------------------","\n")    
  #  odbcClose(connection.access) 
}

# Grab the VideoMiner data: observations of species, habitat, etc
GrabObsDat <- function( db, obsTN, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the species data
  obs <- sqlFetch( channel=dataBase, sqtable=obsTN )
  # Message re observation data
  cat( "Imported species and habitat observation table with", nrow(obs), 
       "rows and", ncol(obs), "columns\n" )
  # If cleaning the data
  if( clean ) {
    # Remove junk rows
    obs <- subset( obs, subset=!is.na(DataCode) )
      
    # Get date and time in proper format
    DateTime <- strptime( x=paste(obs$TransectDate, obs$TimeText), 
                          format="%Y-%m-%d %H:%M:%S", tz="" )
    # Bind date and time to observations
    obs <- cbind( DateTime, obs )
    # Remove junk columns
    obs <- subset( obs, select=-c(ProtocolID, Side, Range, Length, 
                                  Height, Width, Comment, X, Y, Z, FileName,
                                  ElapsedTime, ReviewedDate, ReviewedTime) ) #TransectDate, TimeText
    # Sort data by date and time
    obs <- obs[order(obs$DateTime), ]
    # Message re observation data
    cat( "\tCleaned-up table has", nrow(obs), "rows and", ncol(obs), 
         "columns\n" )
  }  # End if cleaning the data
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( obs )
}  # End GrabobsDat function

### Data & Queries ###
######################
# Hard code in AUV dive number and site names
dives <- c("3","6","8","13")
sites <- c("AUV_1","AUV_5","AUV_2","AUV_4")
miner_num <-c("1","2","3","4")
auv.sites <- data.frame(dives,sites,miner_num)


# Grab the raw data
obsDat <- GrabObsDat( db=locDB, obsTN=obsTable )
setwd("~/R/Cobb")

# ROV Habitat summary ....
obsDat$DominantSubstrate<-as.factor(obsDat$DominantSubstrate)
h <- levels(obsDat$DominantSubstrate)
h
obsDat$DominantSubstrate<-revalue(obsDat$DominantSubstrate, c("1"="Bedrock, smooth",
    "2"="Bedrock with crevices","3"="Boulders","4"="Cobble", "5"="Gravel","6"="Pea Gravel", "7"="Sand"))
hab<-xtabs(~TransectName+DominantSubstrate, data=obsDat)
write.csv(hab, "AUV Dominant Substrate.csv", row.names=TRUE) 
hab

# ROV Relief summary ....
obsDat$ReliefID<-as.factor(obsDat$ReliefID)
r <- levels(obsDat$ReliefID)
r
obsDat$ReliefID<-revalue(obsDat$ReliefID, c("1"="Flat or rolling","2"="Vertical relief 0.5 - 2m",
                                      "3"="Vertical relief > 2m"))
relief<-(xtabs(~TransectName+ReliefID, data=obsDat))
write.csv(relief, "AUV Relief.csv", row.names=TRUE) 
relief

# Determine start / end locations, times, & depths
setwd("~/Cobb/Spatial/AUV_Dives") 
ReadAccessDB("AUV_tracking_files.mdb")

query.start <- "SELECT AUV_ObjectIDs.SiteName, d20120724_Merge.Time_ AS Start_time,
    d20120724_Merge.Latitude_F AS Start_lat, d20120724_Merge.Longitude_ AS Start_long,
    d20120724_Merge.WaterDepth AS Start_depth
    FROM AUV_ObjectIDs INNER JOIN d20120724_Merge ON
    AUV_ObjectIDs.MinOfOBJECTID = d20120724_Merge.OBJECTID;"
query.result1 <- sqlQuery(connection.access, query.start)
auv.pos <- query.result1
query.end <- "SELECT AUV_ObjectIDs.SiteName, d20120724_Merge.Time_ AS End_time,
    d20120724_Merge.Latitude_F AS End_lat, d20120724_Merge.Longitude_ AS End_long, 
    d20120724_Merge.WaterDepth AS End_depth
    FROM AUV_ObjectIDs INNER JOIN d20120724_Merge ON
    AUV_ObjectIDs.MaxOfOBJECTID = d20120724_Merge.OBJECTID;"
query.result2 <- sqlQuery(connection.access, query.end)
auv.pos <- cbind(auv.pos, query.result2)
auv.pos$Start_time <- strptime(auv.pos$Start_time, "%H:%M:%S")
auv.pos$Start_time <- format(auv.pos$Start_time, "%H:%M:%S")
auv.pos$End_time <- strptime(auv.pos$End_time, "%H:%M:%S")
auv.pos$End_time <- format(auv.pos$End_time, "%H:%M:%S")
auv.pos <- auv.pos[,-6]
auv.pos[,c(1,2,6,3,4,7,8,5,9)]

setwd("~/R/Cobb")
write.csv(auv.pos, "AUV dive positions.csv", row.names=TRUE)

# Be tidy
odbcClose(connection.access)
cat("------------------ connection closed ------------------------------","\n")    

