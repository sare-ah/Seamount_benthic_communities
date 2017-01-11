####################################################################################################################
# Submersible summaries for Cobb Cruise Tech Report
# 
# Objective:  Determine start and end times for each dive              
#
# Background:  
#
# Summary:    The GrabsobsDat() function runs on 32 bit R only
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       December, 2014
###################################################################################################################

setwd("H:/Pac2012-043 Tully Cobb Seamount/Sarah and Janelle analysis")  #### ROV DRIVE ####

# start fresh
rm(list=ls())

library(RODBC)
library(sqldf)

options(useFancyQuotes=FALSE)
options(expressions=10000) # merge() was crashing until I increase the maximum number of nested expressions that can be evaluated

# 1. Read video miner data table
# 2. For each dive, determine the minimum & maximum timestamp
#    and match this timestamp to navigation data (lat/long/depth)

### Controls###
# Location and name of the database
locDB <- "Cobb Analysis.mdb" 
# Table with ROV video data: species counts and relative abundance
obsTable <- "data_ROV_video"
# Table with navigation data: latitude and longitude
navTable <- "Pac2012-043 nav data"


### Functions ###
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
  #  # Ensure correct column types
  #  obs[, tranCol] <- factor( obs[, tranCol] )
  #  obs[, segCol] <- factor( obs[, segCol] )
  #  obs$SpeciesName <- as.character( obs$SpeciesName )
  #  obs$SpeciesID <- as.character( obs$SpeciesID )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( obs )
}  # End GrabobsDat function

# Grab the Navigational data: lat / long positions, depth
GrabNavDat <- function( db, navTN, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the species data
  nav <- sqlFetch( channel=dataBase, sqtable=navTN )
  # Message re observation data
  cat( "Imported navigational table with", nrow(nav), 
       "rows and", ncol(nav), "columns\n" )
  # If cleaning the data
  if( clean ) { 
#     nav$sectime <- as.character( nav$sectime )
    # Get date and time in proper format
    DateTime <- strptime( x=paste(nav$Date, nav$sectime), 
                          format="%Y-%m-%d %H:%M:%S", tz="" )
    # Bind date and time 
    nav <- cbind( DateTime, nav )
    # Remove junk columns
    nav <- subset( nav, select=-c(MaxOfID, dive_number, fname ) )
    # Sort data by date and time
    nav <- nav[order(nav$DateTime), ]
    # Message re navervation data
    cat( "\tCleaned-up table has", nrow(nav), "rows and", ncol(nav), 
         "columns\n" )
  }  # End if cleaning the data
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( nav )
}  # End GrabNavDat function

### Data ###
obsDat <- GrabObsDat( db=locDB, obsTN=obsTable )
navDat <- GrabNavDat( db=locDB, navTN=navTable )

# Be tidy
setwd("~/R/Cobb") #### Go back to working directory on my computer

### Summaries ###
# Determine ROV start and end ID's in video miner
sqlstr1 <- ("SELECT obsDat.TransectName, Min(obsDat.ID) AS MinID, 
          Max(obsDat.ID) AS MaxID FROM obsDat GROUP BY obsDat.TransectName;")
minerID <- data.frame(sqldf(sqlstr1))
minerID <- minerID[order(minerID$MinID), ]
colnames(minerID)<-c("TransectName", "ID","MaxID")
start <- merge(obsDat, minerID, by=c("ID"))
start <- subset( start, select=-c(TransectNum, OnBottom, DominantSubstrate, DominantPercent,
                                  SubdominantSubstrate, SubdominantPercent,
                                  SurveyModeID, ReliefID, DisturbanceID, ImageQualityID,
                                  SpeciesName, SpeciesID, SpeciesCount, Abundance, 
                                  IDConfidence, FieldOfView, TransectName.y, MaxID) )
start <- start[order(start$TransectName.x), ]
colnames(minerID)<-c("TransectName", "MinID","ID")
end <- merge(obsDat, minerID, by=c("ID"))
end <- subset( end, select=-c(TransectNum, OnBottom, DominantSubstrate, DominantPercent,
                                  SubdominantSubstrate, SubdominantPercent,
                                  SurveyModeID, ReliefID, DisturbanceID, ImageQualityID,
                                  SpeciesName, SpeciesID, SpeciesCount, Abundance, 
                                  IDConfidence, FieldOfView, TransectName.y, MinID) )

# Select only the columns of interest from the navigation data
nav <- subset( navDat, select=-c(dive, x, y, N, W))

# Force time format --- How is this stored in R?  I suspect a millisecond value is still attached
start$DateTime <- format(start$DateTime, "%H:%M:%S")
end$DateTime <- format(end$DateTime, "%H:%M:%S")
nav$DateTime <- format(nav$DateTime, "%H:%M:%S")

# Select the navigation records that match the start and end timestamps
start.pos <- merge(start, nav, by=c("DateTime"), all.x=TRUE)
end.pos <- merge(end, navDat, by=c("DateTime"), all.x=TRUE)

# Incomplete merge
# I did confirm that there should be a navigation record for each timestamp in start & end
start.pos
end.pos





