####################################################################################################################
# Summary of benthic communities for Cobb Seamount
# 
# Objective:  Create datasets for various tables in the Cobb 2012 cruise 
#             tech report            
#
# Background: Imagery, navigation, and dive log data compiled into 
#             'Cobb Analysis.mdb'
#
# Summary:    
#
# 1. Number of taxonomic groups (ROV video, ROV photos, AUv)
# 2. Number of species on ROV transects
# 3. Counts of species on ROV transects
# 4. Community matrix
# 5. Diverity indices
# 6. Species accummulation
# 7. Ranked abundance
# 8. Species pool
# 
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       January, 2015
###################################################################################################################

setwd("H:/Pac2012-043 Tully Cobb Seamount/Sarah and Janelle analysis")

# start fresh
rm(list=ls())

library(RODBC)
library(plyr)
library(vegan)

options(useFancyQuotes=FALSE) # renders summary output corrects - EXPLAIN BETTER!

### Functions ###
ReadAccessDB <- function(filename.mdb) {
  require(RODBC) 
  connection.access <<- odbcConnectAccess(filename.mdb)  
  print(connection.access) 
  cat("------------------ connected to database --------------------------","\n")    
  #  odbcClose(connection.access) 
}

# Read in the data
ReadAccessDB("Cobb Analysis.mdb")

####################################################################################
# 1. Number of taxonomic groups
####################################################################################
# Start with ROV video data
# Determine the number of taxonomic groups in ...
# ... Video counts 
sql.video.sp <- "SELECT data_ROV_video.SpeciesID, data_ROV_video.SpeciesName
          FROM data_ROV_video
          WHERE (((data_ROV_video.SpeciesCount) Is Not Null))
          GROUP BY data_ROV_video.SpeciesID, data_ROV_video.SpeciesName;"
query.result <- sqlQuery(connection.access, sql.video.sp)
video.sp <- query.result
video.sp <- na.omit(video.sp)
video.sp.no <- nrow(video.sp)

# ... and Video relative abundance
sql.video.spAbun <- "SELECT data_ROV_video.SpeciesID, data_ROV_video.SpeciesName
          FROM data_ROV_video
          WHERE (((data_ROV_video.Abundance) Is Not Null))
          GROUP BY data_ROV_video.SpeciesID, data_ROV_video.SpeciesName;"
query.result <- sqlQuery(connection.access, sql.video.spAbun)
video.spAbun <- query.result
video.spAbun <- na.omit(video.spAbun)
video.spAbun.no <- nrow(video.spAbun)


cat("There are \"", video.sp.no, "\" taxanomic groups in ROV video counts")
cat("There are \"", video.spAbun.no, "\" taxanomic groups in ROV video relative abundance records")

#################################################################
# Continue determining number of taxonomic groups with ROV photos
#obs.photo <- sqlFetch(connection.access, "data_ROV_photo")

# Determine the number of taxonomic groups in ...
# ... Photo counts
sql.photo.sp <- "SELECT data_ROV_photo.SpeciesID, data_ROV_photo.SpeciesName
          FROM data_ROV_photo
          WHERE (((data_ROV_photo.SpeciesCount) Is Not Null))
          GROUP BY data_ROV_photo.SpeciesID, data_ROV_photo.SpeciesName;"
query.result <- sqlQuery(connection.access, sql.photo.sp)
photo.sp <- query.result
photo.sp.no <- nrow(photo.sp)

cat("There are \"", photo.sp.no, "\" taxanomic groups in ROV photo counts")

#################################################################
# Continue determining number of taxonomic groups with AUV photos
#obs.auv <- sqlFetch(connection.access, "data_AUV")

# Determine the number of taxonomic groups
sql.auv.sp <- "SELECT data_AUV.SpeciesID, data_AUV.SpeciesName
          FROM data_AUV
          WHERE (((data_AUV.SpeciesCount) Is Not Null))
          GROUP BY data_AUV.SpeciesID, data_AUV.SpeciesName;"
query.result <- sqlQuery(connection.access, sql.auv.sp)
auv.sp <- query.result
auv.sp.no <- nrow(auv.sp)

cat("AUV numbers represent Cherisse counts only")
cat("There are \"", auv.sp.no, "\" taxanomic groups in AUV photo counts")

############################################################
# Summary of total number of species observed on the cruise (combine ROV video, photos, & AUV)
sql.tot.sp <- "SELECT data_AUV.SpeciesName, data_AUV.SpeciesID
      FROM data_AUV
      GROUP BY data_AUV.SpeciesName, data_AUV.SpeciesID
      UNION SELECT data_ROV_photo.SpeciesName,data_ROV_photo.SpeciesID
      FROM data_ROV_photo
      GROUP BY data_ROV_photo.SpeciesName, data_ROV_photo.SpeciesID
      UNION SELECT data_ROV_video.SpeciesName, data_ROV_video.SpeciesID
      FROM data_ROV_video
      GROUP BY data_ROV_video.SpeciesName, data_ROV_video.SpeciesID;"
query.result <- sqlQuery(connection.access, sql.tot.sp)
tot.sp <- query.result
tot.sp <- na.omit(tot.sp)
tot.sp.no <- nrow(tot.sp)

cat("There are a total of \"",tot.sp.no,"\" taxanomic groups observed in the ROV video, photo, & AUV photos")

############################################################
# Create summary table
df.no.taxa <- data.frame(cbind(video.sp.no, video.spAbun.no, photo.sp.no, auv.sp.no, tot.sp.no))
colnames(df.no.taxa)<-c("Video Counts", "Video Relative Abundance", "Photos", "AUV", "Total")


####################################################################################
# 2. Number of species on ROV transects
####################################################################################
# obs <- sqlFetch(connection.access, "data_ROV_video")

sql.sp.cnt <- "SELECT DISTINCT data_ROV_video.SpeciesName
      FROM data_ROV_video
      WHERE (((data_ROV_video.SpeciesName) Is Not Null));"
query.result <- sqlQuery(connection.access, sql.sp.cnt)
rov.sp.no <- nrow(query.result)

cat("There are a total of \"",rov.sp.no,"\" taxanomic groups observed in the ROV video (count + rel abundance)")

####################################################################################
# 3. Counts for ROV dives
####################################################################################
# Determine counts per transect and output to text file
sql.counts <- "TRANSFORM Sum([ROV video species counts].SumOfSpeciesCount) AS SumOfSumOfSpeciesCount
          SELECT [ROV video species counts].SpeciesName
          FROM [ROV video species counts]
          GROUP BY [ROV video species counts].OrderByNo, [ROV video species counts].SpeciesName
          PIVOT [ROV video species counts].SiteName;"
query.result <- sqlQuery(connection.access, sql.counts)
transect.counts <- query.result
write.csv(transect.counts, "~/R/Cobb/ROV counts by dive.csv")


odbcClose(connection.access) 
cat("------------------ connection closed --------------------------------","\n")  
setwd("~/R/Cobb/")

####################################################################################
# 4. Community Matrix
####################################################################################
# Create the community matrix from count data; transpose the counts by dive and 
# set one column as the header for the community matrix

# Remember the species names
n <- transect.counts$SpeciesName
# Test to ensure same number of species as listed above
n
# Transpose all but the first column (SpeciesName)
benthic.community <- as.data.frame(t(transect.counts[,-1]))
# Add species names as header
colnames(benthic.community) <- n
# Convert NA's to 0's
benthic.community[is.na(benthic.community)] <- 0
# Remove HOLOTHUROIDEA, PISCES, and PLEURONECTIFORMES
benthic.community$HOLOTHUROIDEA <- NULL
benthic.community$PLEURONECTIFORMES <- NULL
benthic.community$PISCES <- NULL

write.csv(benthic.community, "~/R/Cobb/Benthic_communityNewRowNames.csv")


####################################################################################
# 5. Diversity Indices
####################################################################################
# Number of species
S <- specnumber(benthic.community)
# ###HETEROGENEITY (1) --> Shannon index of diversity
H <- diversity(benthic.community)
###HETEROGENEITY (2) --> Pielou's index of evenness
J <-H/log(S)

# Table with all heterogeneity indices
div.indices <- data.frame(cbind(benthic.community$rownames,S,H,J))
colnames(div.indices)<-c("Number of Species Observed (S)", "Shannon-Wiener (H)", "Pielou's (J)")
write.csv(div.indices, "~/R/Cobb/Diversity_indices.csv")


####################################################################################
# 6. Species Accumulation
####################################################################################

# Species Accumulation
sp1 <- specaccum(benthic.community)       
sp2 <- specaccum(benthic.community, "random")
sp2
summary(sp2)
png("Species_Accumulation.png")
  plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
        xlab="Sample",ylab="Number of Species")
        boxplot(sp2, col="yellow", add=TRUE, pch="+")
dev.off()


####################################################################################
# 7. Species abundance
####################################################################################
# Construct a rank - abundance plot using a log Normal model of species abundance
# using maximum likelihood estimation
rad <- radfit(benthic.community)
# View AIC values for each model
rad

# Log Normal 
plot(rad, model="Lognormal", legend = FALSE)
# Broken stick
plot(rad, model="Brokenstick")

# Zipf-Mandelbrot model
# hast three estimated parameters (c, Beta, gamma)
# has the lowest AIC
png("Rank_abundance_by_site.png")
  plot(rad, model="Mandelbrot", legend=FALSE, index.cond=list(c(1,6,7,8,9,10,11,12,2,3,4,5)))
dev.off()


####################################################################################
# 8. Species Pool
####################################################################################
# Number of unseen species
# Species accummulation models indicate that not all species were seen in any site
# These unseen species also belong to the species pool
# Assumption is that the number of unseen species is related to the number of rare species

# Potential to explore a little further for the primary....

# Estimate the number of unseen species within a collection of sites
specpool(benthic.community)

# If the estimation of pool size really works, we should get the same values of estimated
# richness if we take a random subset of a half of the plots (but this is rarely true)
s <- sample(nrow(benthic.community), 6)
specpool(benthic.community[s,])




  