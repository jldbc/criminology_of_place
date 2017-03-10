library(rgdal)
library(geosphere)
library(data.table)
library(ggplot2)
library(fuzzyjoin)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle")

#Author: James LeDoux
# Take all observations from incident reports file and from shapefile, calculate
# the point in the shapefile with the smallest distance to each incident report 
# observation, and append that street segment ID to that row of the incident report
# data

transportation.shapefile = readOGR(dsn="seattle_centerline.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

#convert to standard coordinate dataframe
transportation.table <- fortify(transportation.shapefile)

#seattle data
seattle = fread("seattle.csv", data.table = FALSE) #Limit n rows while this is being tuned

#time the script
ptm = proc.time()

#do the first iteration here so there's something to append to in the loop that follows
new_df = seattle[ 1:(1+399), ]
#distance matrix
D = distm(new_df[, 15:16], transportation.table[, 1:2])
new_df = cbind(new_df, transportation.table[apply(D, 1, which.min),])
#add IDs to the @data part of the spatiallines df
transportation.shapefile@data$ID = seq.int(nrow(transportation.shapefile@data))-1
new_df <- merge(new_df, transportation.shapefile, by.x = "id", by.y = "ID")

step=400
num_obs = nrow(seattle)-400
num_steps = ceiling(num_obs/step)

for( i in seq(401,nrow(seattle),by=step) ) {
  temp_df = seattle[ i:(i+399), ]
  #distance matrix
  D = distm(temp_df[, 15:16], transportation.table[, 1:2])
  temp_df = cbind(temp_df, transportation.table[apply(D, 1, which.min),])
  #add IDs to the @data part of the spatiallines df
  transportation.shapefile@data$ID = seq.int(nrow(transportation.shapefile@data))-1
  temp_df <- merge(temp_df, transportation.shapefile, by.x = "id", by.y = "ID")
  new_df = rbind(new_df, temp_df)
  print(i+399)
}

#get final time
proc.time() - ptm

#write.table(new_df, "seattle_with_shapefile.csv")
write.csv(new_df, "seattle_with_shapefile2.csv")
