library(rgdal)
library(geosphere)
library(data.table)
library(ggplot2)
library(fuzzyjoin)
library(snow)
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



### ROADMAP ###
# 1: SORT DF BY LAT AND LONG
# 2: BREAK INTO LARGEST SIZED CHUNKS THAT SERVER WILL HANDLE W/ DISTMATS
# 3: GET MIN AND MAX LAT, LONG IN SEATTLE DF FOR THAT CHUNK
# 4: CREATE DUPLICATE OF SHAPEFILE TABLE. DROP OBSERVATIONS OUTSIDE A CERTAIN RANGE OF
#    THE MIN AND MAX VALUES FOR LAT AND LONG (I.E. ROWS THAT DEF AREN'T THE MATCH)
# THIS SHOULD SPEED UP THE DISTMAT CALCULATION AND ALLOW THIS TO FINISH EARLIER



#with trimming  
# +/- .05: (~25 vs 38 sec w/ 200 rows.  55 v. 80 w/ 400.)
# +/- .02: 44 vs 80

ptm = proc.time()
#1: sort the df by lat and long
seattle = seattle[with(seattle, order(Latitude, Longitude)), ]

#do the first iteration here so there's something to append to in the loop that follows
new_df = seattle[ 2:(1+399), ]

#min and max lat and long +/- approx. 2.5 miles (if .05) or 1.5 miles (if .02)
min_lat = min(new_df$Latitude) - 0.02
max_lat = max(new_df$Latitude) + 0.02  
min_long = min(new_df$Longitude) - 0.02
max_long = max(new_df$Longitude) + 0.02

#trim the shapefile table
temp_table = transportation.table
temp_table = temp_table[temp_table$long < max_long & temp_table$long > min_long & temp_table$lat > min_lat &
                        temp_table$lat < max_lat,]

#distance matrix
D = distm(new_df[, 15:16], temp_table[, 1:2])
new_df = cbind(new_df, temp_table[apply(D, 1, which.min),])
#add IDs to the @data part of the spatiallines df
transportation.shapefile@data$ID = seq.int(nrow(transportation.shapefile@data))-1
new_df <- merge(new_df, transportation.shapefile, by.x = "id", by.y = "ID")


proc.time() - ptm








#without trimming

ptm = proc.time()
new_df = seattle[ 2:(1+399), ]

#distance matrix
D = distm(new_df[, 15:16], transportation.table[, 1:2])
new_df = cbind(new_df, transportation.table[apply(D, 1, which.min),])
#add IDs to the @data part of the spatiallines df
transportation.shapefile@data$ID = seq.int(nrow(transportation.shapefile@data))-1
new_df <- merge(new_df, transportation.shapefile, by.x = "id", by.y = "ID")

proc.time() - ptm






fuzzy_geo_join = function(a,b){
  #distance matrix
  crime_df = seattle[a:b, ]
  D = distm(crime_df[, 15:16], transportation.table[, 1:2])
  crime_df = cbind(crime_df, transportation.table[apply(D, 1, which.min),])
  #add IDs to the @data part of the spatiallines df
  transportation.shapefile@data$ID = seq.int(nrow(transportation.shapefile@data))-1
  crime_df <- merge(crime_df, transportation.shapefile, by.x = "id", by.y = "ID")
  return(crime_df)
}


cl <- makeCluster(4)
#pass functions and data to the cluster's nodes
clusterExport(cl, list("distm"))
clusterExport(cl, list("seattle"))
clusterExport(cl, list("transportation.table"))
clusterExport(cl, list("transportation.shapefile"))





clust_out = clusterApply(cl, c(1, 101, 201, 301), 
                         fuzzy_geo_join, c(100, 200, 300, 400))







new_df = rbind(new_df, crime_df)





#get final time
proc.time() - ptm

#write.table(new_df, "seattle_with_shapefile.csv")
write.csv(new_df, "seattle_with_shapefile2.csv")
