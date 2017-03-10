library(httr)
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis")

#LOADING DATA (note: this will be slow)
#all incidents reported in chicago 2001 to present
chicago = fread("data/chicago/chicago_2001_present.csv", data.table = FALSE)#,nrows=1000000) #Limit n rows while this is being tuned
#centerline file (coordinates of all streets within city limits)
#ogrInfo("Transportation.geojson", layer="OGRGeoJSON")
centerline.shapefile <- readOGR(dsn="data/chicago/Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
centerline.table <- fortify(centerline.shapefile)
head(centerline.table)

#number of street segments in chicago (56,320)
num_segments = length(unique(centerline.table$id))

#count(chicago, c('Block'))
frequencies = sort(table(chicago['Block']), decreasing=T)

#get number of street segments with crime on them (31,118)
num_segments_with_crime = as.data.frame(frequencies)

#see if the crime data falls within the area covered by the centerline file
#ggplot() + geom_point(data=chicago[which(chicago$Longitude>(-90.0)),], aes(x=Longitude,y=Latitude), size=.1, color="blue") +
#geom_path(data=centerline.table, aes(x=long, y=lat, group=group), size=0.2, color="red") 


#find num segments accounting for x% of total crime
find_concentration = function(percent_of_all_crime, chicago){
  totalcrime = nrow(chicago)
  crime_count_to_explain = totalcrime*percent_of_all_crime
  frequencies = sort(table(chicago['Block']), decreasing=T)
  sum = 0
  n = 1
  while(sum < crime_count_to_explain){
    #print(n)
    sum = sum(frequencies[1:n])
    n = n+1
  }
  print("number of segments needed: ")
  print(n)
  return(n)
}

find_concentration(0.5, chicago)


#~8% of street segments make up 50% of crime
#TODO: filter this data as needed, sanity check it