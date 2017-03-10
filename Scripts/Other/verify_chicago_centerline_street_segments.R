library(rgdal)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Thesis/Data/chicago")

# Author: James LeDoux
# This script is for verifying that the City of Chicago centerline file’s IDs accurately identify street segments, defined as the space on a street between two intersecting roads or dead ends. Data is in geojson format, and comes from github.com/Chicago/osd-street-center-line.


#ogrInfo("Transportation.geojson", layer="OGRGeoJSON")
transportation.shapefile <- readOGR(dsn="Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

par(mar = rep(2, 4)) #need this or else the figure is too large to plot

#convert to standard dataframe
transportation.table <- fortify(transportation.shapefile)

#let's see how the data looks
head(transportation.table)

#let's hone in on one specific corner of the map
tab3 = transportation.table[order('long', 'lat')] 
tab3 = transportation.table[order(transportation.table$lat, transportation.table$long, transportation.table$group),]
#g = ggplot() + geom_path(data=tab3[1:30000,], aes(x=long, y=lat, group=group), size=0.2)
g = ggplot() + geom_path(data=tab3[1:250000,], aes(x=long, y=lat, group=group), size=0.2)

#let's try to see if street IDs are intersection-to-intersection segments 
for(i in 30000:30010){
  lat = tab3[which(tab3$id==i),][0:1,]$lat
  long = tab3[which(tab3$id==i),][0:1,]$long
  g = ggplot() + geom_path(data=tab3[which(tab3$lat<(lat+.02) & tab3$lat>(lat-.02) & tab3$long>(long-.02) & tab3$long<(long+.02)),], aes(x=long, y=lat, group=group), size=0.2)
  g2 = g + geom_path(data=tab3[which(tab3$id==i),], aes(x=long, y=lat, group=group), size=0.7, color="red")
  print(i)
  plot(g2)
}

for(i in 24875:24881){
  lat = tab3[which(tab3$id==i),][0:1,]$lat
  long = tab3[which(tab3$id==i),][0:1,]$long
  g = ggplot() + geom_path(data=tab3[which(tab3$lat<(lat+.02) & tab3$lat>(lat-.02) & tab3$long>(long-.02) & tab3$long<(long+.02)),], aes(x=long, y=lat, group=group), size=0.2)
  g2 = g + geom_path(data=tab3[which(tab3$id==i),], aes(x=long, y=lat, group=group), size=0.7, color="red")
  print(i)
  plot(g2)
}


for(i in 6690:6700){
  lat = tab3[which(tab3$id==i),][0:1,]$lat
  long = tab3[which(tab3$id==i),][0:1,]$long
  g = ggplot() + geom_path(data=tab3[which(tab3$lat<(lat+.02) & tab3$lat>(lat-.02) & tab3$long>(long-.02) & tab3$long<(long+.02)),], aes(x=long, y=lat, group=group), size=0.2)
  g2 = g + geom_path(data=tab3[which(tab3$id==i),], aes(x=long, y=lat, group=group), size=0.7, color="red")
  print(i)
  plot(g2)
}