#########################################
# Map Plots
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(tidyr)
library(ggmap)
library(MASS)
library(cowplot)

# Data preparation approach:
#   1. remove nonsensical rows (there are a few with obviously wrong lat/longs)
#   2. create a copy of the data 
#   3. select only the violent crimes in dataframe 1 
#   4. get 25 and 50% concentration levels in dataframe 1. Create an is_hotspot column that equals 1 for these segments
#   5. Create a variable called count, equal to 1 in df1 and 0 in df2. We only want to count violent crimes, but need to use df2 to
#      capture some of our violent-crimeless street segments (the negative examples)
# 

hotspot_year = 2015  #year we are modeling

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
#df = fread("chicago_2001_present.csv", data.table=FALSE)
df = fread('../../chicago_with_shapefile2.csv', data.table=FALSE)

#shapefile
transportation.shapefile <- readOGR(dsn="Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

par(mar = rep(2, 4)) #need this or else the figure is too large to plot

#convert to standard dataframe
transportation.table = fortify(transportation.shapefile)

#let's see how the data looks
head(transportation.table)

#join tables to have hotspot status on the fortified street network table 
merge_df = df[,c('is_hotspot_25', 'is_hotspot_50', 'group')]
table_with_hotspots = merge(x = transportation.table, y = merge_df, by = "group", all.x = TRUE)
table_with_hotspots$is_hotspot_25 = as.factor(table_with_hotspots$is_hotspot_25)
# . . . WIP . . . 
table_with_hotspots[is.na(table_with_hotspots$is_hotspot_25), 'is_hotspot_25'] = 0
#let's hone in on one specific corner of the map
#tab3 = transportation.table[order(transportation.table$lat, transportation.table$long, transportation.table$group),]
#g = ggplot() + geom_path(data=tab3[1:30000,], aes(x=long, y=lat, group=group), size=0.2)
#g = ggplot() + geom_path(data=tab3[1:30000,], aes(x=long, y=lat, group=group), size=0.2)
palette(c("black","red"))
g = ggplot() + geom_path(data=table_with_hotspots, aes(x=long, y=lat, group=group, col=is_hotspot_25), size=0.2) + scale_color_manual(values=c("#3a3434", "#e44849"))
