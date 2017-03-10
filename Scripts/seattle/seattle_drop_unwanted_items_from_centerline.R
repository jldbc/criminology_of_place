library(rgdal)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle")

#Author: James LeDoux
# #To replicate Weisburd, we will need to cut this centerline down to 
# 24,023 street segments from the original 33,991. Drop everything besdies
# residential and streets

# NOTE: this is close to being right. Still have a few unnecessary segments though. 
# next try cropping points with lat < 47.47 (a few rogue points ended up on the 
# map which you can see when plotting this)
# currently: 24,331. required: 24,023

#view features included in the file and read it in
#per SND_Business_Rules.htm file, SND_FEACODE looks like the feature we care about here
#1: Local Street, 5: Major Street are the ones that matter
ogrInfo("seattle_centerline.geojson", layer="OGRGeoJSON")
transportation.shapefile = readOGR(dsn="seattle_centerline.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]

#convert to standard coordinate dataframe
transportation.table <- fortify(smaller_shapefile)

#let's see how it looks
head(transportation.table)