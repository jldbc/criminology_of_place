## remaining: 
# - sacramento_county_2007_present
# - pitt_crime (can get pct. relationship, but can't plot or regress)
# - Oakland (some lat/long parsing needed)
# - Cincinnati (no plotting, but the block level data is good)

# - Austin (lat/long unreliable, but block level data looks good)   -- this seems too concentrated. what am i missing?

# - Baton Rouge (geojoin)
# - NYC (geojoin)
# - Boston (geojoin) ** arcGIS. geocode to address. coerce to segs by given address ranges.



#####################################################################
# Baton Rouge
# to get segments, get location and geojoin shapefile for segment IDs. This format is too specific currently.
#####################################################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Baton_Rouge")

library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)

#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("baton_rouge_crime.csv", data.table=FALSE)