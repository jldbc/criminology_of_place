#####################################################################
# Austin
#####################################################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/austin")

library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)

#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("austin_tx_crime.csv", data.table=FALSE)

#####  dropping non arterial / residential segments (see shapefile for CHI versions of these) #####
#df = df[which(df$SND_FEACOD %in% list(1,5)),]
#df = df[which(df$SEGMENT_TY %in% list(1,5)),]
#df = df[which(df$CITYCODE==1),]
#################################################

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[!grepl('/', df$ADDRESS),] #get rid of intersections

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(ADDRESS)))

#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="austin_streets.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

###### get chicago equivalent of this. weed out non arterial and residential streets #####
# smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]

# filter out unwanted segment types then get number of segmetns
unwanted = c("RAMP", "HWY", "BRG")
transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$street_typ %in% unwanted,]
num_segments = length(unique(transportation.shapefile@data$segment_id))
cat("num segments: ", num_segments)

#mean_segment_length = mean(transportation.shapefile@data$shape_len[complete.cases(transportation.shapefile@data$shape_len)])
#cat("Avg segment length: ", mean_segment_length, " feet")

#filter out nonviolent crime
#sort(table(df$CrimeType), decreasing=T)

violent_crimes = c("ASSAULT W/INJURY-FAM/DATE VIOL", "ASSAULT WITH INJURY", "ASSAULT BY CONTACT",
                   "ASSAULT BY CONTACT FAM/DATING", "AGG ASSAULT", "AGG ASLT STRANGLE/SUFFOCATE",
                   "AGG ROBBERY/DEADLY WEAPON", "AGG ASSAULT FAM/DATE VIOLENCE", "FELONY ENHANCEMENT/ASSLT W/INJ",
                   "ASSAULT ON PUBLIC SERVANT", "SEXUAL ASSAULT W/ OBJECT", "ASSAULT  CONTACT-SEXUAL NATURE", "MURDER",
                   "RAPE", "AGG RAPE")

df = df[df$CrimeType %in% violent_crimes,]

# incidents observed by street segment ID
#frequencies = sort(table(df['id']), decreasing=T) 
frequencies = sort(table(df['segment_id']), decreasing=T)
plot(frequencies)

# get number of street segments with crime on them (13440)
num_segments_with_crime = nrow(as.data.frame(frequencies))

#get year 
df$Year = as.numeric(substr(df$Date, nchar(df$Date)-3, nchar(df$Date)))
df = df[df$Year != "NA",]

#find num segments accounting for x% of total crime
find_concentration = function(percent_of_all_crime, df){
  totalcrime = nrow(df)
  crime_count_to_explain = totalcrime*percent_of_all_crime
  #frequencies = sort(table(df['id']), decreasing=T)
  frequencies = sort(table(df['segment_id']), decreasing=T)
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

# see top types of crime in the data (uncomment below)
#sort(table(df$Category), decreasing=T)

# values from figures 3 and 4
# note: segment_id and id return the same value.. maybe we don't need to do the join?
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments
