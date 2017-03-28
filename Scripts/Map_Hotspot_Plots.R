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

# needs:
# get segment centroids (share that piece of code with the facilities script)
# geojoin to get id from shapefile in this 
# first plot all segments
# then highlight the segments whose ids match hotspots

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
df = fread("chicago_2001_present.csv", data.table=FALSE)

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[df$Longitude > -90 & !is.na(df$Longitude),] #there are a few impossible values in this data. Need to drop these. 
df = df[!is.na(df$Block),]

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))

#get segment centroids (or at least something close to these)
# procedure:
# keep only unique lat/long pairs
# get center point by segment ID (approximately the street segment centroid)
# join these onto original df by segment id
# re-run this for those observations w/ null centroid values w/o the uniqueness constraint added and use the resulting centroids for those rows
temp_df_no_repeat_locations = df[!duplicated(df$Location),]
lats = aggregate(Latitude ~ segment_id, temp_df_no_repeat_locations, mean)  # you're allowed to take means of lat/long for centroids, right? double check
longs = aggregate(Longitude ~ segment_id, temp_df_no_repeat_locations, mean)

df = merge(x=df, y=lats, by='segment_id', all.x=TRUE)
df = merge(x=df, y=longs, by='segment_id', all.x=TRUE)

#fix some naming issues caused by the merge
df$centroid_latitude = df$Latitude.y
df$centroid_longitude = df$Longitude.y
df$Longitude = df$Longitude.x
df$Latitude = df$Latitude.x
df$Latitude.x = NULL
df$Longitude.x = NULL
df$Latitude.y = NULL
df$Longitude.y = NULL


#now to clean up the ones this missed
df_missing_centroids = df[is.na(df$centroid_latitude),]
lats = aggregate(Latitude ~ segment_id, df_missing_centroids, mean)  # you're allowed to take means of lat/long for centroids, right? double check
longs = aggregate(Longitude ~ segment_id, df_missing_centroids, mean)

df = merge(x=df, y=lats, by='segment_id', all.x=TRUE)
df = merge(x=df, y=longs, by='segment_id', all.x=TRUE)

df[is.na(df$centroid_latitude), 'centroid_latitude'] = df[is.na(df$centroid_latitude), 'Latitude.y']
df[is.na(df$centroid_longitude), 'centroid_longitude'] = df[is.na(df$centroid_longitude), 'Longitude.y']
# df[is.na(df$Longitude), 'Longitude'] = df[is.na(df$Longitude), 'Longitude.x'] 
# df[is.na(df$Latitude), 'Latitude'] = df[is.na(df$Latitude), 'Latitude.x'] 
df$Latitude = df$Latitude.x
df$Longitude = df$Longitude.x
df$Latitude.x = NULL
df$Longitude.x = NULL
df$Latitude.y = NULL
df$Longitude.y = NULL

#tests to see if this worked as intended:
# head(df)
# names(df)
# nrow(df)
# head(df[is.na(df$centroid_longitude),])
# head(df[is.na(df$Latitude),])
# View(df)

#hotspot identifiers
df$is_hotspot_25 = 0 
df$is_hotspot_50 = 0 

#filter out nonviolent crime
violent_crimes = c("BATTERY", "ASSAULT", "ROBBERY", "CRIM SEXUAL ASSAULT", "HOMICIDE",
                   "DOMESTIC VIOLENCE")

backup_df = df
df = df[df$PrimaryType %in% violent_crimes,]

#only look at one year.. hotspots vary too much YoY to see all.
#df = df[df$Year==2015,]
frequencies = sort(table(df[df$Year==hotspot_year, 'segment_id']), decreasing=T)

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

#create IDs for crimes happening at hotspots (both 25 andf 50% levels)
n_seg = find_concentration(0.25, df[df$Year==hotspot_year,])
hotspot_ids_25 = names(frequencies[1:n_seg])
n_seg = find_concentration(0.5, df[df$Year==hotspot_year,])
hotspot_ids_50 = names(frequencies[1:n_seg])

df$count = 1 #we only want to be counting violent crimes
backup_df$count = 0 
#add nonviolent rows back in.. we won't count the crimes but we want to keep those segment IDs
df = rbind(df, backup_df[!backup_df$PrimaryType %in% violent_crimes,])
df[df$segment_id %in% hotspot_ids_25, c("is_hotspot_25")] = 1 
df[df$segment_id %in% hotspot_ids_50, c("is_hotspot_50")] = 1 


#make sure all data has location
df = df[!is.na(df$Longitude),]
df = df[!is.na(df$Latitude),]


#### put data into wide format ####
# - each row is a segment 
# - features: is_hot (0/1)
# - lat / long (for the dist matrices)
# - **** add segment length before widening
# - joined on features for number of [type] facilities within y ft. 

#only keep the columns we'll need
df = df[, c("Block","segment_id", "CommunityArea", 'centroid_longitude', 'centroid_latitude', "is_hotspot_25", "is_hotspot_50", "count", "Year")]
#counts = cbind(aggregate(count~segment_id, sum, data=df[df$Year==hotspot_year,]), 
#               table(df[df$Year==hotspot_year, 'segment_id'])) #only cont observations from the target year
df$Latitude = df$centroid_latitude
df$Longitude = df$centroid_longitude
df$centroid_latitude = NULL
df$centroid_longitude = NULL

####
# DATA FORMATTING PROCEDURE:
# 1: remove duplicates of segment_id so that we have one row per street segment
# create separate table of each segment and the numbers of crimes it has seen in the chosen year
# join this separet dataframe in by id
# replace NAs of joined crime count ('n') column with zeros. These are the segments that didn't have crime that year. 
# select only the useful columns 
####

new_df = df[!duplicated(df$segment_id),] #note: approx. 1/3 of street segments don't have a community area attached to them 
counts_2015_df = df[df$count==1,] %>% count(segment_id, Year) %>% filter(Year==2015) #head(20) #crime count / yr (but only count the violent ones)
new_df = merge(x = new_df, y = counts_2015_df, by = "segment_id", all.x = TRUE) #merge counts where we have them
new_df[is.na(new_df$n), 'n'] = 0
columns_to_keep = c('segment_id', 'Block', 'CommunityArea', 'Latitude', 'Longitude',
                    'is_hotspot_25', 'is_hotspot_50', 'n')
new_df = new_df[, columns_to_keep]


#now: geojoin, plot all (black), overlay hotspots (red)
