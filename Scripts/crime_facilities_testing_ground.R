#########################################
# Facilities and Crime
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(tidyr)
library(ggmap)
library(MASS)
library(fmsb)
library(car)

# Data preparation approach:
#   1. remove nonsensical rows (there are a few with obviously wrong lat/longs)
#   2. create a copy of the data 
#   3. select only the violent crimes in dataframe 1 
#   4. get 25 and 50% concentration levels in dataframe 1. Create an is_hotspot column that equals 1 for these segments
#   5. Create a variable called count, equal to 1 in df1 and 0 in df2. We only want to count violent crimes, but need to use df2 to
#      capture some of our violent-crimeless street segments (the negative examples)
# 



hotspot_year = 2015  #year we are modeling

#setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
#df = fread("chicago_2001_present.csv", data.table=FALSE)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
df = fread("chi_data_processed.csv", data.table=FALSE)

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[df$Longitude > -90,] #there are a few impossible values in this data. Need to drop these. 

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))

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
df = df[, c("Block","segment_id", "CommunityArea", "Latitude", "Longitude", "is_hotspot_25", "is_hotspot_50", "count", "Year")]
#counts = cbind(aggregate(count~segment_id, sum, data=df[df$Year==hotspot_year,]), 
#               table(df[df$Year==hotspot_year, 'segment_id'])) #only cont observations from the target year

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

####
# IMPUTING MISSING COMMUNITY AREAS 
# load in spatial polygon file
# identify which polygon each lat/long point belongs to 
# take the community area identifier from said polygons
# source: https://www.nceas.ucsb.edu/scicomp/usecases/point-in-polygon
# 2.05% error rate in this. Not bad. 
# result: we go from 18,000 to 20 rows without corresponding community areas
####
community_areas_boundaries = readOGR('/Users/jamesledoux/Downloads/community_area_boundaries.geojson')
coordinates(new_df) <- c("Longitude", "Latitude")
proj4string(new_df) = proj4string(community_areas_boundaries) #we can do this since we know they use the same point system
new_df$area = over(new_df, community_areas_boundaries)$area_numbe
new_df$area = as.numeric(as.character(new_df$area))
new_df = as.data.frame(new_df)
new_df[is.na(new_df$CommunityArea), c('CommunityArea')] = new_df[is.na(new_df$CommunityArea), c('area')]


##### load in drug treatment center data ######
drug_centers = fread('drug_centers.csv', data.table=FALSE)
drug_centers$loc = gsub(".*\n","",drug_centers$`Physical Address`)
drug_centers <- drug_centers %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
drug_centers$Latitude = as.numeric(drug_centers$Latitude)
drug_centers$Longitude = as.numeric(drug_centers$Longitude)

dists = distm(new_df[,c("Latitude", "Longitude")], drug_centers[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$drug_centers_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$drug_centers_800ft = out

##### load in public school data #####
schools = read.csv("public_schools.csv")
schools = schools[!is.na(schools$School_Latitude),]
schools = schools[!is.na(schools$School_Longitude),]

#calculate distance matrix between schools and crime locations (in meters, calc. by haversine distance)
dists = distm(new_df[,c("Latitude", "Longitude")], schools[,c("School_Latitude", "School_Longitude")])
#make proximity to schools a feature
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$schools_400ft = out 
out = apply(dists, 1, function(x) sum(x<243 & x>=121)) #121 m. ~ 400 ft. ~ one segment length
new_df$schools_800ft = out 

##### load in subway system data #####
trains = read.csv("chicago_train_system.csv")
trains <- trains %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
trains$Latitude = as.numeric(trains$Latitude)
trains$Longitude = as.numeric(trains$Longitude)
trains = trains[!duplicated(trains$STATION_NAME),]

dists = distm(new_df[,c("Latitude", "Longitude")], trains[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$subway_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$subway_800ft = out
# plot the sub stops if curious
# mapdf = trains[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 11) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)


##### load in bar and tavern data #####
bars = read.csv("active_liquor_licenses.csv") #Outdoor Patio seems to have some bars as well, but mixed w/ restaurants 
keeps = c("Tavern", "Late Hour", "Music and Dance")  #maybe of interest: Package Goods includes liquor stores
bars = bars[bars$LICENSE.DESCRIPTION %in% keeps,]
bars = bars[!duplicated(bars$LOCATION),]
bars = bars[!duplicated(bars$ADDRESS),]
bars = bars[!is.na(bars$LATITUDE),]
bars = bars[!is.na(bars$LONGITUDE),]

dists = distm(new_df[,c("Latitude", "Longitude")], bars[,c("LATITUDE", "LONGITUDE")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$bars_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$bars_800ft = out


#### proximity to city center ####  (might want to log-transform this)
dists = distm(new_df[,c("Latitude", "Longitude")], c(41.8785, -87.6357)) #41.878562, -87.635770
out = apply(dists, 1, function(x) sum(x<3218.69)) #within 2 mi of downtown
new_df$dist_to_city_center = dists
new_df$close_to_downtown = out
new_df$log_dist_city_center = log(dists)  #need to add this to regressions


#### vacant property ####
vac_land = read.csv("vacant_property.csv")
vac_land$loc = gsub(".*\n","",vac_land$Location)
vac_land = vac_land[vac_land$loc != "",]
vac_land = vac_land[!duplicated(vac_land$Location),]
vac_land = vac_land %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
vac_land$Latitude = as.numeric(vac_land$Latitude)
vac_land$Longitude = as.numeric(vac_land$Longitude)


#initialize to zero since we're going to be doing this in chunks
new_df$vacant_land_400ft = 0
new_df$vacant_land_800ft = 0

dists = distm(new_df[1:10000,c("Latitude", "Longitude")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[1:10000, c('vacant_land_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[1:10000, c('vacant_land_800ft')] = out

dists = distm(new_df[10001:20000,c("Latitude", "Longitude")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[10001:20000, c('vacant_land_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[10001:20000, c('vacant_land_800ft')] = out

dists = distm(new_df[20001:30000,c("Latitude", "Longitude")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[20001:30000, c('vacant_land_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[20001:30000, c('vacant_land_800ft')] = out

dists = distm(new_df[30001:40000,c("Latitude", "Longitude")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[30001:40000, c('vacant_land_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[30001:40000, c('vacant_land_800ft')] = out

dists = distm(new_df[40000:nrow(new_df),c("Latitude", "Longitude")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[40000:nrow(new_df), c('vacant_land_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[40000:nrow(new_df), c('vacant_land_800ft')] = out


###########  BUS STOPS  ###############  (chunk this into groups of 10k distance matrices)
# need to break this into chunks similar to how I did with the vacant land)
bus_stops = read.csv("bus_routes_chicago.csv")
bus_stops <- bus_stops %>% extract(location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
bus_stops$Latitude = as.numeric(bus_stops$Latitude)
bus_stops$Longitude = as.numeric(bus_stops$Longitude)

dists = distm(new_df[,c("Latitude", "Longitude")], bus_stops[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$bus_stops_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$bus_stops_800ft = out

#bus_stops = bus_stops[!duplicated(bus_stops[,c('on_street', 'cross_street', 'routes')]),]
# wow that's a lot.. website says there are >10k though so it should be correct http://www.transitchicago.com/about/facts.aspx
# mapdf = bus_stops[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 11) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

###########  BUSINESS LICENSES  ###############   (need to add features + calculate distance matrices)

businesses = read.csv('business_licenses_chicago.csv')
businesses = businesses[businesses$CITY=='CHICAGO',]
#sort(table(businesses$LICENSE.DESCRIPTION))   #see which types of business are in the data
businesses = businesses[businesses$LICENSE.DESCRIPTION != 'Tavern',] #we already have data on bars
general_business = businesses[businesses$LICENSE.DESCRIPTION %in% c('Limited Business License', 'Regulated Business License'),]
food = businesses[businesses$LICENSE.DESCRIPTION %in% c('Retail Food Establishment', 'Consumption on Premises - Incidental Activity'),]
parking_garages = businesses[businesses$LICENSE.DESCRIPTION %in% c('Public Garage', 'Accessory Garage'),]
liquor_stores = businesses[businesses$LICENSE.DESCRIPTION =='Package Goods',]
childrens_services_and_daycare = businesses[businesses$LICENSE.DESCRIPTION == "Children's Services Facility License",]
animal_care = businesses[businesses$LICENSE.DESCRIPTION == "Animal Care License",]
gas_station = businesses[businesses$LICENSE.DESCRIPTION == "Filling Station",]
pawn = businesses[businesses$LICENSE.DESCRIPTION == "Pawnbroker",]
arts_music = businesses[businesses$LICENSE.DESCRIPTION %in% c('Performing Arts Venue', 'Music and Dance'),]
# places of amusement? seems to be a problematic overlap w/ bars in this license. an interaction might fix this though. 


###########  GROCERY STORES  ###############
groceries = read.csv('grocery_stores_2013_chicago.csv')
dists = distm(new_df[,c("Latitude", "Longitude")], groceries[,c("LATITUDE", "LONGITUDE")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$groceries_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$groceries_800ft = out


###########  GRAFFITI  ###############  (break this job into subsections)
graffiti = read.csv('/Users/jamesledoux/Documents/Research/Thesis/Data/chicago/graffiti_removal.csv')
#remove duplicate reports
graffiti= graffiti[graffiti$Status != "Completed - Dup",]
graffiti= graffiti[graffiti$Status != "Open - Dup",]
#get year as a feature then select only graffiti removals that happened that year or the year before
graffiti$Creation.Date = as.character(graffiti$Creation.Date)
graffiti$Year = as.numeric(substr(graffiti$Creation.Date, nchar(graffiti$Creation.Date)-3, nchar(graffiti$Creation.Date)))
graffiti = graffiti[graffiti$Year %in% c(hotspot_year-1, hotspot_year),]

new_df$graffiti_400ft = 0
new_df$graffiti_800ft = 0

dists = distm(new_df[1:10000,c("Latitude", "Longitude")], graffiti[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[1:10000, c('graffiti_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[1:10000, c('graffiti_800ft')] = out

dists = distm(new_df[10001:20000,c("Latitude", "Longitude")], graffiti[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[10001:20000, c('graffiti_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[10001:20000, c('graffiti_800ft')] = out

dists = distm(new_df[20001:30000,c("Latitude", "Longitude")], graffiti[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[20001:30000, c('graffiti_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[20001:30000, c('graffiti_800ft')] = out

dists = distm(new_df[30001:40000,c("Latitude", "Longitude")], graffiti[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[30001:40000, c('graffiti_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[30001:40000, c('graffiti_800ft')] = out

dists = distm(new_df[40000:nrow(new_df),c("Latitude", "Longitude")], graffiti[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df[40000:nrow(new_df), c('graffiti_400ft')] = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df[40000:nrow(new_df), c('graffiti_800ft')] = out


###########  SENIOR CENTERS  #############
senior_centers = read.csv('senior_centers_chicago.csv')
senior_centers$loc = gsub(".*\n","",senior_centers$`LOCATION`)
senior_centers = senior_centers %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
senior_centers$Latitude = as.numeric(senior_centers$Latitude)
senior_centers$Longitude = as.numeric(senior_centers$Longitude)
senior_centers = senior_centers[!is.na(senior_centers$Latitude),]

dists = distm(new_df[,c("Latitude", "Longitude")], senior_centers[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$senior_centers_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$senior_centers_800ft = out

# mapdf = senior_centers[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 11) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)




###########  PARKS   ###############
# for future improvement.. see if each 'type' of park should remain in this data 
# -> table(parks$PARK.CLASS)
parks = read.csv('parks_chicago.csv')
parks$loc = gsub(".*\n","",parks$`LOCATION`)
parks = parks %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
parks$Latitude = as.numeric(parks$Latitude)
parks$Longitude = as.numeric(parks$Longitude)
parks = parks[!is.na(parks$Latitude),]

dists = distm(new_df[,c("Latitude", "Longitude")], parks[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$parks_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$parks_800ft = out

# mapdf = parks[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 13) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

###### bring in census tract age data ######
# - read in census tract shapefile. 
# - assign each st. segment to its census tract to get this feature in the dataframe. 
# - read in census-tract-level population age data
# - clean census tract column so that it ends after first column (matching the other data's census tract naming scheme)
# - merge age data onto crime data on census tract columns
census_tract_boundaries = readOGR('census_tracts.geojson')
coordinates(new_df) <- c("Longitude", "Latitude")
proj4string(new_df) = proj4string(census_tract_boundaries) #we can do this since we know they use the same point system
new_df$census_tract = over(new_df, census_tract_boundaries)$namelsad10
new_df = as.data.frame(new_df)

population_age = read.csv("census_block_ages.csv")
population_age$Geography = gsub("(.*),.*", "\\1", population_age$Geography)  #keep only the census tract from this column
population_age$Geography = gsub("(.*),.*", "\\1", population_age$Geography)  

new_df = merge(x = new_df, y = population_age, by.x = "census_tract", by.y="Geography", all.x = TRUE)

##### bring in demographic data #####
community_data = read.csv('socioeconomic_indicators.csv')

new_df_merged = merge(x = new_df, y = community_data, by.x = "CommunityArea", by.y="Community.Area.Number", all.x = TRUE)

#mean impute the data.. NAs are for O'Hare airport, which is a unique case
for(i in 1:ncol(new_df_merged)){
  new_df[is.na(new_df_merged[,i]), i] <- mean(new_df_merged[,i], na.rm = TRUE)
}

#new_df_merged = read.csv("chicago_processed_data.csv")  #skip the processing

#write.csv(new_df_merged, 'chi_data_processed.csv')

# removed ' + PERCENT.AGED.UNDER.18.OR.OVER.64'
all_features_count = "n ~ schools_400ft + schools_800ft + subway_400ft + subway_800ft + bars_400ft + bars_800ft + 
log_dist_city_center + close_to_downtown + drug_centers_400ft + drug_centers_800ft + 
bus_stops_400ft + bus_stops_800ft + groceries_400ft + groceries_800ft + senior_centers_400ft + senior_centers_800ft + 
parks_400ft + parks_800ft + PERCENT.OF.HOUSING.CROWDED + PERCENT.AGED.16..UNEMPLOYED + 
HARDSHIP.INDEX + PER.CAPITA.INCOME + PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA + PERCENT.HOUSEHOLDS.BELOW.POVERTY + 
businesses_400ft + businesses_800ft + restaurants_400ft + restaurants_800ft + parking_garagess_400ft + 
parking_garages_800ft + liquor_stores_400ft + liquor_storess_800ft + daycares_400ft + daycares_800ft + 
animal_care_400ft + animal_care_800ft + gas_stations_400ft + gas_stations_800ft + pawn_400ft + pawn_800ft + 
arts_venues_400ft + arts_venues_800ft + graffiti_400ft + graffiti_800ft + X5_9 + X10_14 + X15_19 + X20_24 + 
X25_29 + X30_34  + X35_39 + X40_44 + X45_49 + X50_54 + X55_59 + X60_64 + X65_69 + X70_74 + X75_79 + X80_84 + X85_plus"


all_features_binary_25 = "is_hotspot_25 ~ schools_400ft + schools_800ft + subway_400ft + subway_800ft + bars_400ft + bars_800ft + 
log_dist_city_center + close_to_downtown + drug_centers_400ft + drug_centers_800ft + 
bus_stops_400ft + bus_stops_800ft + groceries_400ft + groceries_800ft + senior_centers_400ft + senior_centers_800ft + 
parks_400ft + parks_800ft + PERCENT.OF.HOUSING.CROWDED + PERCENT.AGED.16..UNEMPLOYED + 
HARDSHIP.INDEX + PER.CAPITA.INCOME + PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA + PERCENT.HOUSEHOLDS.BELOW.POVERTY + 
businesses_400ft + businesses_800ft + restaurants_400ft + restaurants_800ft + parking_garagess_400ft + 
parking_garages_800ft + liquor_stores_400ft + liquor_storess_800ft + daycares_400ft + daycares_800ft + 
animal_care_400ft + animal_care_800ft + gas_stations_400ft + gas_stations_800ft + pawn_400ft + pawn_800ft + 
arts_venues_400ft + arts_venues_800ft + graffiti_400ft + graffiti_800ft + X5_9 + X10_14 + X15_19 + X20_24 + 
X25_29 + X30_34  + X35_39 + X40_44 + X45_49 + X50_54 + X55_59 + X60_64 + X65_69 + X70_74 + X75_79 + X80_84 + X85_plus"

all_features_binary_50 = "is_hotspot_25 ~ schools_400ft + schools_800ft + subway_400ft + subway_800ft + bars_400ft + bars_800ft + 
log_dist_city_center + close_to_downtown + drug_centers_400ft + drug_centers_800ft + 
bus_stops_400ft + bus_stops_800ft + groceries_400ft + groceries_800ft + senior_centers_400ft + senior_centers_800ft + 
parks_400ft + parks_800ft + PERCENT.OF.HOUSING.CROWDED + PERCENT.AGED.16..UNEMPLOYED + 
HARDSHIP.INDEX + PER.CAPITA.INCOME + PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA + PERCENT.HOUSEHOLDS.BELOW.POVERTY + 
businesses_400ft + businesses_800ft + restaurants_400ft + restaurants_800ft + parking_garagess_400ft + 
parking_garages_800ft + liquor_stores_400ft + liquor_storess_800ft + daycares_400ft + daycares_800ft + 
animal_care_400ft + animal_care_800ft + gas_stations_400ft + gas_stations_800ft + pawn_400ft + pawn_800ft + 
arts_venues_400ft + arts_venues_800ft + graffiti_400ft + graffiti_800ft + X5_9 + X10_14 + X15_19 + X20_24 + 
X25_29 + X30_34  + X35_39 + X40_44 + X45_49 + X50_54 + X55_59 + X60_64 + X65_69 + X70_74 + X75_79 + X80_84 + X85_plus"



#all_features_count = "n ~ X20_24 + X85_plus"
new_df_merged = df
summary(lm(data=new_df_merged, all_features_count))
summary(glm(data=new_df_merged, all_features_count), family='poisson')
summary(glm(data=new_df_merged, all_features_binary_50), family='binomial')

vif(lm(data=new_df_merged, all_features_count))
#vif_func(in_frame=rand.vars,thresh=5,trace=T)
### log wages??? ####
# non parametrics ?? #
## feature collinearity. . get count betwn 4 and 800 rather than <400 and <800 (there's an overlap here)
new_df = na.omit(new_df)

#regression
model = lm(data=new_df, formula = "is_hotspot_25.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
           subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
           drug_centers_800ft + vacant_land_400ft + vacant_land_800ft")
summary(model)

model2 = lm(data=new_df, formula = "is_hotspot_50.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
            subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
            drug_centers_800ft + vacant_land_400ft + vacant_land_800ft")
summary(model2)


logit_model = glm(data=new_df, formula = "is_hotspot_25.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
                  subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
                  drug_centers_800ft + vacant_land_400ft + vacant_land_800ft")
summary(logit_model)

logit_model2 = glm(data=new_df, formula = "is_hotspot_50.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
                   subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
                   drug_centers_800ft + vacant_land_400ft + vacant_land_800ft")
summary(logit_model2)


summary(negative_binom_regression <- glm.nb(data=new_df, formula = "is_hotspot_25.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
                                            subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
                                            drug_centers_800ft + vacant_land_400ft + vacant_land_800ft"))

