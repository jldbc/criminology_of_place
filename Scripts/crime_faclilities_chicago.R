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



##################################################################################
# CHANGES TO DATA:
# DO NOT LIMIT TO 2015 AT FIRST
# WIDEN FIRST, THEN LIMIT TO 2015 SO THAT WE KEEP THE NEGATIVE SEGMENTS 
##################################################################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
df = fread("chicago_2001_present.csv", data.table=FALSE)

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[df$Longitude > -90,] #there are a few impossible values in this data. Need to drop these. 

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))

#filter out nonviolent crime
violent_crimes = c("BATTERY", "ASSAULT", "ROBBERY", "CRIM SEXUAL ASSAULT", "HOMICIDE",
                   "DOMESTIC VIOLENCE")

df = df[df$PrimaryType %in% violent_crimes,]

#only look at one year.. hotspots vary too much YoY to see all.
df = df[df$Year==2015,]
frequencies = sort(table(df['segment_id']), decreasing=T)

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
df$is_hotspot_25 = 0
df$is_hotspot_50 = 0
n_seg = find_concentration(0.25, df)
hotspot_ids = names(frequencies[1:n_seg])
df[df$segment_id %in% hotspot_ids, c("is_hotspot_25")] = 1 
n_seg = find_concentration(0.5, df)
hotspot_ids = names(frequencies[1:n_seg])
df[df$segment_id %in% hotspot_ids, c("is_hotspot_50")] = 1 
df$count = 1
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
counts = cbind(aggregate(count~segment_id, sum, data=df), table(df$segment_id))

#convert from long to wide form
new_df = reshape(data=df, idvar="segment_id", timevar="Year", direction = "wide")
new_df$count = counts$count

#replace NAs with zeros (these are years in which the segments saw no crime)
new_df[is.na(new_df)] <- 0

##### load in drug treatment center data ######
drug_centers = fread('drug_centers.csv', data.table=FALSE)
drug_centers$loc = gsub(".*\n","",drug_centers$`Physical Address`)
drug_centers <- drug_centers %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
drug_centers$Latitude = as.numeric(drug_centers$Latitude)
drug_centers$Longitude = as.numeric(drug_centers$Longitude)

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], drug_centers[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$drug_centers_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$drug_centers_800ft = out

##### load in public school data #####
schools = read.csv("public_schools.csv")
schools = schools[!is.na(schools$School_Latitude),]
schools = schools[!is.na(schools$School_Longitude),]

#calculate distance matrix between schools and crime locations (in meters, calc. by haversine distance)
dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], schools[,c("School_Latitude", "School_Longitude")])
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

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], trains[,c("Latitude", "Longitude")])
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

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], bars[,c("LATITUDE", "LONGITUDE")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$bars_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$bars_800ft = out


#### proximity to city center ####
dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], c(41.8781, -87.6298))
out = apply(dists, 1, function(x) sum(x<3218.69)) #within 2 mi of downtown
new_df$dist_to_city_center = dists
new_df$close_to_downtown = out


#### vacant property ####
vac_land = read.csv("vacant_property.csv")
vac_land$loc = gsub(".*\n","",vac_land$Location)
vac_land = vac_land[vac_land$loc != "",]
vac_land = vac_land[!duplicated(vac_land$Location),]
vac_land = vac_land %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
vac_land$Latitude = as.numeric(vac_land$Latitude)
vac_land$Longitude = as.numeric(vac_land$Longitude)

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], vac_land[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$vacant_land_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$vacant_land_800ft = out


###########  BUS STOPS  ###############

bus_stops = read.csv("bus_routes_chicago.csv")
bus_stops <- bus_stops %>% extract(location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
bus_stops$Latitude = as.numeric(bus_stops$Latitude)
bus_stops$Longitude = as.numeric(bus_stops$Longitude)

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], bus_stops[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$bus_stops_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$bus_stops_800ft = out

#bus_stops = bus_stops[!duplicated(bus_stops[,c('on_street', 'cross_street', 'routes')]),]
# wow that's a lot.. website says there are >10k though so it should be correct http://www.transitchicago.com/about/facts.aspx
# mapdf = bus_stops[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 11) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

###########  BUSINESS LICENSES  ###############










###########  GROCERY STORES  ###############










###########  SENIOR CENTERS  #############
senior_centers = read.csv('senior_centers_chicago.csv')
senior_centers$loc = gsub(".*\n","",senior_centers$`LOCATION`)
senior_centers = senior_centers %>% extract(loc, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
senior_centers$Latitude = as.numeric(senior_centers$Latitude)
senior_centers$Longitude = as.numeric(senior_centers$Longitude)
senior_centers = senior_centers[!is.na(senior_centers$Latitude),]

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], senior_centers[,c("Latitude", "Longitude")])
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

dists = distm(new_df[,c("Latitude.2015", "Longitude.2015")], parks[,c("Latitude", "Longitude")])
out = apply(dists, 1, function(x) sum(x<121)) #121 m. ~ 400 ft. ~ one segment length
new_df$parks_400ft = out
out = apply(dists, 1, function(x) sum(x<243 & x>=121))
new_df$parks_800ft = out

# mapdf = parks[,c("Latitude","Longitude")]
# mapdf <- na.omit(mapdf)
# qmap("chicago", zoom = 13) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

###########  LANGUAGES SPOKEN  ###############



















##### bring in demographic data #####
community_data = read.csv('socioeconomic_indicators.csv')

new_df_merged = merge(x = new_df, y = community_data, by.x = "CommunityArea.2015", by.y="Community.Area.Number", all.x = TRUE)


model = lm(data=new_df_merged, formula = "is_hotspot_25.2015 ~ schools_400ft + schools_800ft + subway_400ft + 
   subway_800ft + bars_400ft + bars_800ft + dist_to_city_center + close_to_downtown + drug_centers_400ft +
           drug_centers_800ft + vacant_land_400ft + vacant_land_800ft + PERCENT.OF.HOUSING.CROWDED + 
           PERCENT.HOUSEHOLDS.BELOW.POVERTY + PERCENT.AGED.16..UNEMPLOYED + 
           PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA + bus_stops_400ft + bus_stops_800ft +
senior_centers_400ft + senior_centers_800ft")
summary(model)




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

