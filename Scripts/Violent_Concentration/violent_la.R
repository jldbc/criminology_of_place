#########################################
# LOS ANGELES
# note: see if they've published 2016's data yet. That extra year would help. 
#########################################

# Replication and extension of Weisburd's experiments of crime at place in Seattle
# Author: James LeDoux
# Jan. 9 2017
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(crimCV)

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/LA")


#cleaned and merged DataFrame
df = fread("la_12_15.csv", data.table=FALSE)
total_num_reports = nrow(df)
df$Year = as.numeric(substr(df$DATE, nchar(df$DATE.OCC)-3, nchar(df$DATE.OCC)))
#drop NA years
df = df[df$Year != "NA",]
#drop crime at intersections 
df = df[df$Cross.Street=="",]
reports_sans_intersections = nrow(df)
pct_crime_at_intersections = 1 - (reports_sans_intersections/total_num_reports)
cat("Percent of crime at intersections in Los Angeles: ", pct_crime_at_intersections)


#df = fread("/Users/jamesledoux/Documents/seattle_merged_with_geo2.csv", data.table=FALSE)
#df = df[which(df$SND_FEACOD %in% list(1,5)),]
#df = df[which(df$SEGMENT_TY %in% list(1,5)),]
#df = df[which(df$CITYCODE==1),]
#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

######################################################################
# ATTN: CENTERLINE FILE APPEARS TO BE WRONG. FIND BETTER LA STREET CENTERLINE (CURRENT ONE INCLUDES NON LA STREETS I THINK)
# num_segments = 69000 #hardcoding for now.. took this figure from http://bss.lacity.org/State_Streets/StateOfTheStreets.htm
# also see: http://myladot.lacity.org/arcgis/rest/services/Basemap/MapServer/2  (might be able to get the num I want from their API)
######################################################################
# get total number of residential + arterial street segments in Seattle 
# note: this will be slow since it loads and fortifies the geojson file
# transportation.shapefile = readOGR(dsn="la_streets.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
# smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$ST_CODE %in% list(0,1,2,6,7,8)),]

csv_centerline = read.csv("Street_Centerline.csv")
#table(csv_centerline$STREET_DES)
drops = c("Divided Major Highway - Class II", "Major Highway - Class I", "Major Highway - Class II",
          "Major Highway Class II", "Modified Major Highway", "Modified Secondary Highway",
          "Scenic Divided Major Highway - Class II", "Scenic Divided Secondary Highway",
          "Scenic Major Highway - Class I", "Scenic Major Highway - Class II", "Scenic Secondary Highway",
          "Secondary Highway", "Und. or Prop. Divided Mjr Hwy - Class II", "nd. or Prop. Major Hwy - Class II",
          "Und. or Prop. Scenic Mjr Hwy - Class II", "Und. or Prop. Scenic Secondary Hwy", "Und. or Prop. Secondary Hwy")
csv_centerline = csv_centerline[!csv_centerline$STREET_DES %in% drops,]
num_segments = length(unique(csv_centerline$ASSETID))
cat("Number of street segments in Los Angeles: ", num_segments)
avg_segment_length = mean(csv_centerline$SHAPE_Leng)
cat("mean street segment length is ", avg_segment_length, " feet")

#convert to standard coordinate dataframe
#transportation.table <- fortify(smaller_shapefile)

backup_df = df

# let's see which crimes happen the most (uncomment line below)
#sort(table(df$CrmCd.Desc), decreasing=T)  #if tables not geojoined

#fix address names (spaces are inconsistent so we need to drop them)
df$LOCATION <- gsub(x = df$LOCATION,
                    pattern = " ",
                    replacement = "")
#see the names of the worst street segments (mostly curious if I'm still picking up traffic offenses on highways)
sort(table(df$LOCATION), decreasing=T)

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(LOCATION)))

#filter out nonviolent crimes
#sort(table(df$CrmCd.Desc), decreasing=T)
violent_crimes = c("BATTERY - SIMPLE ASSSAULT", "SPOUSAL(COHAB) ABUSE - SIMPLE ASSAULT", "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT",
                   "ROBBERY", "BURGLARY, ATTEMPTED", "CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT", "BATTERY WITH SEXUAL CONTACT", 
                   "RAPE, FORCIBLE", "ATTEMPTED ROBBERY", "BATTERY POLICE", "SPOUSAL (COHAB) ABUSE - AGGRAVATED ASSAULT",
                   "CRIMINAL HOMICIDE", "SHOTS FIRED AT INHABITED DWELLING", "SEXUAL PENTRATION WITH A FOREIGN OBJECT",
                   "BURGLARY FROM VEHICLE, ATTEMPTED", "OTHER ASSAULT", "CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT",
                   "ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER", "SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH",
                   "RAPE, ATTEMPTED", "SHOTS INHABITED DWELLING", "BATTERY ON A FIREFIGHTER", "BATTERY FIREMAN", "SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT",
                   "LYNCHING - ATTEMPTED")
df = df[df$CrmCd.Desc %in% violent_crimes,]


frequencies = sort(table(df['segment_id']), decreasing=T)  #if tables not geojoined
plot(frequencies)

# get number of street segments with crime on them (13440)
num_segments_with_crime = nrow(as.data.frame(frequencies))


#find num segments accounting for x% of total crime
find_concentration = function(percent_of_all_crime, df){
  totalcrime = nrow(df)
  crime_count_to_explain = totalcrime*percent_of_all_crime
  #frequencies = sort(table(df['id']), decreasing=T)
  frequencies = sort(table(df['segment_id']), decreasing=T)  #if not joined
  sum = 0
  n = 1
  while(sum < crime_count_to_explain){
    #print(n)
    sum = sum(frequencies[1:n])
    n = n+1
  }
  #print("number of segments needed: ")
  #print(n)
  return(n)
}

# values from figures 3 and 4
# note: segment_id and id return the same value.. maybe we don't need to do the join?
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments
cat("Avg. pct. of segments to explain 50% of crime: ", pct_concentration*100, "%")

n_seg = find_concentration(0.25, df)
pct_concentration = n_seg / num_segments
cat("Avg. pct. of segments to explain 25% of crime: ", pct_concentration*100, "%")


#### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
df = df[df$Year != "NA",]
df = df[df$Year != "NA",]
years_in_data = unique(df$Year)
crimes_per_year = table(df['Year'])#, decreasing=T)
#note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
#plot(crimes_per_year)  #use ggplot later to make this look nice

concentration_time_series = data.frame(years_in_data)
concentration_time_series$all = 999
concentration_time_series$fifty = 999
concentration_time_series$twentyfive = 999
concentration_time_series$total_crime = -1
df = df[complete.cases(df),] #drop incomplete examples
for(year in years_in_data){
  if(year>2007){  #pre-2009 data looks sketchy in this
    fifty = find_concentration(0.5, df[df['Year']==year,])
    twentyfive = find_concentration(0.25, df[df['Year']==year,])
    all = find_concentration(1, df[df['Year']==year,])
    concentration_time_series[concentration_time_series$years_in_data==year,]$all = all
    concentration_time_series[concentration_time_series$years_in_data==year,]$fifty = fifty
    concentration_time_series[concentration_time_series$years_in_data==year,]$twentyfive = twentyfive
    concentration_time_series[concentration_time_series$years_in_data==year,]$total_crime = nrow(df[df['Year']==year,])
  }
}

#convert raw counts to percentages
# TODO: put these three serieses on a single, two-y-axis plot (total crime vs. 50 and 25% lines)
concentration_time_series$all_pct = (concentration_time_series$all / num_segments)*100
concentration_time_series$fifty_pct = (concentration_time_series$fifty / num_segments)*100
concentration_time_series$twentyfive_pct = (concentration_time_series$twentyfive / num_segments)*100
concentration_time_series = concentration_time_series[order(-concentration_time_series$years_in_data),]

concentration_time_series$years_in_data = as.integer(concentration_time_series$years_in_data) #this was a double for some reason
#fix right axis. rotate the labels andshow the full number (no 'e' notation)
#cat(crimetype)
print(concentration_time_series[concentration_time_series$years_in_data > 2007,])
mean(concentration_time_series$all_pct)
mean(concentration_time_series$fifty_pct)
mean(concentration_time_series$twentyfive_pct)

# params for crime concentration plot
start_yr = 2012
end_yr = 2015
y_min = 0
y_max = 20

print(concentration_time_series[concentration_time_series$years_in_data > (start_yr-1),])

par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
                                     ylab="Concentration", xlab='Year',xaxt='n'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA, 
                                     col='blue', type='l', xlim=c(start_yr,end_yr)))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                      , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA,
#                                      col='orange', type='l', xlim=c(start_yr,end_yr)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(start_yr,end_yr)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')
axis(1, at=c(2012, 2013, 2014, 2015))


legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)


# How long do hotspots stay hot?
y_0 = 2012 #starting year
pct_concentration = 0.5
n = find_concentration(pct_concentration, df[df['Year']==y_0,])
dffreq = df[df$Year==y_0,]
frequencies = sort(table(dffreq['segment_id']), decreasing=T)
segment_ids_y0 = as.numeric(names(frequencies)[1:n])

vals = c()

for(j in y_0:2015){
  n_ = find_concentration(pct_concentration, df[df['Year']==j,])
  dffreq = df[df$Year==j,]
  frequencies = sort(table(dffreq['segment_id']), decreasing=T)
  segment_ids = as.numeric(names(frequencies)[1:n_])
  n_matches = length(intersect(segment_ids_y0, segment_ids))
  pct_still_hot = n_matches/n
  print(pct_still_hot)
  vals = c(vals, pct_still_hot)
}

plot(vals)


###### plot the points existing on segments accounting for 50%, 25% of total crime ########

#first turn location column into seperate lat and long columns
df$Lat = sub("^[^,]*", "", df$Location.1)
df$Lat = as.numeric(substr(df$Lat, 2, nchar(df$Lat)-1))

df$Long = sub(",.*", "", df$Location.1)
df$Long = as.numeric(substr(df$Long, 2, nchar(df$Lat)))

yr = 2014
temp_df = df[df$Year==yr,]
n_seg_50pct = find_concentration(.5, temp_df)
n_seg_25pct = find_concentration(.25, temp_df)
frequencies = sort(table(temp_df['segment_id']), decreasing=T)
hotspot_segment_ids_50pct = names(frequencies)[1:n_seg_50pct]
hotspot_segment_ids_25pct = names(frequencies)[1:n_seg_25pct]

temp_df = temp_df[temp_df$segment_id %in% hotspot_segment_ids_25pct,]
library(ggmap)
#ggmap(seattle)

mapdf = temp_df[,c("Lat","Long")]
mapdf <- na.omit(mapdf)
#ggmap(seattle) 
qmap("los angeles", zoom = 10) + geom_point(data=mapdf, aes(x=Lat, y=Long), color="red", size=1, alpha=1)

