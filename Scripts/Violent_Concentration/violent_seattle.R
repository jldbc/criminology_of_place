#########################################
# SEATTLE
#########################################

# Replication and extension of Weisburd's experiments of crime at place in Seattle
# Author: James LeDoux
# Jan. 9 2017
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)

###################################
# SEATTLE 
###################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle")


#cleaned and merged DataFrame
df = fread("seattle.csv", data.table=FALSE)
total_num_reports = nrow(df)
#drop NA years
df = df[df$Year != "NA",]
#drop crime at intersections 
df = df[!grepl('/', df$`Hundred Block Location`),] #get rid of intersections
reports_sans_intersections = nrow(df)
pct_crime_at_intersections = 1 - (reports_sans_intersections/total_num_reports)
cat("Percent of crime at intersections in Seattle: ", pct_crime_at_intersections)


#df = fread("/Users/jamesledoux/Documents/seattle_merged_with_geo2.csv", data.table=FALSE)
#df = df[which(df$SND_FEACOD %in% list(1,5)),]
#df = df[which(df$SEGMENT_TY %in% list(1,5)),]
#df = df[which(df$CITYCODE==1),]
#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(HundredBlockLocation)))

# get total number of residential + arterial street segments in Seattle 
# 24331 for now.. actual number should be a little smaller than this according to Weisburd
# note: this will be slow since it loads and fortifies the geojson file
transportation.shapefile = readOGR(dsn="seattle_centerline.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$ST_CODE %in% list(0,1,2,6,7,8)),]

#convert to standard coordinate dataframe
transportation.table <- fortify(smaller_shapefile)
num_segments = length(unique(transportation.table$id))
cat("Number of street segments in Seattle: ", num_segments)
avg_segment_length = mean(transportation.shapefile@data$GIS_SEG_LE)
cat("mean street segment length is ", avg_segment_length, " feet")


backup_df = df

# let's see which crimes happen the most (uncomment line below)
#sort(table(df$SummarizedOffenseDescription), decreasing=T)  #if tables not geojoined

violent_crimes = c("ASSAULT", "ROBBERY", "HOMICIDE")
df = df[df$SummarizedOffenseDescription %in% violent_crimes,]
#see the names of the worst street segments (mostly curious if I'm still picking up traffic offenses on highways)
sort(table(df$HundredBlockLocation), decreasing=T)

#for(crimetype in c('CAR PROWL', 'BURGLARY', 'OTHER PROPERTY', 'PROPERTY DAMAGE', 
#                   'VEHICLE THEFT', 'FRAUD', 'ASSAULT', 'SHOPLIFTING', 'THREATS',
#                   'DISTURBANCE', 'STOLEN PROPERTY', 'NARCOTICS', 'ROBBERY',
#                   'PROSTITUTION', 'WEAPON', 'PICKPOCKET')){
#  df = backup_df[backup_df$SummarizedOffenseDescription == crimetype,]
# incidents observed by street segment ID
#frequencies = sort(table(df['id']), decreasing=T) 
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
df = df[df$Year != "NA",]
df = df[df$Year != "NA",]
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
concentration_time_series$all_pct = (concentration_time_series$all / num_segments) * 100
concentration_time_series$fifty_pct = (concentration_time_series$fifty / num_segments)*100
concentration_time_series$twentyfive_pct = (concentration_time_series$twentyfive / num_segments)*100
concentration_time_series = concentration_time_series[order(-concentration_time_series$years_in_data),]

#fix right axis. rotate the labels andshow the full number (no 'e' notation)
#cat(crimetype)
print(concentration_time_series[concentration_time_series$years_in_data > 2007,])
concentration_time_series = concentration_time_series[concentration_time_series$years_in_data > 2007 & concentration_time_series$years_in_data < 2016,]
mean(concentration_time_series$twentyfive_pct)
mean(concentration_time_series$fifty_pct)
mean(concentration_time_series$all_pct)

write.csv(concentration_time_series, '../Concentration_Levels/violent_concentration_seattle.csv')

par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(2008,2015), ylim=c(0,20), 
                                     ylab="Concentration (%)", xlab='Year'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(0,20), ylab=NA, 
                                     col='blue', type='l', xlim=c(2008,2015)))
par(new = T)
#with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                     , pch=16, axes=F, xlab=NA, ylim=c(0,.5), ylab=NA,
#                                     col='orange', type='l', xlim=c(2008,2016)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(2008,2015)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')
#}

legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)



# How long do hotspots stay hot?
y_0 = 2008 #starting year
pct_concentration = 0.25
n = find_concentration(pct_concentration, df[df['Year']==y_0,])
dffreq = df[df$Year==y_0,]
frequencies = sort(table(dffreq['segment_id']), decreasing=T)
segment_ids_y0 = as.numeric(names(frequencies)[1:n])

vals = c()

for(j in y_0:2016){
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
vals = data.frame(vals)
vals$city = 'Seattle'
vals$year = y_0:2016

write.csv(vals, '../Dropoff_Rates/violent_dropoff_seattle.csv')

###### plot the points existing on segments accounting for 50%, 25% of total crime ########

temp_df = df[df$Year==2015,]
n_seg_50pct = find_concentration(.5, temp_df)
n_seg_25pct = find_concentration(.25, temp_df)
hotspot_segment_ids_50pct = names(frequencies)[1:n_seg_50pct]
hotspot_segment_ids_25pct = names(frequencies)[1:n_seg_25pct]

temp_df = temp_df[temp_df$segment_id %in% hotspot_segment_ids_25pct,]
library(ggmap)
#ggmap(seattle)

mapdf = temp_df[,c("Latitude","Longitude")]
mapdf <- na.omit(mapdf)
#ggmap(seattle) 
qmap("seattle", zoom = 12) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

