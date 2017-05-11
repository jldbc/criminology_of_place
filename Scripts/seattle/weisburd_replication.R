# Replication of Weisburd's experiments of crime at place in Seattle
# Author: James LeDoux
# Jan. 9 2017

# note: if not using the shapefile-merged data, we do not know which segments are arterial
# / residential vs. other kinds we should be dropping. That's the main danger of ignoring it

library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(crimCV)


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

# let's see which crimes happen the most
sort(table(df$SummarizedOffenseDescription), decreasing=T)  #if tables not geojoined

#see the names of the worst street segments (mostly curious if I'm still picking up traffic offenses on highways)
sort(table(df$HundredBlockLocation), decreasing=T)

#let's remove car prowl as a category for now, since I can't drop highways without finding their names or joining the shapefile
df = df[df$SummarizedOffenseDescription != 'CAR PROWL',]

#for(crimetype in c('CAR PROWL', 'BURGLARY', 'OTHER PROPERTY', 'PROPERTY DAMAGE', 
#                   'VEHICLE THEFT', 'FRAUD', 'ASSAULT', 'SHOPLIFTING', 'THREATS',
#                   'DISTURBANCE', 'STOLEN PROPERTY', 'NARCOTICS', 'ROBBERY',
#                   'PROSTITUTION', 'WEAPON', 'PICKPOCKET')){
#  df = backup_df[backup_df$SummarizedOffenseDescription == crimetype,]
  # incidents observed by street segment ID
  #frequencies = sort(table(df['id']), decreasing=T) 
  frequencies = sort(table(df['segment_id']), decreasing=T)  #if tables not geojoined
  
  # get number of street segments with crime on them (13440)
  num_segments_with_crime = nrow(as.data.frame(frequencies))
  
  #find num segments accounting for x% of total crime
  find_concentration = function(percent_of_all_crime, df){
    totalcrime = nrow(df)
    crime_count_to_explain = totalcrime*percent_of_all_crime
    #frequencies = sort(table(df['id']), decreasing=T)
    frequencies = sort(table(df['segment_id']), decreasing=T)  #if not joined
    sum_ = 0
    n = 1
    while(sum_ < crime_count_to_explain){
      #print(n)
      sum_ = sum(frequencies[1:n])
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
  years_in_data = as.numeric(names(table(df$Year)))  #unique(df$Year)
  crimes_per_year = table(df['Year'])#, decreasing=T)
  #note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
  #plot(crimes_per_year)  #use ggplot later to make this look nice
  
  concentration_time_series = data.frame(years_in_data)
  concentration_time_series$all = 999
  concentration_time_series$fifty = 999
  concentration_time_series$twentyfive = 999
  concentration_time_series$total_crime = -1
  #df = df[complete.cases(df),] #drop incomplete examples
  for(year in years_in_data){
    if(year>2007){  #pre-2009 data looks sketchy in this
      fifty = find_concentration(0.5, df[df['Year']==year,])
      twentyfive = find_concentration(0.25, df[df['Year']==year,])
      all = find_concentration(.99, df[df['Year']==year,])
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

n = find_concentration(.25, df[df['Year']==2008,])
segment_ids_2010 = as.numeric(names(frequencies)[1:n])

vals = c(1)

for(j in 2008:2016){
  n_ = find_concentration(.25, df[df['Year']==j,])
  segment_ids = as.numeric(names(frequencies)[1:n_])
  n_matches = length(intersect(segment_ids_2010, segment_ids))
  pct_still_hot = n_matches/n
  print(pct_still_hot)
  vals = c(vals, pct_still_hot)
}

plot(vals)


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
qmap("seattle", zoom = 13) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)



###### trajectory clustering (zero inflated poisson)









#########################################
# CHICAGO
#########################################
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")


#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("chicago_2001_present.csv", data.table=FALSE)

#####  dropping non arterial / residential segments (see shapefile for CHI versions of these) #####
#df = df[which(df$SND_FEACOD %in% list(1,5)),]
#df = df[which(df$SEGMENT_TY %in% list(1,5)),]
#df = df[which(df$CITYCODE==1),]
# also: how do I find intersections in this data? they aren't in the block description apparently
#################################################

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))

#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

###### get chicago equivalent of this. weed out non arterial and residential streets #####
# smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]

#convert to standard coordinate dataframe
transportation.table <- fortify(transportation.shapefile)
num_segments = length(unique(transportation.table$id))

# incidents observed by street segment ID
#frequencies = sort(table(df['id']), decreasing=T) 
frequencies = sort(table(df['segment_id']), decreasing=T)

# get number of street segments with crime on them (13440)
num_segments_with_crime = nrow(as.data.frame(frequencies))

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

# values from figures 3 and 4
# note: segment_id and id return the same value.. maybe we don't need to do the join?
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments


#### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
years_in_data = unique(df$Year)
crimes_per_year = table(df['Year'])#, decreasing=T)
#note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
plot(crimes_per_year)  #use ggplot later to make this look nice

concentration_time_series = data.frame(years_in_data)
concentration_time_series$all = 999
concentration_time_series$fifty = 999
concentration_time_series$twentyfive = 999
concentration_time_series$total_crime = -1

df = df[complete.cases(df),] #drop incomplete examples
for(year in years_in_data){
  if(year>2000){  #pre-2009 data looks sketchy in this
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
concentration_time_series$all = concentration_time_series$all / num_segments
concentration_time_series$fifty = concentration_time_series$fifty / num_segments
concentration_time_series$twentyfive = concentration_time_series$twentyfive / num_segments
concentration_time_series = concentration_time_series[order(years_in_data),]

plot(x=concentration_time_series$years_in_data, y=concentration_time_series$fifty)
plot(x=concentration_time_series$years_in_data, y=concentration_time_series$twentyfive)



par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty, type="l", 
                                     col="red3", xlim=c(2003,2016), ylim=c(0,.3), ylab="Concentration"))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive
                                     , pch=16, axes=F, xlab=NA, ylab=NA, ylim=c(0,.3), 
                                     col='blue', type='l', xlim=c(2003,2016)))
#par(new = T)
#with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                     , pch=16, axes=F, xlab=NA, ylab=NA, ylim=c(0,.3), 
#                                     col='blue', type='l', xlim=c(2003,2016)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(2003,2016)))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of Incidents Reported')


legend("topleft",
       legend=c("Total Crime", "50% concentration", "25% concentration"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "red3", "blue"), cex=0.3)




# how long do hotspots stay hot?

n = find_concentration(.25, df[df['Year']==2006,])
segment_ids_2001 = as.numeric(names(frequencies)[1:n])

vals = c(1)

for(j in 2006:2016){
  n_ = find_concentration(.25, df[df['Year']==j,])
  segment_ids = as.numeric(names(frequencies)[1:n_])
  n_matches = length(intersect(segment_ids_2001, segment_ids))
  pct_still_hot = n_matches/n
  print(n_matches)
  vals = c(vals, pct_still_hot)
}

plot(vals)




#####################################################################
# San Francisco
#####################################################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/SF")


#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("sf.csv", data.table=FALSE)

#####  dropping non arterial / residential segments (see shapefile for CHI versions of these) #####
#df = df[which(df$SND_FEACOD %in% list(1,5)),]
#df = df[which(df$SEGMENT_TY %in% list(1,5)),]
#df = df[which(df$CITYCODE==1),]
#################################################

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[!grepl('/', df$Address),] #get rid of intersections

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Address)))

#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="SanFranciscoBasemapStreetCenterlines.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

###### get chicago equivalent of this. weed out non arterial and residential streets #####
# smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]

#convert to standard coordinate dataframe
transportation.table <- fortify(transportation.shapefile)
num_segments = length(unique(transportation.table$id))

# incidents observed by street segment ID
#frequencies = sort(table(df['id']), decreasing=T) 
frequencies = sort(table(df['segment_id']), decreasing=T)

# get number of street segments with crime on them (13440)
num_segments_with_crime = nrow(as.data.frame(frequencies))

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

# values from figures 3 and 4
# note: segment_id and id return the same value.. maybe we don't need to do the join?
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments


#### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
years_in_data = unique(df$Year)
crimes_per_year = table(df['Year'])#, decreasing=T)
#note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
plot(crimes_per_year)  #use ggplot later to make this look nice

concentration_time_series = data.frame(years_in_data)
concentration_time_series$all = 999
concentration_time_series$fifty = 999
concentration_time_series$twentyfive = 999
concentration_time_series$total_crime = -1

df = df[complete.cases(df),] #drop incomplete examples
for(year in years_in_data){
  if(year>2000){  #pre-2009 data looks sketchy in this
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
concentration_time_series$all = concentration_time_series$all / num_segments
concentration_time_series$fifty = concentration_time_series$fifty / num_segments
concentration_time_series$twentyfive = concentration_time_series$twentyfive / num_segments
concentration_time_series = concentration_time_series[order(years_in_data),]

plot(x=concentration_time_series$years_in_data, y=concentration_time_series$fifty)
plot(x=concentration_time_series$years_in_data, y=concentration_time_series$twentyfive)



par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty, type="l", 
                                     col="red3", xlim=c(2003,2016), ylim=c(0,.3), ylab="Concentration"))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive
                                     , pch=16, axes=F, xlab=NA, ylab=NA, ylim=c(0,.3), 
                                     col='blue', type='l', xlim=c(2003,2016)))
#par(new = T)
#with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                     , pch=16, axes=F, xlab=NA, ylab=NA, ylim=c(0,.3), 
#                                     col='blue', type='l', xlim=c(2003,2016)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(2003,2016)))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of Incidents Reported')


legend("topleft",
       legend=c("Total Crime", "50% concentration", "25% concentration"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "red3", "blue"), cex=0.3)



##########
# EXTENSIONS 
##########

#GET HOTSPOTS IN Y0, SEE HOW MANY REMAIN HOT IN YEARS 1 THROUGH N






par(mar = rep(2, 4)) #need this or else the figure is too large to plot
ggplot() + geom_path(data=transportation.table, aes(x=long, y=lat, group=group), size=0.2)
