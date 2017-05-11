#####################################################################
# San Francisco
#####################################################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/SF")

library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)

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
df = df[df$Address != '800 Block of BRYANT ST',]   #this is the police station. sfpd puts this address when real address not available
#df = df[df$Category != 'RUNAWAY',]
#df = df[df$Category != 'RECOVERED VEHICLE',]
#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="SanFranciscoBasemapStreetCenterlines.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
transportation.shapefile = transportation.shapefile[transportation.shapefile@data$layer == 'STREETS',]
###### get chicago equivalent of this. weed out non arterial and residential streets #####
# smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
# smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]

#convert to standard coordinate dataframe
transportation.table <- fortify(transportation.shapefile)
#num_segments = length(unique(transportation.table$id))
num_segments = nrow(transportation.shapefile@data)
cat("num segments: ", num_segments)

violent_crimes = c("ASSAULT", "ROBBERY", "SEX OFFENSES, FORCIBLE")

df = df[df$Category %in% violent_crimes,]

# incidents observed by street segment ID
#frequencies = sort(table(df['id']), decreasing=T) 
frequencies = sort(table(df['segment_id']), decreasing=T)
plot(frequencies)



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

# see top types of crime in the data (uncomment below)
#sort(table(df$Category), decreasing=T)

# values from figures 3 and 4
# note: segment_id and id return the same value.. maybe we don't need to do the join?
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments


#### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
df$Year = as.numeric(substr(df$Date, nchar(df$Date)-3, nchar(df$Date)))
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


write.csv(concentration_time_series, '../Concentration_Levels/violent_concentration_sf.csv')

# params for crime concentration plot
start_yr = 2007
end_yr = 2016
y_min = 0
y_max = 20

print(concentration_time_series[concentration_time_series$years_in_data > (start_yr-1),])

par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
                                     ylab="Concentration", xlab='Year'))
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


legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)


# How long do hotspots stay hot?
y_0 = 2004 #starting year
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
vals$city = 'San Francisco'
vals$year = y_0:2016

write.csv(vals, '../Dropoff_Rates/violent_dropoff_sf.csv')
###### plot the points existing on segments accounting for 50%, 25% of total crime ########
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

mapdf = temp_df[,c("X","Y")]
mapdf <- na.omit(mapdf)
#ggmap(seattle) 
qmap("san francisco", zoom = 13) + geom_point(data=mapdf, aes(x=X, y=Y), color="red", size=1, alpha=1)



# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/SF")
# 
# library(rgdal)
# library(data.table)
# library(dplyr)
# library(ggplot2)
# 
# #cleaned and merged DataFrame
# #all incidents reported in seattle xxxx to present (2011 for now, it appears)
# #note: this is incomplete data until I find a way to fix the failed merge from earlier
# df = fread("sf.csv", data.table=FALSE)
# 
# #####  dropping non arterial / residential segments (see shapefile for CHI versions of these) #####
# #df = df[which(df$SND_FEACOD %in% list(1,5)),]
# #df = df[which(df$SEGMENT_TY %in% list(1,5)),]
# #df = df[which(df$CITYCODE==1),]
# #################################################
# 
# #drop spaces in column names
# names(df) <- gsub(x = names(df),
#                   pattern = " ",
#                   replacement = "")
# 
# before_crimes = nrow(df)
# df = df[!grepl('/', df$Address),] #get rid of intersections
# cat("pct of crime at intersections: ", ((before_crimes - nrow(df))/before_crimes))
# 
# #unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
# df <- transform(df,segment_id=as.numeric(factor(Address)))
# 
# #get total number of street segmets in the city
# transportation.shapefile = readOGR(dsn="SanFranciscoBasemapStreetCenterlines.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
# 
# #filter out non street segments
# drops = c('FREEWAYS', 'Paper_fwys')
# transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$layer %in% drops,]
# 
# #convert to standard coordinate dataframe
# #transportation.table <- fortify(transportation.shapefile)
# num_segments = length(unique(transportation.shapefile@data$cnn))
# cat("num segments: ", num_segments)
# 
# # get number of street segments with crime on them (13440)
# num_segments_with_crime = nrow(as.data.frame(frequencies))
# 
# 
# #filter out nonviolent crime
# #sort(table(df$Descript), decreasing=T)
# violent_crimes = c("BATTERY", "DOMESTIC VIOLENCE", "AGGRAVATED ASSAULT WITH A DEADLY WEAPON", 
#                    "AGGRAVATED ASSAULT WITH BODILY FORCE", "BATTERY, FORMER SPOUSE OR DATING RELATIONSHIP",
#                    "ROBBERY, BODILY FORCE", "CHILD ABUSE (PHYSICAL)", "AGGRAVATED ASSAULT WITH A KNIFE", 
#                    "BATTERY OF A POLICE OFFICER", "ROBBERY ON THE STREET WITH A GUN", "SEXUAL BATTERY", 
#                    "AGGRAVATED ASSAULT WITH A GUN", "FORCIBLE RAPE, BODILY FORCE", "ROBBERY, ARMED WITH A GUN", 
#                    "ATTEMPTED ROBBERY ON THE STREET WITH BODILY FORCE", "CHILD ABUSE SEXUAL", "ROBBERY OF A COMMERCIAL ESTABLISHMENT, STRONGARM",
#                    "ATTEMPTED ROBBERY WITH BODILY FORCE", "ASSAULT", "DISTURBING THE PEACE, FIGHTING", "ROBBERY OF A COMMERCIAL ESTABLISHMENT WITH A GUN",
#                    "ROBBERY OF A CHAIN STORE WITH BODILY FORCE", "ROBBERY ON THE STREET WITH A DANGEROUS WEAPON", "ASSAULT WITH CAUSTIC CHEMICALS",
#                    "ATTEMPTED SIMPLE ASSAULT", "ROBBERY, ARMED WITH A KNIFE", "ROBBERY ON THE STREET WITH A KNIFE", "ROBBERY, ARMED WITH A DANGEROUS WEAPON", 
#                    "ASSAULT TO RAPE WITH BODILY FORCE", "ROBBERY OF A RESIDENCE WITH BODILY FORCE", "SHOOTING INTO INHABITED DWELLING OR OCCUPIED VEHICLE", 
#                    "ATTEMPTED HOMICIDE WITH A GUN", "ATTEMPTED ROBBERY ON THE STREET WITH A GUN", "ASSAULT, AGGRAVATED, W/ GUN", "ASSAULT ON A POLICE OFFICER WITH A DEADLY WEAPON",
#                    "CHILD ABUSE, PORNOGRAPHY", "ATTEMPTED HOMICIDE WITH A KNIFE", "CHILD, INFLICTING INJURY RESULTING IN TRAUMATIC CONDITION",
#                    "ROBBERY OF A RESIDENCE WITH A GUN", "UNLAWFUL SEXUAL INTERCOURSE", "BURGLARY OF HOTEL ROOM, FORCIBLE ENTRY", "AGGRAVATED ASSAULT OF POLICE OFFICER,BODILY FORCE",
#                    "ATTEMPTED RAPE, BODILY FORCE", "SODOMY (ADULT VICTIM)", "ATTEMPTED ROBBERY WITH A GUN", "ROBBERY OF A CHAIN STORE WITH A GUN",
#                    "ROBBERY OF A BANK WITH A GUN", "ROBBERY OF A BANK WITH BODILY FORCE", "ROBBERY OF A SERVICE STATION WITH A GUN", "ATTEMPTED ROBBERY ON THE STREET W/DEADLY WEAPON",
#                    "SEXUAL ASSAULT, AGGRAVATED, OF CHILD", "SODOMY", "ATTEMPTED ROBBERY ON THE STREET WITH A KNIFE", "ROBBERY OF A BANK WITH A DANGEROUS WEAPON", 
#                    "ROBBERY OF A CHAIN STORE WITH A DANGEROUS WEAPON", "ATTEMPTED ROBBERY COMM. ESTAB. WITH BODILY FORCE", "ATTEMPTED ROBBERY WITH A KNIFE",
#                    "ATTEMPTED HOMICIDE WITH A DANGEROUS WEAPON", "ATTEMPTED ROBBERY WITH A DEADLY WEAPON", "ATTEMPTED ROBBERY COMM. ESTABLISHMENT WITH A GUN",
#                    "ATTEMPTED ROBBERY CHAIN STORE WITH BODILY FORCE", "ATTEMPTED HOMICIDE WITH BODILY FORCE") #stopped looking at encodings w/ <100 observations
# 
# violent_crimes = c("ASSAULT", "ROBBERY", "SEX OFFENSES, FORCIBLE")
# 
# df = df[df$Category %in% violent_crimes,]
# 
# # incidents observed by street segment ID
# #frequencies = sort(table(df['id']), decreasing=T) 
# frequencies = sort(table(df['segment_id']), decreasing=T)
# plot(frequencies)
# 
# 
# #find num segments accounting for x% of total crime
# find_concentration = function(percent_of_all_crime, df){
#   totalcrime = nrow(df)
#   crime_count_to_explain = totalcrime*percent_of_all_crime
#   #frequencies = sort(table(df['id']), decreasing=T)
#   frequencies = sort(table(df['segment_id']), decreasing=T)
#   sum = 0
#   n = 1
#   while(sum < crime_count_to_explain){
#     #print(n)
#     sum = sum(frequencies[1:n])
#     n = n+1
#   }
#   print("number of segments needed: ")
#   print(n)
#   return(n)
# }
# 
# # see top types of crime in the data (uncomment below)
# #sort(table(df$Category), decreasing=T)
# 
# # values from figures 3 and 4
# # note: segment_id and id return the same value.. maybe we don't need to do the join?
# n_seg = find_concentration(0.5, df)
# pct_concentration = n_seg / num_segments
# cat("Avg. pct. of segments to explain 50% of crime: ", pct_concentration*100, "%")
# 
# n_seg = find_concentration(0.25, df)
# pct_concentration = n_seg / num_segments
# cat("Avg. pct. of segments to explain 25% of crime: ", pct_concentration*100, "%")
# 
# 
# #### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
# df$Year = as.numeric(substr(df$Date, nchar(df$Date)-3, nchar(df$Date)))
# df = df[df$Year != "NA",]
# df = df[df$Year != "NA",]
# years_in_data = unique(df$Year)
# crimes_per_year = table(df['Year'])#, decreasing=T)
# #note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
# #plot(crimes_per_year)  #use ggplot later to make this look nice
# 
# concentration_time_series = data.frame(years_in_data)
# concentration_time_series$all = 999
# concentration_time_series$fifty = 999
# concentration_time_series$twentyfive = 999
# concentration_time_series$total_crime = -1
# 
# df = df[complete.cases(df),] #drop incomplete examples
# for(year in years_in_data){
#   if(year>2000){  #pre-2009 data looks sketchy in this
#     fifty = find_concentration(0.5, df[df['Year']==year,])
#     twentyfive = find_concentration(0.25, df[df['Year']==year,])
#     all = find_concentration(1, df[df['Year']==year,])
#     concentration_time_series[concentration_time_series$years_in_data==year,]$all = all
#     concentration_time_series[concentration_time_series$years_in_data==year,]$fifty = fifty
#     concentration_time_series[concentration_time_series$years_in_data==year,]$twentyfive = twentyfive
#     concentration_time_series[concentration_time_series$years_in_data==year,]$total_crime = nrow(df[df['Year']==year,])
#   }
# }
# 
# #convert raw counts to percentages
# # TODO: put these three serieses on a single, two-y-axis plot (total crime vs. 50 and 25% lines)
# concentration_time_series$all = concentration_time_series$all / num_segments
# concentration_time_series$fifty = concentration_time_series$fifty / num_segments
# concentration_time_series$twentyfive = concentration_time_series$twentyfive / num_segments
# concentration_time_series = concentration_time_series[order(-concentration_time_series$years_in_data),]
# 
# # params for crime concentration plot
# start_yr = 2007
# end_yr = 2016
# y_min = 0
# y_max = 0.5
# 
# print(concentration_time_series[concentration_time_series$years_in_data > (start_yr-1),])
# 
# par(mar = c(5,5,2,5))
# with(concentration_time_series, plot(concentration_time_series$years_in_data, 
#                                      concentration_time_series$fifty, type="l", 
#                                      col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
#                                      ylab="Concentration", xlab='Year'))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive
#                                      , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA, 
#                                      col='blue', type='l', xlim=c(start_yr,end_yr)))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                      , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA,
#                                      col='orange', type='l', xlim=c(start_yr,end_yr)))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
#                                      , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(start_yr,end_yr)))
# axis(side = 4)
# mtext(side = 4, line = 3, 'Incidents Reported')
# 
# 
# legend("topleft",
#        legend=c("Total Crime", "100%", "50%", "25%"),
#        lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)
# 
# 
# # How long do hotspots stay hot?
# y_0 = 2003 #starting year
# pct_concentration = 0.50
# n = find_concentration(pct_concentration, df[df['Year']==y_0,])
# dffreq = df[df$Year==y_0,]
# frequencies = sort(table(dffreq['segment_id']), decreasing=T)
# segment_ids_y0 = as.numeric(names(frequencies)[1:n])
# 
# vals = c()
# 
# for(j in y_0:2016){
#   n_ = find_concentration(pct_concentration, df[df['Year']==j,])
#   dffreq = df[df$Year==j,]
#   frequencies = sort(table(dffreq['segment_id']), decreasing=T)
#   segment_ids = as.numeric(names(frequencies)[1:n_])
#   n_matches = length(intersect(segment_ids_y0, segment_ids))
#   pct_still_hot = n_matches/n
#   print(pct_still_hot)
#   vals = c(vals, pct_still_hot)
# }
# 
# plot(vals)
# 
# 
# ###### plot the points existing on segments accounting for 50%, 25% of total crime ########
# yr = 2014
# temp_df = df[df$Year==yr,]
# n_seg_50pct = find_concentration(.5, temp_df)
# n_seg_25pct = find_concentration(.25, temp_df)
# frequencies = sort(table(temp_df['segment_id']), decreasing=T)
# hotspot_segment_ids_50pct = names(frequencies)[1:n_seg_50pct]
# hotspot_segment_ids_25pct = names(frequencies)[1:n_seg_25pct]
# 
# temp_df = temp_df[temp_df$segment_id %in% hotspot_segment_ids_25pct,]
# library(ggmap)
# #ggmap(seattle)
# 
# mapdf = temp_df[,c("X","Y")]
# mapdf <- na.omit(mapdf)
# #ggmap(seattle) 
# qmap("san francisco", zoom = 13) + geom_point(data=mapdf, aes(x=X, y=Y), color="red", size=1, alpha=1)
# 
