#########################################
# PORTLAND
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/PDX")


#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("pdx.csv", data.table=FALSE) #portland_police_bureau is a better dataset but only has 16 - 17

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

before_crimes = nrow(df)
df = df[!grepl(' and ', df$Address),] #get rid of intersections
cat("pct of crime at intersections: ", (before_crimes-nrow(df))/before_crimes)

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Address)))

# get rid of observations with no address (geocoded to the police precinct address)
df = df[df$segment_id !=57009,]

df$Year = as.numeric(substr(df$ReportDate, nchar(df$ReportDate)-1, nchar(df$ReportDate)))
df = df[df$Year != 11,]   #comes from a different source
df = df[df$Year != 13,]

#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="pdxshapefile.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
transportation.shapefile = transportation.shapefile@data
transportation.shapefile = transportation.shapefile[transportation.shapefile$LCITY=="Portland" & transportation.shapefile$RCITY=="Portland",]

#convert to standard coordinate dataframe
#transportation.table <- fortify(transportation.shapefile)
transportation.shapefile = transportation.shapefile[transportation.shapefile$RIGHT_JUR == 'PORT',]
transportation.shapefile = transportation.shapefile[transportation.shapefile$LEFT_JUR == 'PORT',]
# key: https://www.portlandmaps.com/metadata/index.cfm?&action=DisplayLayer&LayerID=52058
drops = c(1100, 1120, 1121, 1122, 1123, 1200, 5201)
transportation.shapefile = transportation.shapefile[!transportation.shapefile$TYPE %in% drops,]

num_segments = length(unique(transportation.shapefile$OBJECTID))
cat("num segments: ", num_segments)
avg_segment_length = mean(transportation.shapefile[!is.na(transportation.shapefile$LENGTH), 'Shape_Leng'])
cat("avg segment length: ", avg_segment_length, " ft.")

#filter out nonviolent crime
#sort(table(df$MajorOffenseType), decreasing=T)
violent_crimes = c("Assault, Simple", "Aggravated Assault", "ROBBERY", "Robbery", "ASSAULT W/DANGEROUS WEAPON", "SEX ABUSE",
                   "Homicide", "Rape")
df = df[df$MajorOffenseType %in% violent_crimes,]

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

# see top types of crime in the data (uncomment below)
#sort(table(df$PrimaryType), decreasing=T)

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
  if(year>3){  #pre-2009 data looks sketchy in this
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
print(concentration_time_series)
mean(concentration_time_series$twentyfive_pct)
mean(concentration_time_series$fifty_pct)
mean(concentration_time_series$all_pct)

# params for crime concentration plot
start_yr = 2004
end_yr = 2013
y_min = 0
y_max = 20


concentration_time_series$Yr = c(2014, 2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004)
par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$Yr, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
                                     ylab="Concentration (%)", xlab='Year'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$Yr, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA, 
                                     col='blue', type='l', xlim=c(start_yr,end_yr)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$Yr, concentration_time_series$all
                                     , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA,
                                     col='orange', type='l', xlim=c(start_yr,end_yr)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$Yr, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(start_yr,end_yr)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')


legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)


# How long do hotspots stay hot?
y_0 = 4 #starting year
pct_concentration = 0.25
n = find_concentration(pct_concentration, df[df['Year']==y_0,])
dffreq = df[df$Year==y_0,]
frequencies = sort(table(dffreq['segment_id']), decreasing=T)
segment_ids_y0 = as.numeric(names(frequencies)[1:n])

vals = c()

for(j in y_0:13){
  n_ = find_concentration(pct_concentration, df[df['Year']==j,])
  dffreq = df[df$Year==j,]
  frequencies = sort(table(dffreq['segment_id']), decreasing=T)
  segment_ids = as.numeric(names(frequencies)[1:n_])
  n_matches = length(intersect(segment_ids_y0, segment_ids))
  pct_still_hot = n_matches/n
  print(pct_still_hot)
  vals = c(vals, pct_still_hot)
}

plot(vals)  #so 2011 and 2014 are clearly endcoded different from the rest of the data. this is also why there are too many segments in the data (some are duplicates)



## can't plot this unless i figure out how to plot coordinate data 