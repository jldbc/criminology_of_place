#########################################
# CHICAGO
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")


#cleaned and merged DataFrame
df = fread("chicago_2001_present.csv", data.table=FALSE)

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))

#get total number of street segmets in the city
# transportation.shapefile = readOGR(dsn="Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
#filter out highways, rivers and ramps
# transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$CLASS %in% c('1', '9', 'RIV'),]
### transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$CLASS %in% c('1', 'RIV'),]
# #transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$STREET_TYP %in% c('EXPY', 'HWY', 'VIA', 'WAY'),]
###transportation.shapefile = transportation.shapefile[!transportation.shapefile@data$STREET_TYP %in% c('EXPY', 'HWY'),]
num_segments = nrow(transportation.shapefile@data)

#convert to standard coordinate dataframe
# transportation.table <- fortify(transportation.shapefile)
# num_segments = length(unique(transportation.table$id))
#num_segments = 52887 # (hardcode this in to save time)
#num_segments = 54050 # a little bit looser about what I cut out from the shapefile 
num_segments = 56320   #all of them, since we can't filter out highways accurately in the cime data
cat("num segments: ", num_segments)

#cat("average segment length: ", mean(transportation.shapefile@data$LENGTH), " ft.")
cat("average segment length: ",415.9804, " ft.")

#drop the 'other' crimes
df = df[!df$PrimaryType %in% c("OTHER OFFENSE"),]
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
n_seg = find_concentration(1, df)
pct_concentration = n_seg / num_segments
cat("Avg. pct. of segments to explain 100% of crime: ", pct_concentration*100, "%")

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
#num_segments = 58000
# TODO: put these three serieses on a single, two-y-axis plot (total crime vs. 50 and 25% lines)
concentration_time_series$all_pct = (concentration_time_series$all / num_segments) * 100
concentration_time_series$fifty_pct = (concentration_time_series$fifty / num_segments) * 100
concentration_time_series$twentyfive_pct = (concentration_time_series$twentyfive / num_segments) * 100
concentration_time_series = concentration_time_series[order(-concentration_time_series$years_in_data),]

# params for crime concentration plot
start_yr = 2002
end_yr = 2016
y_min = 0
y_max = 20

print(concentration_time_series[concentration_time_series$years_in_data > (start_yr-1),])

avg_fifty_concentration = mean(concentration_time_series[concentration_time_series$years_in_data>2001,'fifty_pct'])
avg_twentyfive_concentration = mean(concentration_time_series[concentration_time_series$years_in_data>2001,'twentyfive_pct'])
avg_all_concentration = mean(concentration_time_series[concentration_time_series$years_in_data>2001,'all_pct'])

options(scipen=5) #make scientific notation less likely
par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
                                     ylab="Concentration (%)", xlab='Year'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA, 
                                     col='blue', type='l', xlim=c(start_yr,end_yr)))
#par(new = T)
#with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all_pct
#                                     , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA,
#                                     col='orange', type='l', xlim=c(start_yr,end_yr)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(start_yr,end_yr)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')


legend("topleft",
       legend=c("Total Crime","50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black","red3", "blue"), cex=0.5)


# how long do hotspots stay hot?


# How long do hotspots stay hot?
y_0 = 2003 #starting year
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

mapdf = temp_df[,c("Latitude","Longitude")]
mapdf <- na.omit(mapdf)
#ggmap(seattle) 
qmap("chicago", zoom = 11) + geom_point(data=mapdf, aes(x=Longitude, y=Latitude), color="red", size=1, alpha=1)

