library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(crimCV)


###################################
# Philadelphia 
###################################

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Philly")


#cleaned and merged DataFrame
df = fread("philly_06_present.csv", data.table=FALSE)
#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

total_num_reports = nrow(df)
#drop crime at intersections 
df = df[!grepl('/', df$LocationBlock),] #get rid of intersections
df = df[grepl('BLOCK', df$LocationBlock),] #get rid of intersections
reports_sans_intersections = nrow(df)
pct_crime_at_intersections = 1 - (reports_sans_intersections/total_num_reports)
cat("Percent of crime at intersections: ", pct_crime_at_intersections)

df$Year = as.numeric(substr(df$DispatchDate, 1,4))

df = df[df$Year != "NA",]
df = df[!is.na(df$PoliceDistricts),]
df = df[!is.na(df$UCRCode),]

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(LocationBlock)))

# get total number of residential + arterial street segments in Seattle 
streets = fread("philly_centerline.csv", data.table=FALSE)
#drops = c(0,1,9,10,14)
#streets = streets[!streets$CLASS %in% drops,]

num_segments = length(unique(streets$SEG_ID))
cat("Number of street segments in Seattle: ", num_segments)

avg_segment_length = mean(streets$SHAPE_LEN)
cat("mean street segment length is ", avg_segment_length, " feet")

backup_df = df
#df = df[df$GeneralCrimeCategory != 'All Other Offenses',]

# let's see which crimes happen the most (uncomment line below)
#sort(table(df$SummarizedOffenseDescription), decreasing=T)  #if tables not geojoined

#see the names of the worst street segments (mostly curious if I'm still picking up traffic offenses on highways)
#sort(table(df$LocationBlock), decreasing=T)

#uncomment below to see top crime categories (lots of "others")
#sort(table(df$GeneralCrimeCategory), decreasing=T)

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
  if(year>2005){  #pre-2009 data looks sketchy in this
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


#fix right axis. rotate the labels andshow the full number (no 'e' notation)
#cat(crimetype)
print(concentration_time_series[concentration_time_series$years_in_data > 2007,])

par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(2008,2016), ylim=c(0,20), 
                                     ylab="Concentration", xlab='Year'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(0,20), ylab=NA, 
                                     col='blue', type='l', xlim=c(2008,2016)))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                      , pch=16, axes=F, xlab=NA, ylim=c(0,.5), ylab=NA,
#                                      col='orange', type='l', xlim=c(2008,2016)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(2008,2016)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')
#}

legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)



# How long do hotspots stay hot?
y_0 = 2006 #starting year
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

temp_df = df[df$Year==2014,]
n_seg_50pct = find_concentration(.5, temp_df)
n_seg_25pct = find_concentration(.25, temp_df)
hotspot_segment_ids_50pct = names(frequencies)[1:n_seg_50pct]
hotspot_segment_ids_25pct = names(frequencies)[1:n_seg_25pct]

#might have gotten the names backwards here.. had to swap them on the map
df$Long = sub("^[^ ]*", "", df$Shape)
df$Long = sub("^[^(]*", "", df$Shape)
df$Long = substr(df$Long, 2, nchar(df$Long)-1)
df$Lat = df$Long
df$Long = sub("^[^ ]*", "", df$Long)
df$Long = as.numeric(substr(df$Long, 2, nchar(df$Long)-1))

df$Lat = as.numeric(sub(" .*", "", df$Lat))

temp_df = df
temp_df = temp_df[temp_df$segment_id %in% hotspot_segment_ids_25pct,]
library(ggmap)
#ggmap(seattle)

mapdf = temp_df[,c("Lat","Long")]
mapdf <- na.omit(mapdf)
#ggmap(seattle) 
qmap("Philadelphia", zoom = 12) + geom_point(data=mapdf, aes(x=Lat, y=Long), color="red", size=1, alpha=1)
