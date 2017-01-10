# Replication of Weisburd's experiments of crime at place in Seattle
# Author: James LeDoux
# Jan. 9 2017

###################################
# TODO: get rid of intersections in this data. See if numbers match the way they should once
# that's done
###################################

library(httr)
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle")


#cleaned and merged DataFrame
#all incidents reported in seattle xxxx to present (2011 for now, it appears)
#note: this is incomplete data until I find a way to fix the failed merge from earlier
df = fread("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle/seattle_with_shapefile2.csv", data.table=FALSE)
df = df[which(df$SND_FEACOD %in% list(1,5)),]
df = df[which(df$SEGMENT_TY %in% list(1,5)),]
df = df[which(df$CITYCODE==1),]


# get total number of residential + arterial street segments in Seattle 
# 24331 for now.. actual number should be a little smaller than this according to Weisburd
# note: this will be slow since it loads and fortifies the geojson file
transportation.shapefile = readOGR(dsn="seattle_centerline.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")
smaller_shapefile = transportation.shapefile[which(transportation.shapefile$SND_FEACOD %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$SEGMENT_TY %in% list(1,5)),]
smaller_shapefile = smaller_shapefile[which(smaller_shapefile$CITYCODE==1),]
#convert to standard coordinate dataframe
transportation.table <- fortify(smaller_shapefile)
num_segments = length(unique(transportation.table$id))

# incidents observed by street segment ID
frequencies = sort(table(df['id']), decreasing=T)

# get number of street segments with crime on them (13440)
num_segments_with_crime = nrow(as.data.frame(frequencies))

#find num segments accounting for x% of total crime
find_concentration = function(percent_of_all_crime, df){
  totalcrime = nrow(df)
  crime_count_to_explain = totalcrime*percent_of_all_crime
  frequencies = sort(table(df['id']), decreasing=T)
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

#values from figures 3 and 4
n_seg = find_concentration(0.5, df)
pct_concentration = n_seg / num_segments


#### DOES THE LAW OF CRIME CONCENTRATION APPLY ACROSS TIME? ####
years_in_data = unique(df$Year)
crimes_per_year = table(df['Year'])#, decreasing=T)
#note: this plot looks funky because the data is subsetted weird here due to the failed join earlier
plot(crimes_per_year)  #use ggplot later to make this look nice

concentration_time_series = data.frame(years_in_data)
concentration_time_series$fifty = 999
concentration_time_series$twentyfive = 999
concentration_time_series$total_crime = -1

for(year in years_in_data){
  #if(year>2009){
    fifty = find_concentration(0.5, df[df['Year']==year,])
    twentyfive = find_concentration(0.25, df[df['Year']==year,])
    concentration_time_series[concentration_time_series$years_in_data==year,]$fifty = fifty
    concentration_time_series[concentration_time_series$years_in_data==year,]$twentyfive = twentyfive
    concentration_time_series[concentration_time_series$years_in_data==year,]$total_crime = nrow(df[df['Year']==year,])
  #}
}

#convert raw counts to percentages
# TODO: put these three serieses on a single, two-y-axis plot (total crime vs. 50 and 25% lines)
concentration_time_series$fifty = concentration_time_series$fifty / num_segments
concentration_time_series$twentyfive = concentration_time_series$twentyfive / num_segments
concentration_time_series = concentration_time_series[order(years_in_data),]

plot(x=concentration_time_series$years_in_data, y=concentration_time_series$fifty)
plot(x=concentration_time_series$years_in_data, y=concentration_time_series$twentyfive)



par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty, type="l", 
                                     col="red3", ylim=c(0,.2), ylab="Concentration"))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive
                                     , pch=16, axes=F, xlab=NA, ylim=c(0,.2), 
                                     col='blue', type='l'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number of Incidents Reported')
legend("topleft",
       legend=c("Total Crime", "50% concentration", "25% concentration"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "red3", "blue"), cex=0.3)
