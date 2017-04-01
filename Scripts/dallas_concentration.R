#########################################
# DALLAS
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis")


#cleaned and merged DataFrame
df = fread("dallas_crime.csv", data.table=FALSE)

#drop spaces in column names
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

#drop rows with no street or block
df = df[!is.na(df$StreetBlock),]
df = df[!is.na(df$StreetName),]

df$Block = with(df, paste0(StreetBlock, StreetName))
#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df <- transform(df,segment_id=as.numeric(factor(Block)))
df$Year = df$YearofIncident

df = df[df$Block != '1400LAMAR ST',] #this location is a police station. these crimes didn't actually happen here

#get total number of street segmets in the city
transportation.shapefile = readOGR(dsn="dallas_streets.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

#drops = c('DALLAS AREA HIGHWAY', 'HIGHWAY', 'RAMP', 'PRIMARY HIGHWAY')
#drops_type = c('EXPY', 'FWY', 'HWY', 'RAMP')
transportation.shapefile = transportation.shapefile@data
#transportation.shapefile$class = as.character(transportation.shapefile$class)
#transportation.shapefile = transportation.shapefile[!transportation.shapefile$class %in% drops,]
#transportation.shapefile = transportation.shapefile[!transportation.shapefile$type %in% drops_type,]


#convert to standard coordinate dataframe
num_segments = length(unique(transportation.shapefile$segment_id))
cat("num segments: ", num_segments)

cat("average segment length: ", mean(transportation.shapefile$shape_len), " ft.")

# from weisburd: 
# property (e.g., burglary and property destruction), personal (e.g., homicide, assault, and robbery), 
# disorder (e.g., graffiti and abandoned vehicles), drugs, prostitution, and traffic-related crimes 
# (e.g., drunk driving and hit and run)
# a = table(df)
# a = a[a>20] #there are a lot of sparse crime types in here
# keeps = c('BURGLARY OF BUILDING - FORCED ENTRY', 'BURGLARY OF BUILDING - NO FORCED ENTRY', 'BURGLARY OF BUILDING (ATT)', 
#           'BURGLARY OF HABITATION - FORCED ENTRY', 'BURGLARY OF HABITATION -NO FORCED ENTRY', 'BURGLARY OF HABITATION (ATT)',
#           'BURGLARY OF HABITATION INTEND TO COMMIT AGG ASSAULT', 'BURGLARY OF HABITATION INTEND TO COMMIT ROBBERY',
#           'BURGLARY OF HABITATION INTEND TO COMMIT SIMPLE ASSAULT', 'GRAFFITI >OR EQUAL $100 BUT <$750', 
#           'GRAFFITI PECUNIARY LOSS <$500', 'MURDER')

keeps = c('BURGLARY-RESIDENCE', 'BURGLARY-BUSINESS', 'VANDALISM & CRIM MISCHIEF', 'LANDALISM & CRIM MISCHIEF', 
          'MURDER', 'MANSLAUGHTER', 'ASSAULT', 'AGG ASSAULT - NFV', 'ROBBERY-BUSINESS', 
          'ROBBERY-INDIVIDUAL', 'NARCOTICS & DRUGS', 'TRAFFIC VIOLATION', 'TRAFFIC FATALITY', 'DWI', 
          'ACCIDENT MV', 'UUMV', 'THEFT/BMV', 'DISORDERLY CONDUCT', 'CRIMINAL MISCHIEF/VANDALI', 'INJURED PUBLIC INTOXICATION') # no prostitution, no abandoned vehicles
df = df[df$UCROffenseName %in% keeps,]

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

#df = df[complete.cases(df),] #drop incomplete examples
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
concentration_time_series = concentration_time_series[concentration_time_series$years_in_data %in% c(2015, 2016),]
#fix right axis. rotate the labels andshow the full number (no 'e' notation)
#cat(crimetype)
concentration_time_series
mean(concentration_time_series$twentyfive_pct)
mean(concentration_time_series$fifty_pct)
mean(concentration_time_series$all_pct)
#fix right axis. rotate the labels andshow the full number (no 'e' notation)
#cat(crimetype)
print(concentration_time_series[concentration_time_series$years_in_data > 2007,])

write.csv(concentration_time_series, 'Data/Concentration_Levels/concentration_levels_dallas.csv')



par(mar = c(5,5,2,5))
with(concentration_time_series, plot(concentration_time_series$years_in_data, 
                                     concentration_time_series$fifty_pct, type="l", 
                                     col="red3", xlim=c(2008,2015), ylim=c(0,20), 
                                     ylab="Concentration", xlab='Year'))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                     , pch=16, axes=F, xlab=NA, ylim=c(0,20), ylab=NA, 
                                     col='blue', type='l', xlim=c(2008,2015)))
# par(new = T)
# with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
#                                      , pch=16, axes=F, xlab=NA, ylim=c(0,.5), ylab=NA,
#                                      col='orange', type='l', xlim=c(2008,2015)))
par(new = T)
with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                     , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(2008,2015)))
axis(side = 4)
mtext(side = 4, line = 3, 'Incidents Reported')
#}

legend("topleft",
       legend=c("Total Crime", "100%", "50%", "25%"),
       lty=c(1,0), pch=c(NA, 16), col=c("black", "orange", "red3", "blue"), cex=0.5)


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
vals_df = data.frame(vals)
vals_df$year = y_0:2016
write.csv(vals_df, "../Dropoff_Rates/dropoff_dallas.csv")