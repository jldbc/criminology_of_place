library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/Concentration_Levels")

#read in each year's dataframe, rbind into one single df
df1 = fread("concentration_levels_chicago.csv", data.table=FALSE)
df1$city = 'Chicago'
df2 = fread("concentration_levels_seattle.csv", data.table=FALSE)
df2$city = 'Seattle'
df3 = fread("concentration_levels_la.csv", data.table=FALSE)
df3$city = 'Los Angeles'
df4 = fread("concentration_levels_pdx.csv", data.table=FALSE)
df4$city = 'Portland'
df5 = fread("concentration_levels_sf.csv", data.table=FALSE)
df5$city = 'San Francisco'
#df6 = fread("concentration_levels_dallas.csv", data.table=FALSE)
#df6$city = 'Dallas' #only 2 obs.. maybe this one should be left out
df7 = fread("concentration_levels_dc.csv", data.table=FALSE)
df7$city = 'DC'
df8 = fread("concentration_levels_philadelphia.csv", data.table=FALSE)
df8$city = 'Philadelphia'
df9 = fread("concentration_levels_cincinnati.csv", data.table=FALSE)
df9$city = 'Cincinnati'

df4$years_in_data = df4$Yr #Portland's years were a little weird
df4$Yr = NULL

df = do.call("rbind", list(df1, df2, df3, df4, df5, df7, df8, df9))


concentration_time_series_orig = df #legacy naming scheme. I like it though. 

y_min = 0
y_max = 20
cities_all=unique(df$city)
for(j in c(0,4)){
  #print(j)
  #parentheses needed
  cities = cities_all[(1+j):(4+j)] #skip forward 4 indexes to do the other half of the sample for plot #2
  par(mar = c(5,5,2,5), mfrow=c(2,2))
  #print(cities)
  #print(cities_all)
  for(i in 1:4){
    city=cities[[i]]
    concentration_time_series = concentration_time_series_orig[concentration_time_series_orig['city']==city,]
    start_yr = min(concentration_time_series$years_in_data)
    end_yr = max(concentration_time_series$years_in_data)
    plot(concentration_time_series$years_in_data, 
                                         concentration_time_series$fifty_pct, type="l", 
                                         col="red3", xlim=c(start_yr,end_yr), ylim=c(y_min,y_max), 
                                         ylab="Concentration (%)", xlab='Year', xaxt='n', main=city)
    par(new = T)
    plot(concentration_time_series$years_in_data, concentration_time_series$twentyfive_pct
                                         , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA, 
                                         col='blue', type='l', xlim=c(start_yr,end_yr))
    # par(new = T)
    # with(concentration_time_series, plot(concentration_time_series$years_in_data, concentration_time_series$all
    #                                      , pch=16, axes=F, xlab=NA, ylim=c(y_min,y_max), ylab=NA,
    #                                      col='orange', type='l', xlim=c(start_yr,end_yr)))
    par(new = T)
    plot(concentration_time_series$years_in_data, concentration_time_series$total_crime
                                         , pch=16, axes=F, xlab=NA, ylab=NA, type='l', lty=2, xlim=c(start_yr,end_yr))
    axis(side = 4)
    mtext(side = 4, line = 3, 'Incidents Reported')
    
    axis(1, at=start_yr:end_yr)
  }
}

