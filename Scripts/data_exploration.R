library(plyr)
library(data.table)
library(lattice)
library(ggplot2)
library(ggmap)
library(lubridate)
df <- fread("data/chicago_2001_present.csv", data.table = FALSE)
names(df) <- sub(" ", "", names(df))

#  Make sure the character strings represent date and times and then round to days
df$Date <- strptime(df$Date, "%m/%d/%Y %r")    #%H:%M:%S %Y")
df$Date<- as.POSIXct(df$Date,format="%m/%d/%Y %r")    #)
df$Day <- as.character( round(df$Date , "days" ) )  # a rounded date
df$Month <- floor_date(df$Date, "month")
df$Year <- floor_date(df$Date, "year")
df$Month_num <- month(as.POSIXlt(df$Month, format="%m/%d/%Y %r"))

#df <- subset(df, Year >= 2015)  #cut to <1m observations for faster testing

#show most frequent crimes in the data set
histdf <- as.data.frame(table(df['PrimaryType']))
histdf <- histdf[order(histdf$Freq, decreasing = TRUE),]
ggplot(data=histdf, aes(reorder(Var1, -Freq), Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))

#Breakdown by ward
ward <- as.data.frame(table(df['Ward']))
ward <- ward[order(ward$Freq, decreasing = TRUE),]
ggplot(data=ward, aes(reorder(Var1, -Freq), Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))

crime_by_ward <- function(data=df, crime="HOMICIDE"){
  data <- subset(data, PrimaryType == crime)
  ward <- as.data.frame(table(data['Ward']))
  ward <- ward[order(ward$Freq, decreasing = TRUE),]
  ggplot(data=ward, aes(reorder(Var1, -Freq), Freq)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
}

crime_by_ward(df, "BURGLARY")

#see the trend for any given crime type by month or year '01 to present
plot_var<- function(data=df, crime="HOMICIDE", interval="Month"){
  df1<- subset(data, PrimaryType == crime)
  df1$total_crimes <- 1
  if(interval == "Month"){
    crimes_per_interval <- aggregate( df1[,c(interval, "total_crimes")] , by = list(df1$Month) , length )
    crimes_per_interval2 <- aggregate( df1[,c(interval, "total_crimes")] , by = list(df1$Month_num) , length )
    crimes_per_interval2$xdum <- seq(from = 1, to = nrow(crimes_per_interval2), length.out = nrow(crimes_per_interval2))
    plot1 <- ggplot(data=crimes_per_interval2, aes(xdum, total_crimes)) + geom_point()
    print(plot1)
  }
  if(interval == "Year"){
    crimes_per_interval <- aggregate( df1[,c(interval, "total_crimes")] , by = list(df1$Year) , length )
  }
  crimes_per_interval$xdum <- seq(from = 1, to = nrow(crimes_per_interval), length.out = nrow(crimes_per_interval))
  plot2 <- ggplot(data=crimes_per_interval, aes(xdum, total_crimes)) + geom_point()
  print(plot2)
}

#see options: unique(df$PrimaryType)
plot_var(crime="HOMICIDE", interval="Month")


#heat map of crime x in chicago
chicago <- get_map(location = 'chicago', zoom = 13)
ggmap(chicago) #see if it worked
see_heatmap <- function(crime="HOMICIDE"){
  dfmap <- subset(df, PrimaryType == crime)  #plot all homicides
  ggmap(chicago) + geom_density2d(data = dfmap, 
                                  aes(x = Longitude, y = Latitude), size = 0.2) + stat_density2d(data = dfmap, 
                                  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                                  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
                                  scale_alpha(range = c(0, 0.3), guide = FALSE)
}

see_heatmap("HOMICIDE")

#