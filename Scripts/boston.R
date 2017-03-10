library(plyr)
library(data.table)
library(lattice)
library(ggplot2)
library(ggmap)
library(lubridate)

setwd("/Users/jamesledoux/Documents/Thesis")
df <- fread("data/boston_15_present.csv", data.table = FALSE)
names(df) <- sub(" ", "", names(df))  #might have to revisit this. needs repeating for some reason.
names(df) <- sub(" ", "", names(df))  #might have to revisit this. needs repeating for some reason.
names(df) <- sub(" ", "", names(df))  #might have to revisit this. needs repeating for some reason.

#  Make sure the character strings represent date and times and then round to days
df$Date <- strptime(df$OCCURREDONDATE, "%m/%d/%Y %r")    #%H:%M:%S %Y")
df$Date<- as.POSIXct(df$Date,format="%m/%d/%Y %r")    #)
df$Day <- as.character( round(df$Date , "days" ) )  # a rounded date
df$Month <- floor_date(df$Date, "month")
df$Year <- floor_date(df$Date, "year")
df$Month_num <- month(as.POSIXlt(df$Month, format="%m/%d/%Y %r"))

#df <- subset(df, Date >= "2016-5-25")  #cut to <1m observations for faster testing

#show most frequent crimes in the data set
histdf <- as.data.frame(table(df['OFFENSECODE GROUP']))
histdf <- histdf[order(histdf$Freq, decreasing = TRUE),]
ggplot(data=histdf, aes(reorder(Var1, -Freq), Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))

#Breakdown by ward
ward <- as.data.frame(table(df['DISTRICT']))
ward <- ward[order(ward$Freq, decreasing = TRUE),]
ggplot(data=ward, aes(reorder(Var1, -Freq), Freq)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))

crime_by_ward <- function(data=df, crime="HOMICIDE"){
  data <- subset(data, OFFENSECODEGROUP == crime)
  ward <- as.data.frame(table(data['DISTRICT']))
  ward <- ward[order(ward$Freq, decreasing = TRUE),]
  ggplot(data=ward, aes(reorder(Var1, -Freq), Freq)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
}

crime_by_ward(df, "Aggravated Assault")

#see the trend for any given crime type by month or year '01 to present
heatmap_var<- function(data=df, crime="HOMICIDE", interval="Month"){
  df1<- subset(data, OFFENSECODEGROUP == crime)
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
plot_var(crime="Aggravated Assault", interval="Month")


#heat map of crime x in chicago
boston <- get_map(location = 'boston', zoom = 13)
ggmap(boston) #see if it worked
see_heatmap <- function(crime="Larceny"){
  dfmap <- subset(df, OFFENSECODEGROUP == crime)  #plot all homicides
  dfmap <- dfmap[,c("LAT","LONG","OFFENSECODEGROUP")]
  dfmap <- na.omit(dfmap)
  ggmap(boston) + geom_density2d(data = dfmap, 
                                  aes(x = LONG, y = LAT), size = 0.2) + stat_density2d(data = dfmap, 
                                                                                                 aes(x = LONG, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                 bins = 32, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0, 0.3), guide = FALSE)
}

pointmap <- function(crime="Larceny"){
  dfmap <- subset(df, OFFENSECODEGROUP == crime)  #plot all homicides
  dfmap <- dfmap[,c("LAT","LONG","OFFENSECODEGROUP")]
  #dfmap <- df[,c("LAT","LONG","OFFENSECODEGROUP")]
  dfmap <- na.omit(dfmap)
  ggmap(boston) + geom_point(data=dfmap, aes(x=LONG, y=LAT), color="red", size=2, alpha=1)
}

#see_heatmap("Aggravated Assault")
#see_heatmap("Larceny")
#see_heatmap("Residential Burglary")
#see_heatmap("Drug Violation")
see_heatmap("Prostitution")
pointmap("Prostitution")

#