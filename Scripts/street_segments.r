library(httr)
library(data.table)

setwd("/Users/jamesledoux/Documents/Thesis")
df <- fread("data/chicago_2001_present.csv", data.table = FALSE)
names(df) <- sub(" ", "", names(df))
names(df) <- sub(" ", "", names(df))
names(df) <- sub(" ", "", names(df))


#a small subset to be sure this is working
#df$Date <- strptime(df$Date, "%m/%d/%Y %r")    #%H:%M:%S %Y")
#df$Date<- as.POSIXct(df$Date,format="%m/%d/%Y %r")    #)
#df <- subset(df, Date> "2016-9-21 15:00:00 EDT")  #cut to <1m observations for faster testing


#get credentials
token = POST('https://www.arcgis.com/sharing/rest/oauth2/token/', body=list(f= 'json', client_id = 'gDxBYZbFpUTblc5U',
  client_secret = 'f20a68b266d844078f88c78564bb6c34',
  grant_type = 'client_credentials',
  expiration = '1440')
)

token = content(token)

#reverse geocode the data (lat and long => street address)
#what do do with NAs?
get_address = function(locstring){
  address  = POST('http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?', body=list(location= locstring, f='json'))
  return(content(address))
}

df$Locstring = paste(df$Longitude,df$Latitude,sep=",")   #lon + "," + lat
addresses = apply(df['Locstring'], 1, get_address)

df$Address = addresses





#

