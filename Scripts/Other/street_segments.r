library(httr)
library(data.table)

setwd("/Users/jamesledoux/Documents/Thesis")
df <- fread("data/chicago_2001_present.csv", data.table = FALSE,nrows=100) #Limit n rows while this is being tuned



names(df) <- sub(" ", "", names(df))
names(df) <- sub(" ", "", names(df))
names(df) <- sub(" ", "", names(df))


#a small subset to be sure this is working
df$Date <- strptime(df$Date, "%m/%d/%Y %r")    #%H:%M:%S %Y")
df$Date<- as.POSIXct(df$Date,format="%m/%d/%Y %r")    #)


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

get_segments = function(locstring){
  http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?
  others = singleLine=New%20York%20St%20and%20Redlands%20Blvd,%20Redlands,%20CA&outFields=*&f=pjson
  address =  POST('http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/geocodeAddresses?', body=list(addresses={"records":[{"attributes":{"OBJECTID":1,"SingleLine":"2110 Eola Dr NW, Salem, OR, 97304"}}],outFields='AddNumFrom',f='json'))
  
}

df$Locstring = paste(df$Longitude,df$Latitude,sep=",")   #lon + "," + lat
addresses = apply(df['Locstring'], 1, get_address)

df$Address = addresses



"""
locstring = '-123.076807,44.939852'
"""

addresses={"records":[{"attributes":{"OBJECTID":1,"SingleLine":"2110 Eola Dr NW, Salem, OR, 97304"}}]


  reqstring = 'http://locator.stanford.edu/arcgis/rest/services/geocode/Composite_NorthAmerica/GeocodeServer/geocodeAddresses?addresses={"records":[{"attributes":{"OBJECTID":1,"SingleLine":"380 New York St., Redlands, CA"}}]}&token='
  reqstring =  paste(reqstring,token,sep="")
reqstring = paste(reqstring,'&f=pjson',sep="")  

POST(reqstring)
  

adr <- c('450 Serra Mall, Stanford, CA, 94305', '1600 Amphitheatre Pkwy, Mountain View, CA 94043',
           '1355 Market Street Suite 900, San Francisco, CA 94103', stringsAsFactors = F)

source("https://raw.githubusercontent.com/cengel/ArcGIS_geocoding/master/SUL_gcFunctions.R")

do.call("rbind", lapply(adr, function(x) geocodeSLSF(x, myToken)))
        
