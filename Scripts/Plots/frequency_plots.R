library(cowplot)
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data")

################################# 
# Los Angeles
################################# 
#cleaned and merged DataFrame
la = fread("LA/la_12_15.csv", data.table=FALSE)
total_num_reports = nrow(la)
la$Year = as.numeric(substr(la$DATE, nchar(la$DATE.OCC)-3, nchar(la$DATE.OCC)))
#drop NA years
la = la[la$Year != "NA",]
#drop crime at intersections 
la = la[la$Cross.Street=="",]

names(la) <- gsub(x = names(la),
                  pattern = " ",
                  replacement = "")

la$LOCATION <- gsub(x = la$LOCATION,
                    pattern = " ",
                    replacement = "")
#see the names of the worst street segments (mostly curious if I'm still picking up traffic offenses on highways)
sort(table(la$LOCATION), decreasing=T)

#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
la <- transform(la,segment_id=as.numeric(factor(LOCATION)))

freqs_la = la %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_la$ID = seq.int(nrow(freqs_la))
freqs_la$city = 'LA'
################################# 
#SAN FRANCISCO
################################# 
sf = fread("SF/sf.csv", data.table=FALSE)
names(sf) <- gsub(x = names(sf),
                  pattern = " ",
                  replacement = "")

sf = sf[!grepl('/', sf$Address),] #get rid of intersections

sf <- transform(sf,segment_id=as.numeric(factor(Address)))
sf = sf[sf$Address != '800 Block of BRYANT ST',]   #this is the police station. sfpd puts this address when real address not available

freqs_sf = sf %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_sf$ID = seq.int(nrow(freqs_sf))
freqs_sf$city = "SF"
################################# 
# Seattle
################################# 

sea = fread("seattle/seattle.csv", data.table=FALSE)
sea = sea[sea$Year != "NA",]
#drop crime at intersections 
sea = sea[!grepl('/', sea$`Hundred Block Location`),] #get rid of intersections
names(sea) <- gsub(x = names(sea),
                  pattern = " ",
                  replacement = "")

sea <- transform(sea,segment_id=as.numeric(factor(HundredBlockLocation)))

sea = sea[sea$SummarizedOffenseDescription != 'CAR PROWL',]
frequencies = sort(table(sea['segment_id']), decreasing=T) 

freqs_sea = sea %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_sea$ID = seq.int(nrow(freqs_sea))
freqs_sea$city = 'Seattle'
################################# 
# Chicago
################################# 

chi = fread("chicago/chicago_2001_present.csv", data.table=FALSE)

#drop spaces in column names
names(chi) <- gsub(x = names(chi),
                  pattern = " ",
                  replacement = "")

chi <- transform(chi,segment_id=as.numeric(factor(Block)))
chi = chi[!chi$PrimaryType %in% c("OTHER OFFENSE"),]

freqs_chi = chi %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_chi$ID = seq.int(nrow(freqs_chi))
freqs_chi$city = 'Chicago'

################################# 
# PDX
################################# 

pdx = fread("PDX/pdx.csv", data.table=FALSE) #portland_police_bureau is a better dataset but only has 16 - 17
names(pdx) <- gsub(x = names(pdx),
                  pattern = " ",
                  replacement = "")

pdx = pdx[!grepl(' and ', pdx$Address),] #get rid of intersections
pdx <- transform(pdx,segment_id=as.numeric(factor(Address)))
pdx$Year = as.numeric(substr(pdx$ReportDate, nchar(pdx$ReportDate)-1, nchar(pdx$ReportDate)))
pdx = pdx[pdx$Year != 11,]
pdx = pdx[pdx$Year != 13,]

police_stations = c(57009)
pdx = pdx[!(pdx$segment_id %in% police_stations), ]
pdx = pdx[pdx$MajorOffenseType!='Larceny',]
freqs_pdx = pdx %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_pdx$ID = seq.int(nrow(freqs_pdx))
freqs_pdx$city = 'Portland'

################################# 
# Cincinnati
################################# 

cin = fread("cincinnati/cincinnati_crime.csv", data.table=FALSE)
names(cin) <- gsub(x = names(cin),
                  pattern = " ",
                  replacement = "")

cin$Block = with(cin, paste0(BlockBegin, BlockEnd, StreetName))
cin$Year = sub("^[^/]*", "", cin$OccurredOn)
cin$Year = sub("/", "", cin$OccurredOn)
cin$Year = sub("^[^/]*", "", cin$Year)
cin$Year = sub("/", "", cin$Year)
cin$Year = as.numeric(substr(cin$Year, 1, 4))
cin = cin[cin$Year > 2010,]
cin = cin[cin$Year < 2016,]

cin <- transform(cin,segment_id=as.numeric(factor(Block)))

police_stations = c(5144)
cin = cin[!(cin$segment_id %in% police_stations),]

freqs_cin = cin %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))

freqs_cin$ID = seq.int(nrow(freqs_cin))
freqs_cin$city = 'Cincinnati'
################################# 
# Dallas
################################# 

dal = fread("../dallas_crime.csv", data.table=FALSE)

names(dal) <- gsub(x = names(dal),
                  pattern = " ",
                  replacement = "")
dal = dal[!is.na(dal$StreetBlock),]
dal = dal[!is.na(dal$StreetName),]
dal$Block = with(dal, paste0(StreetBlock, StreetName))
dal = transform(dal,segment_id=as.numeric(factor(Block)))

police_stations = c(10444, 65254)
dal = dal[!(dal$segment_id %in% police_stations),]
freqs_dal = dal %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_dal$ID = seq.int(nrow(freqs_dal))
freqs_dal$city = 'Dallas'

################################# 
# Philadelphia
################################# 
phi = fread("Philly/philly_06_present.csv", data.table=FALSE)
#drop spaces in column names
names(phi) <- gsub(x = names(phi),
                  pattern = " ",
                  replacement = "")
phi = phi[!grepl('/', phi$LocationBlock),] #get rid of intersections
phi = phi[grepl('BLOCK', phi$LocationBlock),] #get rid of intersections
phi$Year = as.numeric(substr(phi$DispatchDate, 1,4))
phi = phi[phi$Year != "NA",]
phi = phi[!is.na(phi$PoliceDistricts),]
phi = phi[!is.na(phi$UCRCode),]

phi <- transform(phi,segment_id=as.numeric(factor(LocationBlock)))

freqs_phi = phi %>%
  group_by(segment_id) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))
freqs_phi$ID = seq.int(nrow(freqs_phi))
freqs_phi$city = 'Philadelphia'

################################# 
# Plot all frequency tables side by side to show comparative power-law distributions 
################################# 
#0-1 scale both frequencies and IDs for each dataframe
dfs = c(freqs_cin, freqs_phi)#, freqs_pdx, freqs_la, freqs_dal, freqs_sf, freqs_sea, freqs_chi)

freqs_cin = as.data.frame(freqs_cin)
freqs_cin$n_crimes = (freqs_cin$n_crimes-min(freqs_cin$n_crimes))/(max(freqs_cin$n_crimes)-min(freqs_cin$n_crimes))
freqs_cin$ID = (freqs_cin$ID-min(freqs_cin$ID))/(max(freqs_cin$ID)-min(freqs_cin$ID))

freqs_phi = as.data.frame(freqs_phi)
freqs_phi$n_crimes = (freqs_phi$n_crimes-min(freqs_phi$n_crimes))/(max(freqs_phi$n_crimes)-min(freqs_phi$n_crimes))
freqs_phi$ID = (freqs_phi$ID-min(freqs_phi$ID))/(max(freqs_phi$ID)-min(freqs_phi$ID))

freqs_pdx = as.data.frame(freqs_pdx)
freqs_pdx$n_crimes = (freqs_pdx$n_crimes-min(freqs_pdx$n_crimes))/(max(freqs_pdx$n_crimes)-min(freqs_pdx$n_crimes))
freqs_pdx$ID = (freqs_pdx$ID-min(freqs_pdx$ID))/(max(freqs_pdx$ID)-min(freqs_pdx$ID))

freqs_la = as.data.frame(freqs_la)
freqs_la$n_crimes = (freqs_la$n_crimes-min(freqs_la$n_crimes))/(max(freqs_la$n_crimes)-min(freqs_la$n_crimes))
freqs_la$ID = (freqs_la$ID-min(freqs_la$ID))/(max(freqs_la$ID)-min(freqs_la$ID))

freqs_dal = as.data.frame(freqs_dal)
freqs_dal$n_crimes = (freqs_dal$n_crimes-min(freqs_dal$n_crimes))/(max(freqs_dal$n_crimes)-min(freqs_dal$n_crimes))
freqs_dal$ID = (freqs_dal$ID-min(freqs_dal$ID))/(max(freqs_dal$ID)-min(freqs_dal$ID))

freqs_sea = as.data.frame(freqs_sea)
freqs_sea$n_crimes = (freqs_sea$n_crimes-min(freqs_sea$n_crimes))/(max(freqs_sea$n_crimes)-min(freqs_sea$n_crimes))
freqs_sea$ID = (freqs_sea$ID-min(freqs_sea$ID))/(max(freqs_sea$ID)-min(freqs_sea$ID))

freqs_chi = as.data.frame(freqs_chi)
freqs_chi$n_crimes = (freqs_chi$n_crimes-min(freqs_chi$n_crimes))/(max(freqs_chi$n_crimes)-min(freqs_chi$n_crimes))
freqs_chi$ID = (freqs_chi$ID-min(freqs_chi$ID))/(max(freqs_chi$ID)-min(freqs_chi$ID))

freqs_sf = as.data.frame(freqs_sf)
freqs_sf$n_crimes = (freqs_sf$n_crimes-min(freqs_sf$n_crimes))/(max(freqs_sf$n_crimes)-min(freqs_sf$n_crimes))
freqs_sf$ID = (freqs_sf$ID-min(freqs_sf$ID))/(max(freqs_sf$ID)-min(freqs_sf$ID))


#merge all dataframes into a single df
frequencies = rbind(freqs_chi, freqs_cin, freqs_phi, freqs_pdx)

ggplot(frequencies, aes(ID, n_crimes)) + 
  geom_point() + facet_grid(. ~ city)
  # + background_grid(major = 'y', minor = "none") + # add thin horizontal lines 
  # panel_border()

frequencies = rbind(freqs_la, freqs_dal, freqs_sf, freqs_sea)
ggplot(frequencies, aes(ID, n_crimes)) + 
  geom_point() + facet_grid(. ~ city)



#