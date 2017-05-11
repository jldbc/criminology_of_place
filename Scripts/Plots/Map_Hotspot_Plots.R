#########################################
# Map Plots
#########################################
library(rgdal)
library(data.table)
library(dplyr)
library(ggplot2)
library(geosphere)
library(tidyr)
library(ggmap)
library(MASS)
library(cowplot)

setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
#df = fread("chicago_2001_present.csv", data.table=FALSE)
df = fread('../../chicago_with_shapefile2.csv', data.table=FALSE)
#View(df[,c('Block', 'STREET_NAM')])
orig_df = df
#now get the others, merge those ones on via the st. segment id (since these should all match)
hotspot_years = 2003:2015

#setwd("/Users/jamesledoux/Documents/Research/Thesis")
#df = fread("chi_data_complete.csv", data.table=FALSE)
setwd("/Users/jamesledoux/Documents/Research/Thesis/Data/chicago")
df = fread("chicago_2001_present.csv", data.table=FALSE)
names(df) <- gsub(x = names(df),
                  pattern = " ",
                  replacement = "")

df = df[df$Longitude > -90,] #there are a few impossible values in this data. Need to drop these. 
df = df[!is.na(df$Block),]
#unique id for blocks (temporarily replacement for the joined data, since the join isn't working right yet)
df = transform(df,segment_id=as.numeric(factor(Block)))
#filter out nonviolent crime
violent_crimes = c("BATTERY", "ASSAULT", "ROBBERY", "CRIM SEXUAL ASSAULT", "HOMICIDE",
                   "DOMESTIC VIOLENCE")

df = df[df$PrimaryType %in% violent_crimes,]
backup_df = df

for(hotspot_year in hotspot_years){
  #hotspot identifiers - reset to zero at each iteration
  df = backup_df
  df$is_hotspot_25 = 0 
  df$is_hotspot_50 = 0 
  
  #only look at one year.. hotspots vary too much YoY to see all.
  #df = df[df$Year==2015,]
  frequencies = sort(table(df[df$Year==hotspot_year, 'segment_id']), decreasing=T)
  
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
  
  #create IDs for crimes happening at hotspots (both 25 andf 50% levels)
  n_seg = find_concentration(0.25, df[df$Year==hotspot_year,])
  hotspot_ids_25 = names(frequencies[1:n_seg])
  n_seg = find_concentration(0.5, df[df$Year==hotspot_year,])
  hotspot_ids_50 = names(frequencies[1:n_seg])
  
  #df$count = 1 #we only want to be counting violent crimes
  #backup_df$count = 0 
  #add nonviolent rows back in.. we won't count the crimes but we want to keep those segment IDs
  df = rbind(df, backup_df[!backup_df$PrimaryType %in% violent_crimes,])
  df[df$segment_id %in% hotspot_ids_25, c("is_hotspot_25")] = 1 
  df[df$segment_id %in% hotspot_ids_50, c("is_hotspot_50")] = 1 
  
  #make sure all data has location
  df = df[!is.na(df$Longitude),]
  df = df[!is.na(df$Latitude),]
  
  df = df[, c("segment_id","is_hotspot_25", "is_hotspot_50")]

  new_df = df[!duplicated(df$segment_id),] #note: approx. 1/3 of street segments don't have a community area attached to them 
  #counts_2015_df = df[df$count==1,] %>% count(segment_id, Year) %>% filter(Year==2015) #head(20) #crime count / yr (but only count the violent ones)
  #new_df = merge(x = new_df, y = counts_2015_df, by = "segment_id", all.x = TRUE) #merge counts where we have them
  new_df[is.na(new_df$n), 'n'] = 0
  columns_to_keep = c('segment_id', 'is_hotspot_25', 'is_hotspot_50')
  new_df = new_df[, columns_to_keep]
  orig_df = merge(x=orig_df, y=new_df, by='segment_id', all.x=TRUE) #left join the hotspot dummies by segment id

}

num_columns = length(names(orig_df))
col_names = names(orig_df)
hotspot_variable_names = c('x25_2003', 'x50_2003', 'x25_2004', 'x50_2004', 'x25_2005', 'x50_2005', 'x25_2006', 
                           'x50_2006', 'x25_2007', 'x50_2007', 'x25_2008', 'x50_2008', 'x25_2009', 'x50_2009', 
                           'x25_2010', 'x50_2010', 'x25_2011', 'x50_2011', 'x25_2012', 'x50_2012', 'x25_2013', 
                           'x50_2013', 'x25_2014', 'x50_2014', 'x25_2015', 'x50_2015')
col_names[(num_columns-2*length(hotspot_years) + 1):num_columns] = hotspot_variable_names
names(orig_df) = col_names

#13yrs, 26 new cols
#shapefile
transportation.shapefile <- readOGR(dsn="Transportation.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84")

#convert to standard dataframe
transportation.table = fortify(transportation.shapefile)

#let's see how the data looks
head(transportation.table)

#join tables to have hotspot status on the fortified street network table 
cols_to_merge = c('group', 'x25_2003', 'x50_2003', 'x25_2004', 'x50_2004', 
                  'x25_2005', 'x50_2005', 'x25_2006', 'x50_2006', 'x25_2007', 'x50_2007', 'x25_2008', 'x50_2008', 
                  'x25_2009', 'x50_2009', 'x25_2010', 'x50_2010', 'x25_2011', 'x50_2011', 'x25_2012', 'x50_2012',
                  'x25_2013', 'x50_2013', 'x25_2014', 'x50_2014', 'x25_2015', 'x50_2015')
merge_df = orig_df[,cols_to_merge]
table_with_hotspots = merge(x = transportation.table, y = merge_df, by = "group", all.x = TRUE)


#convert hotspot IDs to factors, convert NAs to 0s
for(col in cols_to_merge[2:length(cols_to_merge)]){
  table_with_hotspots[is.na(table_with_hotspots[,col]), col] = 0
  table_with_hotspots[,col] = as.factor(table_with_hotspots[,col])
}

#palette(c("black","red"))
par(mar = rep(2, 4)) #need this or else the figure is too large to plot
ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2003==0,], aes(x=long, y=lat, group=group, col=x25_2003), size=0.2) +
           geom_path(data=table_with_hotspots[table_with_hotspots$x25_2003==1,], aes(x=long, y=lat, group=group, col=x25_2003), size=0.4) + 
           scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2004==0,], aes(x=long, y=lat, group=group, col=x25_2004), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2004==1,], aes(x=long, y=lat, group=group, col=x25_2004), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2005==0,], aes(x=long, y=lat, group=group, col=x25_2005), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2005==1,], aes(x=long, y=lat, group=group, col=x25_2005), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2006==0,], aes(x=long, y=lat, group=group, col=x25_2006), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2006==1,], aes(x=long, y=lat, group=group, col=x25_2006), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2007==0,], aes(x=long, y=lat, group=group, col=x25_2007), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2007==1,], aes(x=long, y=lat, group=group, col=x25_2007), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2008==0,], aes(x=long, y=lat, group=group, col=x25_2008), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2008==1,], aes(x=long, y=lat, group=group, col=x25_2008), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2009==0,], aes(x=long, y=lat, group=group, col=x25_2009), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2009==1,], aes(x=long, y=lat, group=group, col=x25_2009), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2010==0,], aes(x=long, y=lat, group=group, col=x25_2010), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2010==1,], aes(x=long, y=lat, group=group, col=x25_2010), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2011==0,], aes(x=long, y=lat, group=group, col=x25_2011), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2011==1,], aes(x=long, y=lat, group=group, col=x25_2011), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2012==0,], aes(x=long, y=lat, group=group, col=x25_2012), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2012==1,], aes(x=long, y=lat, group=group, col=x25_2012), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2013==0,], aes(x=long, y=lat, group=group, col=x25_2013), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2013==1,], aes(x=long, y=lat, group=group, col=x25_2013), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2014==0,], aes(x=long, y=lat, group=group, col=x25_2014), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2014==1,], aes(x=long, y=lat, group=group, col=x25_2014), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))

ggplot() + geom_path(data=table_with_hotspots[table_with_hotspots$x25_2015==0,], aes(x=long, y=lat, group=group, col=x25_2015), size=0.2) +
  geom_path(data=table_with_hotspots[table_with_hotspots$x25_2015==1,], aes(x=long, y=lat, group=group, col=x25_2015), size=0.4) + 
  scale_color_manual(values=c("#cccccc", "#e44849"))


