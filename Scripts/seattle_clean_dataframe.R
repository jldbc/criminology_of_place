# Trim DataFrame down to include only the street segments we care about
# Note: keep cleaning here so that it's all in one place. Save final version once 
# I'm confident that it's correct.
# Author: James LeDoux
# Jan. 9, 2017

df = fread("/Users/jamesledoux/Documents/Research/Thesis/Data/Seattle/seattle_with_shapefile2.csv", data.table=FALSE)
df = df[which(df$SND_FEACOD %in% list(1,5)),]
df = df[which(df$SEGMENT_TY %in% list(1,5)),]
df = df[which(df$CITYCODE==1),]
df = df[!grepl('/', df$`Hundred Block Location`),] #get rid of intersections
