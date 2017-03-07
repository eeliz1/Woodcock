library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)

############################################################
#######Takes pinpoint files and generates MCP (.shp) for each
#######day and night of each bandnumber
############################################################

##set working directory to folder with pinpoint files
setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/new")


##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames

for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, skip=4)
  df=x
  ##create POSIX object
  df$BandNmbr = substr(fileName,1, 9)
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
  om <- with(df, (Latitude==0.00000))
  df <-df[!om,]

  ##assign coordinate reference
  coordinates(df)=c("Longitude", "Latitude")
  proj4string(df)<-CRS('+proj=longlat +datum=WGS84')

  dates = unique(substr(df$gmt, 1, 10))
  
################################################################  
  for (date in dates){
    sub1 = df[which(substr(df$gmt, 1, 10)==date),]
    day = sub1[which(substr(sub1$gmt, 12, 19)==format(df$gmt, '%H') %in% c(13:24, '00')),]
    
    
    sub1 = subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
    sub2 = subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
    night = sub1[which(substr(df$gmt, 1, 10)==date),]
################################################################
    
  }
  
  ##subsets 
  s1 <- subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
  ##night
  s2 <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
  
  ##Day points
  
  
  ##Night points
  
  
  
  
  
